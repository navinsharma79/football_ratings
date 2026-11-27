# =============================================================================
# 05_composite.R
# Build the final descriptive and predictive composite ratings.
#
# Descriptive:  off + def + 0.5*ST  (opponent-adjusted EPA, equal-ish weights)
# Predictive:   optimized weights + regression-to-mean shrinkage
#
# Design principles (from NFL analytics literature + user insight):
#   - Pass offense is the single most stable signal year-to-year (QB continuity)
#   - Rush offense is moderately stable
#   - Defense is less stable year-to-year — it's often dictated by exploiting
#     the opponent's worst matchup, which changes personnel each season
#   - ST is the least stable — high variance, small sample, scheme-dependent
#
# Method:
#   1. Measure year-to-year persistence of each component (autocorrelation)
#   2. Grid search over (w_pass, w_rush, w_def, w_st) to maximize correlation
#      with NEXT-season win% on training data (2010–2020)
#   3. Apply regression-to-mean shrinkage calibrated to each component's reliability
#   4. Validate on held-out seasons (2021–2025)
#
# Inputs:  data/processed/adj_ratings_season_v2.parquet
# Outputs: data/processed/final_ratings.parquet
#          output/ratings/final_ratings_all_seasons.csv
# =============================================================================

library(tidyverse)
library(arrow)
library(cli)
library(glue)

PROJ_ROOT  <- "//wsl.localhost/FedoraLinux-44/home/navin/football_ratings"
setwd(PROJ_ROOT)
PROC_DIR   <- file.path(PROJ_ROOT, "data", "processed")
OUTPUT_DIR <- file.path(PROJ_ROOT, "output", "ratings")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

TRAIN_SEASONS <- 2010:2020
VAL_SEASONS   <- 2021:2025

# =============================================================================
# Load data
# =============================================================================

cli::cli_h1("Loading data")

ratings_raw <- arrow::read_parquet(file.path(PROC_DIR, "adj_ratings_season_v2.parquet"))

# Join play counts from raw aggregations (needed for shrinkage denominators)
play_counts <- arrow::read_parquet(file.path(PROC_DIR, "raw_team_epa_season.parquet")) |>
  select(season, team, off_n_plays, def_n_plays, st_n_plays, n_games)

ratings <- ratings_raw |>
  left_join(play_counts, by = c("season", "team"))

cli::cli_alert_info(
  "Loaded {nrow(ratings)} team-seasons ({min(ratings$season)}–{max(ratings$season)})"
)

# =============================================================================
# Step 1: Year-to-year persistence by component
# High persistence = trust the signal more in predictive model
# =============================================================================

cli::cli_h1("Step 1: Year-to-year persistence by component")

ytoy <- ratings |>
  arrange(team, season) |>
  group_by(team) |>
  mutate(
    next_win_pct  = lead(win_pct),
    next_off_pass = lead(off_adj_pass),
    next_off_rush = lead(off_adj_rush),
    next_def_epa  = lead(def_adj_epa),
    next_st_epa   = lead(st_adj_epa)
  ) |>
  ungroup() |>
  filter(!is.na(next_win_pct))

ytoy_summary <- ytoy |>
  summarise(
    # Same-season r with win% (descriptive power)
    same_r_pass  = cor(off_adj_pass,  win_pct,       use = "complete.obs"),
    same_r_rush  = cor(off_adj_rush,  win_pct,       use = "complete.obs"),
    same_r_def   = cor(def_adj_epa,   win_pct,       use = "complete.obs"),
    same_r_st    = cor(st_adj_epa,    win_pct,       use = "complete.obs"),
    # Next-season r with win% (predictive power)
    next_r_pass  = cor(off_adj_pass,  next_win_pct,  use = "complete.obs"),
    next_r_rush  = cor(off_adj_rush,  next_win_pct,  use = "complete.obs"),
    next_r_def   = cor(def_adj_epa,   next_win_pct,  use = "complete.obs"),
    next_r_st    = cor(st_adj_epa,    next_win_pct,  use = "complete.obs"),
    # Year-to-year autocorrelation (does the metric itself repeat?)
    persist_pass = cor(off_adj_pass,  next_off_pass, use = "complete.obs"),
    persist_rush = cor(off_adj_rush,  next_off_rush, use = "complete.obs"),
    persist_def  = cor(def_adj_epa,   next_def_epa,  use = "complete.obs"),
    persist_st   = cor(st_adj_epa,    next_st_epa,   use = "complete.obs")
  ) |>
  mutate(across(everything(), \(x) round(x, 3)))

cli::cli_h2("Component correlations")
tibble(
  component    = c("pass_off", "rush_off", "defense", "spec_teams"),
  same_season  = c(ytoy_summary$same_r_pass, ytoy_summary$same_r_rush,
                   ytoy_summary$same_r_def,  ytoy_summary$same_r_st),
  next_season  = c(ytoy_summary$next_r_pass, ytoy_summary$next_r_rush,
                   ytoy_summary$next_r_def,  ytoy_summary$next_r_st),
  persistence  = c(ytoy_summary$persist_pass, ytoy_summary$persist_rush,
                   ytoy_summary$persist_def,  ytoy_summary$persist_st)
) |>
  mutate(
    signal_decay     = round(same_season - next_season, 3),
    signal_retention = round(next_season / same_season, 3)
  ) |>
  arrange(desc(next_season)) |>
  print()

# =============================================================================
# Step 2: Calibrate shrinkage constants (k) from persistence
#
# Bayesian shrinkage: rating_shrunk = rating_raw * shrink
#   where shrink = n_plays / (n_plays + k)
#
# k is chosen so that after a full season (~500 pass plays, ~300 rush plays, etc.)
# the shrinkage matches the observed year-to-year signal retention.
# Higher persistence → lower k (trust signal sooner).
# Lower persistence  → higher k (need more data before trusting).
# =============================================================================

cli::cli_h1("Step 2: Calibrating shrinkage constants")

# Full-season play counts (approximate medians across all teams/seasons)
median_counts <- ratings |>
  summarise(
    med_off  = median(off_n_plays, na.rm = TRUE),
    med_def  = median(def_n_plays, na.rm = TRUE),
    med_st   = median(st_n_plays,  na.rm = TRUE)
  )
cli::cli_alert_info("Median play counts — Off: {round(median_counts$med_off)}, Def: {round(median_counts$med_def)}, ST: {round(median_counts$med_st)}")

# k calibration: solve  persist = n / (n + k)  →  k = n * (1 - persist) / persist
# Use persistence values from ytoy_summary
k_from_persist <- function(n, persist) {
  if (is.na(persist) || persist <= 0) return(Inf)
  round(n * (1 - persist) / persist)
}

K_PASS <- k_from_persist(median_counts$med_off, ytoy_summary$persist_pass[[1]])
K_RUSH <- k_from_persist(median_counts$med_off, ytoy_summary$persist_rush[[1]])
K_DEF  <- k_from_persist(median_counts$med_def, ytoy_summary$persist_def[[1]])
K_ST   <- k_from_persist(median_counts$med_st,  ytoy_summary$persist_st[[1]])

cli::cli_alert_info("Shrinkage k values — Pass: {K_PASS}, Rush: {K_RUSH}, Def: {K_DEF}, ST: {K_ST}")
cli::cli_alert_info("(Lower k = signal trusted sooner; Higher k = more shrinkage to mean)")

# =============================================================================
# Step 3: Apply shrinkage to all seasons
# =============================================================================

cli::cli_h1("Step 3: Applying regression-to-mean shrinkage")

ratings_shrunk <- ratings |>
  mutate(
    # Shrinkage factors: how much of the full-season signal to trust
    shrink_pass = off_n_plays / (off_n_plays + K_PASS),
    shrink_rush = off_n_plays / (off_n_plays + K_RUSH),
    shrink_def  = def_n_plays / (def_n_plays + K_DEF),
    shrink_st   = coalesce(st_n_plays, 0L) / (coalesce(st_n_plays, 0L) + K_ST),

    # Shrunk ratings: pulled toward 0 (league average) for small samples
    off_pass_shrunk = off_adj_pass * shrink_pass,
    off_rush_shrunk = off_adj_rush * shrink_rush,
    def_shrunk      = def_adj_epa  * shrink_def,
    st_shrunk       = coalesce(st_adj_epa, 0) * shrink_st
  )

# Sanity: shrinkage factors for a full 17-game season
cli::cli_h2("Full-season shrinkage factors (17 games)")
ratings_shrunk |>
  filter(n_games >= 16) |>
  summarise(
    mean_shrink_pass = round(mean(shrink_pass, na.rm = TRUE), 3),
    mean_shrink_rush = round(mean(shrink_rush, na.rm = TRUE), 3),
    mean_shrink_def  = round(mean(shrink_def,  na.rm = TRUE), 3),
    mean_shrink_st   = round(mean(shrink_st,   na.rm = TRUE), 3)
  ) |>
  print()

# =============================================================================
# Step 4: Grid search for optimal predictive weights (training set only)
# =============================================================================

cli::cli_h1("Step 4: Grid search for predictive weights")

train_pairs <- ratings_shrunk |>
  filter(season %in% TRAIN_SEASONS) |>
  arrange(team, season) |>
  group_by(team) |>
  mutate(next_win_pct = lead(win_pct)) |>
  ungroup() |>
  filter(!is.na(next_win_pct), !is.na(off_pass_shrunk))

# Grid: relative weights only (absolute scale irrelevant for correlation)
# Ranges informed by persistence results above
grid <- expand.grid(
  w_pass = seq(0.30, 0.60, by = 0.05),
  w_rush = seq(0.05, 0.25, by = 0.05),
  w_def  = seq(0.10, 0.40, by = 0.05),
  w_st   = seq(0.00, 0.15, by = 0.05)
)

cli::cli_alert_info("Searching {nrow(grid)} weight combinations...")

grid$r_train <- map_dbl(seq_len(nrow(grid)), function(i) {
  w <- grid[i, ]
  score <- with(train_pairs,
    w$w_pass * off_pass_shrunk +
    w$w_rush * off_rush_shrunk +
    w$w_def  * def_shrunk      +
    w$w_st   * st_shrunk
  )
  cor(score, train_pairs$next_win_pct, use = "complete.obs")
})

best_weights <- grid |>
  slice_max(r_train, n = 1)

cli::cli_h2("Best weights (training set)")
print(best_weights)

cli::cli_h2("Top 15 weight combinations")
grid |>
  slice_max(r_train, n = 15) |>
  mutate(r_train = round(r_train, 4)) |>
  print()

# =============================================================================
# Step 5: Build predictive composite with best weights
# =============================================================================

cli::cli_h1("Step 5: Building composite ratings")

W_PASS <- best_weights$w_pass
W_RUSH <- best_weights$w_rush
W_DEF  <- best_weights$w_def
W_ST   <- best_weights$w_st

cli::cli_alert_info(
  "Using weights — pass: {W_PASS}, rush: {W_RUSH}, def: {W_DEF}, st: {W_ST}"
)

final_ratings <- ratings_shrunk |>
  mutate(
    # Descriptive: opponent-adjusted EPA, no shrinkage
    # (best for explaining current season results)
    total_descriptive = off_adj_epa + def_adj_epa + 0.5 * coalesce(st_adj_epa, 0),

    # Predictive: optimized weights + shrinkage
    # (best for forecasting next season)
    total_predictive = W_PASS * off_pass_shrunk +
                       W_RUSH * off_rush_shrunk +
                       W_DEF  * def_shrunk      +
                       W_ST   * st_shrunk
  ) |>
  group_by(season) |>
  mutate(
    rank_descriptive = rank(-total_descriptive, ties.method = "min"),
    rank_predictive  = rank(-total_predictive,  ties.method = "min")
  ) |>
  ungroup() |>
  arrange(season, rank_descriptive)

# =============================================================================
# Step 6: Validation — predictive vs descriptive on held-out seasons
# =============================================================================

cli::cli_h1("Step 6: Validation on held-out seasons (2021–2025)")

val_pairs <- final_ratings |>
  filter(season %in% VAL_SEASONS) |>
  arrange(team, season) |>
  group_by(team) |>
  mutate(next_win_pct = lead(win_pct)) |>
  ungroup() |>
  filter(!is.na(next_win_pct))

val_cors <- val_pairs |>
  summarise(
    r_descriptive     = cor(total_descriptive, next_win_pct, use = "complete.obs"),
    r_predictive      = cor(total_predictive,  next_win_pct, use = "complete.obs"),
    r_pass_only       = cor(off_pass_shrunk,   next_win_pct, use = "complete.obs"),
    r_pass_no_shrink  = cor(off_adj_pass,       next_win_pct, use = "complete.obs"),
    r_def_only        = cor(def_shrunk,         next_win_pct, use = "complete.obs"),
    r_st_only         = cor(st_shrunk,          next_win_pct, use = "complete.obs")
  ) |>
  mutate(across(everything(), \(x) round(x, 3)))

cli::cli_alert_info("Validation correlations with next-season win%:")
print(val_cors)

cli::cli_h2("Predictive improvement over descriptive")
cli::cli_alert_info(
  "Predictive r={val_cors$r_predictive} vs Descriptive r={val_cors$r_descriptive} (+{round(val_cors$r_predictive - val_cors$r_descriptive, 3)})"
)
cli::cli_alert_info(
  "Shrinkage benefit on pass: shrunk r={val_cors$r_pass_only} vs unshrunk r={val_cors$r_pass_no_shrink}"
)

# =============================================================================
# Step 7: Display final ratings
# =============================================================================

cli::cli_h1("Step 7: Final ratings tables")

display_cols <- c(
  "rank_descriptive", "team", "win_pct",
  "off_adj_pass", "off_adj_rush", "def_adj_epa", "st_adj_epa",
  "total_descriptive", "total_predictive", "rank_predictive"
)

cli::cli_h2("2024 final ratings")
final_ratings |>
  filter(season == 2024) |>
  select(all_of(display_cols)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = 32)

cli::cli_h2("2025 final ratings (season in progress)")
final_ratings |>
  filter(season == 2025) |>
  select(all_of(display_cols)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = 32)

cli::cli_h2("All-time top 20 (descriptive)")
final_ratings |>
  slice_max(total_descriptive, n = 20) |>
  select(season, team, win_pct, off_adj_pass, def_adj_epa,
         st_adj_epa, total_descriptive) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = 20)

# =============================================================================
# Write outputs
# =============================================================================

cli::cli_h1("Writing outputs")

arrow::write_parquet(
  final_ratings,
  file.path(PROC_DIR, "final_ratings.parquet")
)
cli::cli_alert_success("Wrote final_ratings.parquet")

final_ratings |>
  mutate(across(where(is.numeric), \(x) round(x, 4))) |>
  readr::write_csv(file.path(OUTPUT_DIR, "final_ratings_all_seasons.csv"))
cli::cli_alert_success("Wrote final_ratings_all_seasons.csv")

# Per-season CSVs for website
walk(unique(final_ratings$season), function(szn) {
  final_ratings |>
    filter(season == szn) |>
    arrange(rank_descriptive) |>
    mutate(across(where(is.numeric), \(x) round(x, 4))) |>
    readr::write_csv(file.path(OUTPUT_DIR, glue("ratings_{szn}.csv")))
})
cli::cli_alert_success("Wrote per-season CSVs")

cli::cli_alert_success("Composite model complete.")
