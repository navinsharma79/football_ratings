# =============================================================================
# 03_opp_adjustments.R
# Opponent-adjust raw EPA ratings via simultaneous two-way fixed effects
#
# Method: for every play, EPA is modeled as:
#   epa ~ offense_effect[posteam] + defense_effect[defteam] + home_advantage
#
# Solving this via OLS (fixest::feols) gives us both offense and defense
# ratings simultaneously — no iterative adjustment needed.
# A separate model handles special teams (possession + coverage sides).
#
# Inputs:  data/processed/pbp_clean.parquet
#          data/raw/schedules.parquet
# Outputs: data/processed/adj_ratings_season.parquet
#          output/ratings/ratings_{season}.csv  (one per season, human-readable)
# =============================================================================

library(tidyverse)
library(arrow)
library(fixest)
library(cli)
library(glue)

PROJ_ROOT  <- "//wsl.localhost/FedoraLinux-44/home/navin/football_ratings"
setwd(PROJ_ROOT)

PROC_DIR    <- file.path(PROJ_ROOT, "data", "processed")
OUTPUT_DIR  <- file.path(PROJ_ROOT, "output", "ratings")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

SEASONS_ALL <- 2010:2025

# =============================================================================
# Load data
# =============================================================================

cli::cli_h1("Loading data")

pbp <- arrow::read_parquet(file.path(PROC_DIR, "pbp_clean.parquet"))

schedules <- arrow::read_parquet(file.path(PROJ_ROOT, "data", "raw", "schedules.parquet")) |>
  filter(game_type == "REG")

cli::cli_alert_info("Loaded {nrow(pbp)} plays")

# =============================================================================
# Helper: run one season's opponent adjustment
# =============================================================================

#' Run simultaneous two-way fixed effects for one season.
#' Returns a tibble with opponent-adjusted ratings for all three phases.
adjust_season <- function(season_pbp, season_schedules, szn) {

  cli::cli_alert_info("Adjusting season {szn}...")

  # --- Phase filters -------------------------------------------------------

  pbp_off <- season_pbp |>
    filter(epa_valid, !is_garbage, phase %in% c("pass", "rush"))

  pbp_pass <- pbp_off |> filter(phase == "pass")
  pbp_rush <- pbp_off |> filter(phase == "rush")

  pbp_st <- season_pbp |>
    filter(epa_valid, phase == "special_teams")

  # Need at least minimal data to fit models
  if (nrow(pbp_off) < 100) {
    cli::cli_alert_warning("Season {szn}: insufficient offense plays, skipping.")
    return(NULL)
  }

  # --- Home field indicator ------------------------------------------------

  # posteam_type is "home" or "away"; convert to numeric
  pbp_off  <- pbp_off  |> mutate(is_home = as.integer(posteam_type == "home"))
  pbp_pass <- pbp_pass |> mutate(is_home = as.integer(posteam_type == "home"))
  pbp_rush <- pbp_rush |> mutate(is_home = as.integer(posteam_type == "home"))
  pbp_st   <- pbp_st   |> mutate(is_home = as.integer(posteam_type == "home"))

  # --- Model 1: Combined offense + defense (all plays) --------------------
  # epa ~ is_home | posteam + defteam
  # posteam FE = opponent-adjusted offensive contribution
  # defteam FE = opponent-adjusted defensive contribution (flip sign to report)

  mod_all <- tryCatch(
    fixest::feols(epa ~ is_home | posteam + defteam,
                  data    = pbp_off,
                  cluster = ~game_id,
                  warn    = FALSE, notes = FALSE),
    error = function(e) { cli::cli_alert_warning("mod_all failed: {e$message}"); NULL }
  )

  # --- Model 2: Pass only --------------------------------------------------
  mod_pass <- tryCatch(
    fixest::feols(epa ~ is_home | posteam + defteam,
                  data    = pbp_pass,
                  cluster = ~game_id,
                  warn    = FALSE, notes = FALSE),
    error = function(e) NULL
  )

  # --- Model 3: Rush only --------------------------------------------------
  mod_rush <- tryCatch(
    fixest::feols(epa ~ is_home | posteam + defteam,
                  data    = pbp_rush,
                  cluster = ~game_id,
                  warn    = FALSE, notes = FALSE),
    error = function(e) NULL
  )

  # --- Model 4: Special teams ----------------------------------------------
  # ST is trickier: posteam = kicking/punting/returning team,
  # defteam = coverage/blocking team.
  # We model from posteam perspective and separately from defteam perspective.

  mod_st <- if (nrow(pbp_st) >= 20) {
    tryCatch(
      fixest::feols(epa ~ is_home | posteam + defteam,
                    data    = pbp_st,
                    cluster = ~game_id,
                    warn    = FALSE, notes = FALSE),
      error = function(e) NULL
    )
  } else NULL

  # --- Extract fixed effects -----------------------------------------------

  #' Pull and center a named FE vector → tibble
  fe_tibble <- function(mod, fe_name, col_name) {
    if (is.null(mod)) return(tibble(team = character(), !!col_name := numeric()))
    fe <- fixest::fixef(mod, notes = FALSE)[[fe_name]]
    tibble(team = names(fe), !!col_name := fe - mean(fe, na.rm = TRUE))
  }

  # Offense FEs (posteam)
  off_all  <- fe_tibble(mod_all,  "posteam", "off_adj_epa")
  off_pass <- fe_tibble(mod_pass, "posteam", "off_adj_pass")
  off_rush <- fe_tibble(mod_rush, "posteam", "off_adj_rush")

  # Defense FEs (defteam) — flip sign: higher = better defense
  def_all  <- fe_tibble(mod_all,  "defteam", "def_adj_epa_raw")  |>
    mutate(def_adj_epa  = -def_adj_epa_raw)  |> select(-def_adj_epa_raw)
  def_pass <- fe_tibble(mod_pass, "defteam", "def_adj_pass_raw") |>
    mutate(def_adj_pass = -def_adj_pass_raw) |> select(-def_adj_pass_raw)
  def_rush <- fe_tibble(mod_rush, "defteam", "def_adj_rush_raw") |>
    mutate(def_adj_rush = -def_adj_rush_raw) |> select(-def_adj_rush_raw)

  # ST: posteam FE = ST possession quality; defteam FE (flipped) = ST coverage quality
  st_poss     <- fe_tibble(mod_st, "posteam", "st_adj_poss")
  st_coverage <- fe_tibble(mod_st, "defteam", "st_adj_cov_raw") |>
    mutate(st_adj_cov = -st_adj_cov_raw) |> select(-st_adj_cov_raw)

  # --- Home field advantage estimate ---------------------------------------
  hfa <- if (!is.null(mod_all)) coef(mod_all)["is_home"] else NA_real_

  # --- Join all FEs into one team tibble -----------------------------------
  teams <- season_pbp |>
    filter(!is.na(posteam)) |>
    distinct(team = posteam) |>
    arrange(team)

  ratings <- teams |>
    left_join(off_all,      by = "team") |>
    left_join(off_pass,     by = "team") |>
    left_join(off_rush,     by = "team") |>
    left_join(def_all,      by = "team") |>
    left_join(def_pass,     by = "team") |>
    left_join(def_rush,     by = "team") |>
    left_join(st_poss,      by = "team") |>
    left_join(st_coverage,  by = "team") |>
    mutate(
      season = szn,
      hfa    = hfa,

      # Combined ST: average possession + coverage (simple equal weight for now)
      st_adj_epa = rowMeans(cbind(
        coalesce(st_adj_poss, 0),
        coalesce(st_adj_cov,  0)
      ), na.rm = TRUE),

      # Total rating: offense + defense + ST
      # ST weighted at 0.5x — placeholder, optimized in predictive model
      total_adj_epa = off_adj_epa + def_adj_epa + (0.5 * st_adj_epa)
    ) |>
    select(season, team, hfa,
           off_adj_epa, off_adj_pass, off_adj_rush,
           def_adj_epa, def_adj_pass, def_adj_rush,
           st_adj_poss, st_adj_cov, st_adj_epa,
           total_adj_epa)

  ratings
}

# =============================================================================
# Run adjustment for every season
# =============================================================================

cli::cli_h1("Running opponent adjustments by season")

seasons_available <- pbp |> distinct(season) |> pull(season) |> sort()
seasons_to_run    <- intersect(SEASONS_ALL, seasons_available)

all_ratings <- map(seasons_to_run, function(szn) {
  adjust_season(
    season_pbp       = pbp        |> filter(season == szn),
    season_schedules = schedules  |> filter(season == szn),
    szn              = szn
  )
}) |>
  compact() |>
  bind_rows()

cli::cli_alert_info("Opponent-adjusted ratings: {nrow(all_ratings)} team-seasons")

# =============================================================================
# Add win % for validation
# =============================================================================

cli::cli_h1("Joining win % for validation")

win_pct <- schedules |>
  filter(game_type == "REG", !is.na(result)) |>
  select(season, home_team, away_team, result) |>
  pivot_longer(
    cols      = c(home_team, away_team),
    names_to  = "location",
    values_to = "team"
  ) |>
  mutate(
    win = case_when(
      location == "home_team" & result > 0  ~ 1,
      location == "away_team" & result < 0  ~ 1,
      result == 0                            ~ 0.5,
      TRUE                                   ~ 0
    )
  ) |>
  group_by(season, team) |>
  summarise(
    wins   = sum(win),
    games  = n(),
    win_pct = wins / games,
    .groups = "drop"
  )

all_ratings <- all_ratings |>
  left_join(win_pct, by = c("season", "team"))

# =============================================================================
# Validation: correlation of adjusted ratings with win %
# =============================================================================

cli::cli_h1("Validation: correlation with win %")

cor_results <- all_ratings |>
  filter(!is.na(win_pct)) |>
  summarise(
    r_total   = cor(total_adj_epa, win_pct,   use = "complete.obs") |> round(3),
    r_offense = cor(off_adj_epa,   win_pct,   use = "complete.obs") |> round(3),
    r_defense = cor(def_adj_epa,   win_pct,   use = "complete.obs") |> round(3),
    r_st      = cor(st_adj_epa,    win_pct,   use = "complete.obs") |> round(3),
    r_pass    = cor(off_adj_pass,  win_pct,   use = "complete.obs") |> round(3),
    r_rush    = cor(off_adj_rush,  win_pct,   use = "complete.obs") |> round(3)
  )

cli::cli_alert_info("Correlation with same-season win %:")
print(cor_results)

# Target benchmarks (DVOA-level performance):
#   r_total   ~ 0.85–0.90
#   r_offense ~ 0.70–0.75
#   r_defense ~ 0.65–0.70
#   r_st      ~ 0.20–0.35  (ST is noisy)
#   r_pass    ~ 0.70+       (pass EPA is high signal)
#   r_rush    ~ 0.35–0.45   (rush EPA is noisier)

# =============================================================================
# Top/bottom teams sanity check
# =============================================================================

cli::cli_h2("Top 10 team-seasons by adjusted total EPA")
all_ratings |>
  slice_max(total_adj_epa, n = 10) |>
  select(season, team, off_adj_epa, def_adj_epa, st_adj_epa, total_adj_epa, win_pct) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print()

cli::cli_h2("Bottom 10 team-seasons by adjusted total EPA")
all_ratings |>
  slice_min(total_adj_epa, n = 10) |>
  select(season, team, off_adj_epa, def_adj_epa, st_adj_epa, total_adj_epa, win_pct) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print()

cli::cli_h2("Home field advantage by season")
all_ratings |>
  distinct(season, hfa) |>
  arrange(season) |>
  mutate(hfa = round(hfa, 4)) |>
  print(n = Inf)

# =============================================================================
# Write outputs
# =============================================================================

cli::cli_h1("Writing outputs")

arrow::write_parquet(
  all_ratings,
  file.path(PROC_DIR, "adj_ratings_season.parquet")
)
cli::cli_alert_success("Wrote adj_ratings_season.parquet")

# Human-readable CSV per season
walk(seasons_to_run, function(szn) {
  out_path <- file.path(OUTPUT_DIR, glue("ratings_{szn}.csv"))
  all_ratings |>
    filter(season == szn) |>
    arrange(desc(total_adj_epa)) |>
    mutate(
      rank = row_number(),
      across(where(is.numeric), \(x) round(x, 3))
    ) |>
    select(rank, season, team, win_pct,
           off_adj_epa, off_adj_pass, off_adj_rush,
           def_adj_epa, def_adj_pass, def_adj_rush,
           st_adj_poss, st_adj_cov, st_adj_epa,
           total_adj_epa) |>
    readr::write_csv(out_path)
})
cli::cli_alert_success("Wrote per-season CSVs to {OUTPUT_DIR}")

cli::cli_alert_success("Opponent adjustment complete.")
