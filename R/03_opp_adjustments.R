# =============================================================================
# 03_opp_adjustments.R
# Opponent-adjust raw EPA ratings via simultaneous two-way fixed effects
#
# Method: for every play, EPA is modeled as:
#   epa ~ offense_effect[posteam] + defense_effect[defteam] + home_advantage
#
# Solving this via OLS (fixest::feols) gives us both offense and defense
# ratings simultaneously — no iterative adjustment needed.
#
# Key design choices vs. earlier version:
#   1. Phase-specific totals: off_adj_epa = offense's actual pass/rush-weighted
#      combination of off_adj_pass + off_adj_rush. def_adj_epa = league-average
#      weighted combination of def_adj_pass + def_adj_rush. This removes the
#      "pass funnel / run funnel" bias where bad pass defenses face more passes
#      (inflating their total EPA allowed) and skewing the combined defensive FE.
#
#   2. Ridge shrinkage on FEs: each team's fixed effect is shrunk toward zero
#      by n / (n + k), where n is their play count and k is set so a full
#      17-game season retains ~80–85% of the raw FE. Teams with small samples
#      (early season, partial years) are pulled harder toward league average.
#      This is applied to the DESCRIPTIVE rating — not just the predictive one.
#
#   3. mod_all (pooled pass+rush) is retained only for the home-field advantage
#      coefficient. The team-level ratings come entirely from phase-specific models.
#
#   4. Validation compares new vs old approach so we can confirm the changes help.
#
# Inputs:  data/processed/pbp_clean.parquet
#          data/raw/schedules.parquet
# Outputs: data/processed/adj_ratings_season.parquet
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

INCLUDE_PLAYOFFS <- Sys.getenv("INCLUDE_PLAYOFFS", unset = "FALSE") == "TRUE"
VALID_GAME_TYPES <- if (INCLUDE_PLAYOFFS) c("REG","WC","DIV","CON","SB") else "REG"

# =============================================================================
# Ridge shrinkage constants
# k = half-shrinkage point (plays at which shrinkage factor = 0.5)
# Full-season retention at typical play counts:
#   Pass offense:  ~550 plays → 550/(550+100) ≈ 85%
#   Rush offense:  ~270 plays → 270/(270+70)  ≈ 79%
#   ST:            ~130 plays → 130/(130+30)  ≈ 81%
# =============================================================================

K_RIDGE_PASS <- 100L
K_RIDGE_RUSH <-  70L
K_RIDGE_ST   <-  30L

# =============================================================================
# Load data
# =============================================================================

cli::cli_h1("Loading data")
cli::cli_alert_info("INCLUDE_PLAYOFFS = {INCLUDE_PLAYOFFS}")

pbp <- arrow::read_parquet(file.path(PROC_DIR, "pbp_clean.parquet")) |>
  filter(game_type %in% VALID_GAME_TYPES)

# Win % always uses regular season for validation consistency
schedules <- arrow::read_parquet(file.path(PROJ_ROOT, "data", "raw", "schedules.parquet")) |>
  filter(game_type == "REG")

cli::cli_alert_info("Loaded {nrow(pbp)} plays ({if (INCLUDE_PLAYOFFS) 'REG + playoffs' else 'REG only'})")

# =============================================================================
# Helpers
# =============================================================================

#' Extract a named fixed-effect vector from a feols model, center at 0
fe_tibble <- function(mod, fe_name, col_name) {
  if (is.null(mod)) return(tibble(team = character(), !!col_name := numeric()))
  fe <- fixest::fixef(mod, notes = FALSE)[[fe_name]]
  tibble(team = names(fe), !!col_name := fe - mean(fe, na.rm = TRUE))
}

#' Apply ridge shrinkage: FE * n / (n + k), shrinking toward 0
#' n_col: name of the play-count column in n_df
apply_ridge <- function(fe_df, col, n_df, n_col, k) {
  fe_df |>
    left_join(n_df |> select(team, .n = all_of(n_col)), by = "team") |>
    mutate(!!sym(col) := !!sym(col) * coalesce(.n, 0L) / (coalesce(.n, 0L) + k)) |>
    select(-.n)
}

# =============================================================================
# Core: run one season's opponent adjustment
# =============================================================================

adjust_season <- function(season_pbp, season_schedules, szn) {

  cli::cli_alert_info("Adjusting season {szn}...")

  # --- Phase filters ---------------------------------------------------------

  pbp_off  <- season_pbp |> filter(epa_valid, !is_garbage, phase %in% c("pass", "rush"))
  pbp_pass <- pbp_off    |> filter(phase == "pass")
  pbp_rush <- pbp_off    |> filter(phase == "rush")
  pbp_st   <- season_pbp |> filter(epa_valid, phase == "special_teams")

  if (nrow(pbp_off) < 100) {
    cli::cli_alert_warning("Season {szn}: insufficient offense plays, skipping.")
    return(NULL)
  }

  # --- Home field indicator --------------------------------------------------

  pbp_off  <- pbp_off  |> mutate(is_home = as.integer(posteam_type == "home"))
  pbp_pass <- pbp_pass |> mutate(is_home = as.integer(posteam_type == "home"))
  pbp_rush <- pbp_rush |> mutate(is_home = as.integer(posteam_type == "home"))
  pbp_st   <- pbp_st   |> mutate(is_home = as.integer(posteam_type == "home"))

  # --- Play counts (needed for ridge shrinkage) ------------------------------

  off_counts <- pbp_off |>
    group_by(team = posteam) |>
    summarise(
      n_pass  = sum(phase == "pass"),
      n_rush  = sum(phase == "rush"),
      n_off   = n(),
      w_pass  = n_pass / n_off,     # this offense's actual pass fraction
      w_rush  = n_rush / n_off,
      .groups = "drop"
    )

  def_counts <- pbp_off |>
    group_by(team = defteam) |>
    summarise(
      n_pass_def = sum(phase == "pass"),
      n_rush_def = sum(phase == "rush"),
      .groups = "drop"
    )

  st_off_counts <- pbp_st |>
    group_by(team = posteam) |>
    summarise(n_st_off = n(), .groups = "drop")

  st_def_counts <- pbp_st |>
    filter(play_type %in% c("kickoff", "punt")) |>
    group_by(team = defteam) |>
    summarise(n_st_def = n(), .groups = "drop")

  # League-average pass/rush split (used to weight defense totals)
  # Removes pass-funnel bias: def_adj_epa is "how good vs a typical game plan"
  league_w_pass <- nrow(pbp_pass) / nrow(pbp_off)
  league_w_rush <- 1 - league_w_pass

  # --- Fit models ------------------------------------------------------------

  # mod_all: used only for home-field advantage coefficient
  mod_all <- tryCatch(
    fixest::feols(epa ~ is_home | posteam + defteam,
                  data = pbp_off, cluster = ~game_id,
                  warn = FALSE, notes = FALSE),
    error = function(e) { cli::cli_alert_warning("mod_all failed: {e$message}"); NULL }
  )

  # Phase-specific models: team ratings come from these
  mod_pass <- tryCatch(
    fixest::feols(epa ~ is_home | posteam + defteam,
                  data = pbp_pass, cluster = ~game_id,
                  warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )

  mod_rush <- tryCatch(
    fixest::feols(epa ~ is_home | posteam + defteam,
                  data = pbp_rush, cluster = ~game_id,
                  warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )

  mod_st <- if (nrow(pbp_st) >= 20) {
    tryCatch(
      fixest::feols(epa ~ is_home | posteam + defteam,
                    data = pbp_st, cluster = ~game_id,
                    warn = FALSE, notes = FALSE),
      error = function(e) NULL
    )
  } else NULL

  # --- Extract FEs with ridge shrinkage --------------------------------------

  # Offense pass (shrink by pass plays this offense ran)
  off_pass_fe <- fe_tibble(mod_pass, "posteam", "off_adj_pass") |>
    apply_ridge("off_adj_pass", off_counts, "n_pass", K_RIDGE_PASS)

  # Offense rush
  off_rush_fe <- fe_tibble(mod_rush, "posteam", "off_adj_rush") |>
    apply_ridge("off_adj_rush", off_counts, "n_rush", K_RIDGE_RUSH)

  # Defense pass (sign-flip: higher = better defense)
  def_pass_fe <- fe_tibble(mod_pass, "defteam", "def_adj_pass_raw") |>
    apply_ridge("def_adj_pass_raw", def_counts, "n_pass_def", K_RIDGE_PASS) |>
    mutate(def_adj_pass = -def_adj_pass_raw) |>
    select(-def_adj_pass_raw)

  # Defense rush
  def_rush_fe <- fe_tibble(mod_rush, "defteam", "def_adj_rush_raw") |>
    apply_ridge("def_adj_rush_raw", def_counts, "n_rush_def", K_RIDGE_RUSH) |>
    mutate(def_adj_rush = -def_adj_rush_raw) |>
    select(-def_adj_rush_raw)

  # ST possession + coverage
  st_poss <- fe_tibble(mod_st, "posteam", "st_adj_poss") |>
    apply_ridge("st_adj_poss", st_off_counts, "n_st_off", K_RIDGE_ST)

  st_cov <- fe_tibble(mod_st, "defteam", "st_adj_cov_raw") |>
    apply_ridge("st_adj_cov_raw", st_def_counts, "n_st_def", K_RIDGE_ST) |>
    mutate(st_adj_cov = -st_adj_cov_raw) |>
    select(-st_adj_cov_raw)

  # --- OLD approach FEs (from mod_all, no shrinkage) for comparison ----------

  off_all_fe <- fe_tibble(mod_all, "posteam", "off_adj_epa_pooled")
  def_all_fe <- fe_tibble(mod_all, "defteam", "def_adj_epa_pooled_raw") |>
    mutate(def_adj_epa_pooled = -def_adj_epa_pooled_raw) |>
    select(-def_adj_epa_pooled_raw)

  # Home field advantage from mod_all
  hfa <- if (!is.null(mod_all)) coef(mod_all)["is_home"] else NA_real_

  # --- Join all FEs ----------------------------------------------------------

  teams <- season_pbp |>
    filter(!is.na(posteam)) |>
    distinct(team = posteam) |>
    arrange(team)

  ratings <- teams |>
    left_join(off_counts |> select(team, w_pass, w_rush), by = "team") |>
    left_join(off_pass_fe,  by = "team") |>
    left_join(off_rush_fe,  by = "team") |>
    left_join(def_pass_fe,  by = "team") |>
    left_join(def_rush_fe,  by = "team") |>
    left_join(st_poss,      by = "team") |>
    left_join(st_cov,       by = "team") |>
    left_join(off_all_fe,   by = "team") |>
    left_join(def_all_fe,   by = "team") |>
    mutate(
      season = szn,
      hfa    = hfa,

      # --- New approach -------------------------------------------------------
      # Offense total: weighted by this team's actual pass/rush split.
      # An offense that passes 65% of the time gets 65% weight on their
      # pass rating, correctly reflecting their game plan.
      off_adj_epa = coalesce(w_pass, league_w_pass) * coalesce(off_adj_pass, 0) +
                   coalesce(w_rush, league_w_rush) * coalesce(off_adj_rush, 0),

      # Defense total: weighted by league-average pass/rush split.
      # Removes pass-funnel/run-funnel bias — evaluates every defense against
      # the same "standard" offensive game plan.
      def_adj_epa = league_w_pass * coalesce(def_adj_pass, 0) +
                   league_w_rush  * coalesce(def_adj_rush, 0),

      # ST: average of possession and coverage sides
      st_adj_epa = rowMeans(
        cbind(coalesce(st_adj_poss, 0), coalesce(st_adj_cov, 0)),
        na.rm = TRUE
      ),

      total_adj_epa = off_adj_epa + def_adj_epa + (0.5 * st_adj_epa),

      # --- Old approach (pooled, no shrinkage) for comparison -----------------
      total_adj_epa_pooled = coalesce(off_adj_epa_pooled, 0) +
                             coalesce(def_adj_epa_pooled, 0) +
                             (0.5 * coalesce(st_adj_epa, 0))
    ) |>
    select(
      season, team, hfa,
      # New phase-specific, ridge-shrunk ratings
      off_adj_epa, off_adj_pass, off_adj_rush,
      def_adj_epa, def_adj_pass, def_adj_rush,
      st_adj_poss, st_adj_cov, st_adj_epa,
      total_adj_epa,
      # Old pooled ratings for comparison
      off_adj_epa_pooled, def_adj_epa_pooled, total_adj_epa_pooled
    )

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
    season_pbp       = pbp       |> filter(season == szn),
    season_schedules = schedules |> filter(season == szn),
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
    wins    = sum(win),
    games   = n(),
    win_pct = wins / games,
    .groups = "drop"
  )

all_ratings <- all_ratings |>
  left_join(win_pct, by = c("season", "team"))

# =============================================================================
# Validation: new (phase-weighted, ridge) vs old (pooled, no shrinkage)
# =============================================================================

cli::cli_h1("Validation: new vs old approach")

val <- all_ratings |>
  filter(!is.na(win_pct))

cli::cli_h2("New approach — phase-weighted totals + ridge shrinkage")
val |>
  summarise(
    r_total   = cor(total_adj_epa,  win_pct, use = "complete.obs"),
    r_offense = cor(off_adj_epa,    win_pct, use = "complete.obs"),
    r_defense = cor(def_adj_epa,    win_pct, use = "complete.obs"),
    r_st      = cor(st_adj_epa,     win_pct, use = "complete.obs"),
    r_pass    = cor(off_adj_pass,   win_pct, use = "complete.obs"),
    r_rush    = cor(off_adj_rush,   win_pct, use = "complete.obs"),
    r_def_pass = cor(def_adj_pass,  win_pct, use = "complete.obs"),
    r_def_rush = cor(def_adj_rush,  win_pct, use = "complete.obs")
  ) |>
  mutate(across(everything(), \(x) round(x, 3))) |>
  print()

cli::cli_h2("Old approach — pooled mod_all, no shrinkage")
val |>
  summarise(
    r_total   = cor(total_adj_epa_pooled, win_pct, use = "complete.obs"),
    r_offense = cor(off_adj_epa_pooled,   win_pct, use = "complete.obs"),
    r_defense = cor(def_adj_epa_pooled,   win_pct, use = "complete.obs")
  ) |>
  mutate(across(everything(), \(x) round(x, 3))) |>
  print()

cli::cli_h2("Improvement (new - old)")
tibble(
  metric    = c("total", "offense", "defense"),
  new_r     = c(
    cor(val$total_adj_epa,       val$win_pct, use = "complete.obs"),
    cor(val$off_adj_epa,         val$win_pct, use = "complete.obs"),
    cor(val$def_adj_epa,         val$win_pct, use = "complete.obs")
  ),
  old_r     = c(
    cor(val$total_adj_epa_pooled, val$win_pct, use = "complete.obs"),
    cor(val$off_adj_epa_pooled,   val$win_pct, use = "complete.obs"),
    cor(val$def_adj_epa_pooled,   val$win_pct, use = "complete.obs")
  )
) |>
  mutate(
    delta = round(new_r - old_r, 4),
    new_r = round(new_r, 3),
    old_r = round(old_r, 3)
  ) |>
  print()

# Top/bottom sanity check
cli::cli_h2("Top 10 team-seasons (new ratings)")
all_ratings |>
  slice_max(total_adj_epa, n = 10) |>
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
# Write output
# =============================================================================

cli::cli_h1("Writing output")

arrow::write_parquet(
  all_ratings,
  file.path(PROC_DIR, "adj_ratings_season.parquet")
)
cli::cli_alert_success("Wrote adj_ratings_season.parquet")

cli::cli_alert_success("Opponent adjustment complete.")
