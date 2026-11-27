# =============================================================================
# 02_aggregations.R
# Compute raw (unadjusted) EPA per play by team, season, week, and phase
#
# Inputs:  data/processed/pbp_clean.parquet
#          data/raw/schedules.parquet
# Outputs: data/processed/raw_team_epa.parquet   (team-season-week level)
#          data/processed/raw_season_epa.parquet  (team-season level)
# =============================================================================

library(nflreadr)
library(tidyverse)
library(arrow)
library(cli)
library(glue)

PROJ_ROOT <- "//wsl.localhost/FedoraLinux-44/home/navin/football_ratings"
setwd(PROJ_ROOT)

PROC_DIR <- file.path(PROJ_ROOT, "data", "processed")

# INCLUDE_PLAYOFFS: set via environment variable, e.g. INCLUDE_PLAYOFFS=TRUE Rscript ...
# FALSE (default) = regular season only
# TRUE            = regular season + all playoff rounds (WC, DIV, CON, SB)
INCLUDE_PLAYOFFS  <- Sys.getenv("INCLUDE_PLAYOFFS", unset = "FALSE") == "TRUE"
VALID_GAME_TYPES  <- if (INCLUDE_PLAYOFFS) c("REG","WC","DIV","CON","SB") else "REG"

# =============================================================================
# Load clean play-by-play
# =============================================================================

cli::cli_h1("Loading clean play-by-play")
cli::cli_alert_info("INCLUDE_PLAYOFFS = {INCLUDE_PLAYOFFS}")

pbp <- arrow::read_parquet(file.path(PROC_DIR, "pbp_clean.parquet")) |>
  filter(game_type %in% VALID_GAME_TYPES)
schedules <- arrow::read_parquet(file.path(PROJ_ROOT, "data", "raw", "schedules.parquet"))

cli::cli_alert_info("Loaded {nrow(pbp)} plays across seasons {min(pbp$season)}–{max(pbp$season)}")

# =============================================================================
# Helper: plays valid for each phase
# =============================================================================

# Offense (pass + rush): exclude garbage time, kneels, spikes, 2-pt, XP
pbp_off <- pbp |>
  filter(
    epa_valid,
    !is_garbage,
    phase %in% c("pass", "rush")
  )

# Defense: same plays, just from the defensive team's perspective
# EPA for defense = negative of offensive EPA (a good defensive play = negative EPA for offense)
pbp_def <- pbp_off  # same plays, we'll just flip EPA sign when aggregating

# Special teams: all valid ST plays, including garbage time
# (ST plays rarely happen in garbage time and sample size matters more)
pbp_st <- pbp |>
  filter(
    epa_valid,
    phase == "special_teams"
  )

cli::cli_alert_info(
  "Valid plays — Offense/Defense: {nrow(pbp_off)} | Special Teams: {nrow(pbp_st)}"
)

# =============================================================================
# Week-level aggregations (used for opponent adjustment + predictive model)
# =============================================================================

cli::cli_h1("Computing week-level EPA aggregations")

# --- Offense ---
off_weekly <- pbp_off |>
  group_by(season, week, team = posteam) |>
  summarise(
    off_epa_per_play  = mean(epa, na.rm = TRUE),
    off_epa_pass      = mean(epa[phase == "pass"],  na.rm = TRUE),
    off_epa_rush      = mean(epa[phase == "rush"],  na.rm = TRUE),
    off_n_plays       = n(),
    off_n_pass        = sum(phase == "pass"),
    off_n_rush        = sum(phase == "rush"),
    .groups = "drop"
  )

# --- Defense (flip EPA sign: negative EPA allowed = good defense) ---
def_weekly <- pbp_def |>
  group_by(season, week, team = defteam) |>
  summarise(
    def_epa_per_play  = -mean(epa, na.rm = TRUE),   # negative = better defense
    def_epa_pass      = -mean(epa[phase == "pass"],  na.rm = TRUE),
    def_epa_rush      = -mean(epa[phase == "rush"],  na.rm = TRUE),
    def_n_plays       = n(),
    def_n_pass        = sum(phase == "pass"),
    def_n_rush        = sum(phase == "rush"),
    .groups = "drop"
  )

# --- Special Teams ---
# Split by ST sub-type for diagnostics; also compute a combined ST EPA/play
st_weekly <- pbp_st |>
  group_by(season, week, team = posteam) |>
  summarise(
    st_epa_per_play   = mean(epa, na.rm = TRUE),
    st_n_plays        = n(),
    # sub-components (sample sizes will be small per week — used at season level)
    st_epa_fg         = mean(epa[play_type == "field_goal"],  na.rm = TRUE),
    st_epa_kickoff    = mean(epa[play_type == "kickoff"],     na.rm = TRUE),
    st_epa_punt       = mean(epa[play_type == "punt"],        na.rm = TRUE),
    st_n_fg           = sum(play_type == "field_goal"),
    st_n_kickoff      = sum(play_type == "kickoff"),
    st_n_punt         = sum(play_type == "punt"),
    .groups = "drop"
  )

# Note: ST also involves the *defending* team (kick coverage, punt coverage).
# We capture that by joining on defteam for kickoff/punt coverage plays.
st_coverage_weekly <- pbp_st |>
  filter(play_type %in% c("kickoff", "punt")) |>
  group_by(season, week, team = defteam) |>
  summarise(
    st_coverage_epa   = -mean(epa, na.rm = TRUE),   # negative = better coverage
    st_coverage_n     = n(),
    .groups = "drop"
  )

# Merge ST offense + coverage into one ST rating per team per week
st_weekly_combined <- st_weekly |>
  full_join(st_coverage_weekly, by = c("season", "week", "team")) |>
  mutate(
    # Simple combined ST: average of possession and coverage EPA
    # (weighted by play count in the season-level version)
    st_combined_epa = rowMeans(
      cbind(coalesce(st_epa_per_play, 0), coalesce(st_coverage_epa, 0)),
      na.rm = TRUE
    )
  )

# =============================================================================
# Join all three phases into one weekly team frame
# =============================================================================

cli::cli_h1("Building combined weekly team ratings")

# Get game metadata (opponent, home/away) from schedules
# Build two rows per game (one per team) before pivoting away home/away columns
game_meta_base <- schedules |>
  filter(game_type %in% VALID_GAME_TYPES) |>
  select(game_id, season, week, home_team, away_team)

game_meta <- bind_rows(
  game_meta_base |> mutate(team = home_team, opponent = away_team, is_home = TRUE),
  game_meta_base |> mutate(team = away_team, opponent = home_team, is_home = FALSE)
) |>
  select(season, week, team, opponent, is_home, game_id)

weekly_ratings <- game_meta |>
  left_join(off_weekly,          by = c("season", "week", "team")) |>
  left_join(def_weekly,          by = c("season", "week", "team")) |>
  left_join(st_weekly_combined,  by = c("season", "week", "team")) |>
  arrange(season, week, team)

cli::cli_alert_info("Weekly ratings: {nrow(weekly_ratings)} team-game rows")

# Sanity check: every team should have ~17-18 rows per season
weekly_ratings |>
  group_by(season, team) |>
  summarise(n_games = n(), .groups = "drop") |>
  summarise(
    min_games = min(n_games),
    max_games = max(n_games),
    median_games = median(n_games)
  ) |>
  print()

# =============================================================================
# Season-level aggregations (descriptive ratings)
# =============================================================================

cli::cli_h1("Computing season-level EPA aggregations")

season_ratings <- weekly_ratings |>
  group_by(season, team) |>
  summarise(
    # Offense
    off_epa_per_play  = weighted.mean(off_epa_per_play, off_n_plays, na.rm = TRUE),
    off_epa_pass      = weighted.mean(off_epa_pass,     off_n_pass,  na.rm = TRUE),
    off_epa_rush      = weighted.mean(off_epa_rush,     off_n_rush,  na.rm = TRUE),
    off_n_plays       = sum(off_n_plays, na.rm = TRUE),

    # Defense
    def_epa_per_play  = weighted.mean(def_epa_per_play, def_n_plays, na.rm = TRUE),
    def_epa_pass      = weighted.mean(def_epa_pass,     def_n_pass,  na.rm = TRUE),
    def_epa_rush      = weighted.mean(def_epa_rush,     def_n_rush,  na.rm = TRUE),
    def_n_plays       = sum(def_n_plays, na.rm = TRUE),

    # Special Teams
    st_epa_per_play   = weighted.mean(st_epa_per_play,  st_n_plays,  na.rm = TRUE),
    st_epa_fg         = weighted.mean(st_epa_fg,        st_n_fg,     na.rm = TRUE),
    st_epa_kickoff    = weighted.mean(st_epa_kickoff,   st_n_kickoff,na.rm = TRUE),
    st_epa_punt       = weighted.mean(st_epa_punt,      st_n_punt,   na.rm = TRUE),
    st_coverage_epa   = weighted.mean(st_coverage_epa,  st_coverage_n, na.rm = TRUE),
    st_combined_epa   = mean(st_combined_epa,           na.rm = TRUE),
    st_n_plays        = sum(st_n_plays, na.rm = TRUE),

    n_games           = n(),
    .groups = "drop"
  ) |>
  # Composite total rating: offense + defense + ST
  # Weights are naive here; will be optimized in the predictive model (05_composite.R)
  mutate(
    total_epa = off_epa_per_play + def_epa_per_play + (0.5 * st_combined_epa)
  ) |>
  arrange(season, desc(total_epa))

cli::cli_alert_info("Season ratings: {nrow(season_ratings)} team-season rows")

# Quick peek: top 10 teams all-time by total EPA
cli::cli_h2("Top 10 team-seasons by raw total EPA")
season_ratings |>
  slice_max(total_epa, n = 10) |>
  select(season, team, off_epa_per_play, def_epa_per_play, st_combined_epa, total_epa) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print()

cli::cli_h2("Bottom 10 team-seasons by raw total EPA")
season_ratings |>
  slice_min(total_epa, n = 10) |>
  select(season, team, off_epa_per_play, def_epa_per_play, st_combined_epa, total_epa) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print()

# =============================================================================
# Write outputs
# =============================================================================

cli::cli_h1("Writing outputs")

arrow::write_parquet(
  weekly_ratings,
  file.path(PROC_DIR, "raw_team_epa_weekly.parquet")
)
cli::cli_alert_success("Wrote raw_team_epa_weekly.parquet")

arrow::write_parquet(
  season_ratings,
  file.path(PROC_DIR, "raw_team_epa_season.parquet")
)
cli::cli_alert_success("Wrote raw_team_epa_season.parquet")

cli::cli_alert_success("Aggregations complete.")
