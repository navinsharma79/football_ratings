# =============================================================================
# 01_ingest.R
# Download and cache NFL play-by-play data from nflfastR (2010-2025)
#
# Output: data/raw/pbp_{season}.parquet  (one file per season)
#         data/raw/schedules.parquet
#         data/processed/pbp_clean.parquet (all seasons, filtered & enriched)
# =============================================================================

library(nflreadr)
library(tidyverse)
library(arrow)
library(cli)

# -----------------------------------------------------------------------------
# Config
# -----------------------------------------------------------------------------

PROJ_ROOT    <- "//wsl.localhost/FedoraLinux-44/home/navin/football_ratings"
setwd(PROJ_ROOT)

SEASONS      <- 2010:2025
RAW_DIR      <- file.path(PROJ_ROOT, "data", "raw")
PROC_DIR     <- file.path(PROJ_ROOT, "data", "processed")

# nflfastR EPA is NA on a small number of plays (e.g. PAT attempts, some penalties).
# We keep those rows but flag them so downstream code can filter as needed.

# Garbage time definition (can be tuned later for predictive model):
#   Score diff > 28 AND 4th quarter AND < 5 min remaining
GARBAGE_SCORE_DIFF  <- 28
GARBAGE_QTR         <- 4
GARBAGE_SECS_REMAIN <- 300   # 5 minutes

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

#' Write a data frame to parquet, creating the directory if needed
write_parquet_safe <- function(df, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  arrow::write_parquet(df, path)
  cli::cli_alert_success("Wrote {nrow(df)} rows → {.file {path}}")
}

#' Return TRUE if parquet file exists and is non-empty
parquet_exists <- function(path) {
  file.exists(path) && file.size(path) > 0
}

# -----------------------------------------------------------------------------
# Step 1: Download raw play-by-play, one parquet per season
# -----------------------------------------------------------------------------

cli::cli_h1("Step 1: Downloading raw play-by-play data")

for (season in SEASONS) {
  out_path <- file.path(RAW_DIR, glue::glue("pbp_{season}.parquet"))

  if (parquet_exists(out_path)) {
    cli::cli_alert_info("Season {season}: cached, skipping.")
    next
  }

  cli::cli_alert_info("Season {season}: downloading...")

  pbp_raw <- tryCatch(
    nflreadr::load_pbp(seasons = season),
    error = function(e) {
      cli::cli_alert_warning("Season {season}: download failed — {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(pbp_raw) || nrow(pbp_raw) == 0) {
    cli::cli_alert_warning("Season {season}: no data returned, skipping.")
    next
  }

  write_parquet_safe(pbp_raw, out_path)
}

# -----------------------------------------------------------------------------
# Step 2: Download schedules (used for home/away, game results, roof/surface)
# -----------------------------------------------------------------------------

cli::cli_h1("Step 2: Downloading schedules")

schedules_path <- file.path(RAW_DIR, "schedules.parquet")

# nflreadr pbp data standardizes all team abbreviations to current names
# (e.g. OAK→LV, SD→LAC, STL→LA) but load_schedules() retains historical names.
# Apply the same mapping here so joins work correctly.
TEAM_STD <- c(
  OAK = "LV",   # Oakland Raiders → Las Vegas Raiders (moved 2020)
  SD  = "LAC",  # San Diego Chargers → Los Angeles Chargers (moved 2017)
  STL = "LA"    # St. Louis Rams → Los Angeles Rams (moved 2016)
)

standardize_teams <- function(x) {
  dplyr::recode(x, !!!TEAM_STD)
}

schedules <- nflreadr::load_schedules(seasons = SEASONS) |>
  select(
    game_id, season, week, game_type,
    home_team, away_team,
    home_score, away_score,
    result,          # home score - away score
    total,
    roof, surface,
    location,        # Home / Neutral / Away (for London etc.)
    stadium_id, stadium
  ) |>
  mutate(
    home_team = standardize_teams(home_team),
    away_team = standardize_teams(away_team)
  )

write_parquet_safe(schedules, schedules_path)

# -----------------------------------------------------------------------------
# Step 3: Load, filter, and enrich all seasons into a single clean file
# -----------------------------------------------------------------------------

cli::cli_h1("Step 3: Building clean play-by-play")

# Columns we actually need downstream (keeps memory manageable)
KEEP_COLS <- c(
  # Identifiers
  "play_id", "game_id", "season", "week", "game_date",
  "home_team", "away_team", "posteam", "defteam",
  "posteam_type",           # home / away

  # Situation
  "qtr", "down", "ydstogo", "yardline_100",
  "game_seconds_remaining", "half_seconds_remaining",
  "score_differential",
  "posteam_score", "defteam_score",
  "ep",                     # expected points before play
  "epa",                    # expected points added
  "wpa",                    # win probability added

  # Play classification
  "play_type",              # pass, run, field_goal, punt, kickoff, extra_point, no_play
  "special_teams_play",
  "pass_attempt", "rush_attempt",
  "sack", "qb_scramble", "qb_spike", "qb_kneel",
  "penalty", "penalty_yards",
  "two_point_attempt",

  # Passing detail
  "incomplete_pass", "interception",
  "air_yards", "yards_after_catch",
  "cp",                     # completion probability
  "cpoe",                   # completion % over expected

  # Rushing detail
  "rushing_yards",

  # Special teams detail
  "kick_distance",
  "field_goal_result",      # made / missed / blocked
  "extra_point_result",
  "punt_attempt",
  "kickoff_attempt",
  "return_yards",
  "touchback",              # kickoff/punt touchback flag
  "kickoff_in_endzone",     # 2024+ new rules: kick into endzone (not touchback)
  "kickoff_inside_twenty",  # kick downed inside opponent 20
  "punt_blocked",
  "kick_attempt",           # FG or XP attempt flag

  # Environmental (for FG model)
  "roof",                   # outdoors / dome / retractable / open
  "surface",                # grass / fieldturf / etc.

  # Receiver / rusher / passer (for player-level ratings later)
  "passer_player_id", "passer_player_name",
  "rusher_player_id", "rusher_player_name",
  "receiver_player_id", "receiver_player_name",
  "kicker_player_id", "kicker_player_name",
  "punter_player_id", "punter_player_name",

  # Result
  "yards_gained",
  "touchdown",
  "fumble_lost"
)

load_season <- function(season) {
  path <- file.path(RAW_DIR, glue::glue("pbp_{season}.parquet"))
  if (!parquet_exists(path)) {
    cli::cli_alert_warning("Season {season}: raw file missing, skipping.")
    return(NULL)
  }
  arrow::read_parquet(path)
}

clean_all_seasons <- map(SEASONS, function(season) {
  cli::cli_alert_info("Processing season {season}...")
  raw <- load_season(season)
  if (is.null(raw)) return(NULL)

  # Keep only columns that exist in this season's data
  # (schema has evolved over nflfastR versions)
  cols_present <- intersect(KEEP_COLS, names(raw))
  cols_missing <- setdiff(KEEP_COLS, names(raw))
  if (length(cols_missing) > 0) {
    cli::cli_alert_warning(
      "Season {season}: {length(cols_missing)} column(s) not present: {paste(cols_missing, collapse=', ')}"
    )
  }

  raw |>
    select(all_of(cols_present)) |>

    # --- Remove non-plays ---
    filter(
      !is.na(play_type),
      play_type != "no_play" | (play_type == "no_play" & !is.na(penalty_yards)),
      # keep penalties on no_play; drop true non-plays
      !(play_type %in% c("note", ""))
    ) |>

    # --- Flag plays to exclude from EPA aggregations ---
    mutate(
      # Kneel-downs: offensive EPA is misleadingly negative; exclude from offense
      is_kneel     = coalesce(qb_kneel,  0L) == 1,
      # Spikes: exclude from pass EPA
      is_spike     = coalesce(qb_spike,  0L) == 1,
      # Two-point conversions: separate analysis
      is_two_pt    = coalesce(two_point_attempt, 0L) == 1,

      # Garbage time flag
      is_garbage   = (
        abs(score_differential) > GARBAGE_SCORE_DIFF &
        qtr >= GARBAGE_QTR &
        game_seconds_remaining <= GARBAGE_SECS_REMAIN
      ),

      # Phase of game
      phase = case_when(
        special_teams_play == 1 ~ "special_teams",
        pass_attempt == 1 | qb_scramble == 1 | sack == 1 ~ "pass",
        rush_attempt == 1 | is_kneel ~ "rush",
        TRUE ~ "other"
      ),

      # Valid for EPA aggregation
      epa_valid = (
        !is.na(epa) &
        !is_kneel &
        !is_spike &
        !is_two_pt &
        play_type != "extra_point"
      )
    )
}) |>
  compact() |>          # drop NULLs from skipped seasons
  bind_rows()

cli::cli_alert_info("Total plays: {nrow(clean_all_seasons)}")
cli::cli_alert_info("Seasons covered: {paste(range(clean_all_seasons$season), collapse=' – ')}")

# Join roof/surface from schedules (some seasons have it on the play, others don't)
schedules_mini <- arrow::read_parquet(schedules_path) |>
  select(game_id, roof_sched = roof, surface_sched = surface, location)

clean_all_seasons <- clean_all_seasons |>
  left_join(schedules_mini, by = "game_id") |>
  mutate(
    roof    = coalesce(roof,    roof_sched),
    surface = coalesce(surface, surface_sched)
  ) |>
  select(-roof_sched, -surface_sched)

# Write final clean file
clean_path <- file.path(PROC_DIR, "pbp_clean.parquet")
write_parquet_safe(clean_all_seasons, clean_path)

# -----------------------------------------------------------------------------
# Step 4: Sanity checks
# -----------------------------------------------------------------------------

cli::cli_h1("Step 4: Sanity checks")

clean_all_seasons |>
  filter(epa_valid) |>
  group_by(season) |>
  summarise(
    n_plays         = n(),
    n_games         = n_distinct(game_id),
    mean_epa        = round(mean(epa, na.rm = TRUE), 4),
    pct_pass        = round(mean(phase == "pass"), 3),
    pct_rush        = round(mean(phase == "rush"), 3),
    pct_st          = round(mean(phase == "special_teams"), 3),
    pct_garbage     = round(mean(is_garbage), 3),
    .groups = "drop"
  ) |>
  print(n = Inf)

cli::cli_alert_success("Ingest complete.")
