# =============================================================================
# 04_special_teams.R
# Build dedicated expected-value models for FG, punt, and kickoff,
# then produce improved ST ratings that replace raw EPA-based ST.
#
# Why bother beyond raw EPA?
#   nflfastR EPA already prices field position, but it doesn't separate
#   *unit quality* from *opportunity quality*. A kicker who makes 55-yarders
#   looks the same in raw EPA as one who only attempts 35-yarders and makes
#   them. These models isolate the quality signal.
#
# Components:
#   1. FG model     — logistic P(make | distance, dome, surface)
#                     → FG points above expected (PAE) per team per season
#   2. Punt model   — expected net field position given kick distance + field pos
#                     → punt yards above expected per team per season
#   3. Kickoff model— expected return start position given kick distance
#                     → kickoff value above expected per team per season
#   4. Combine into revised ST ratings, joined back to adj_ratings_season
#
# Inputs:  data/processed/pbp_clean.parquet
#          data/processed/adj_ratings_season.parquet
# Outputs: data/processed/st_component_ratings.parquet
#          data/processed/adj_ratings_season_v2.parquet  (with improved ST)
# =============================================================================

library(tidyverse)
library(arrow)
library(cli)
library(glue)
library(broom)

PROJ_ROOT  <- "//wsl.localhost/FedoraLinux-44/home/navin/football_ratings"
setwd(PROJ_ROOT)
PROC_DIR   <- file.path(PROJ_ROOT, "data", "processed")

# =============================================================================
# Kickoff rule history lookup
# Used to price the value of a touchback by season, and to flag era changes
# that affect how we model kicker quality vs. coverage quality.
#
# Kickoff line history:
#   ≤2010: kicking team at own 30-yard line
#   2011+: kicking team at own 35-yard line  → more touchbacks, fewer returns
#
# Touchback yard line history (where receiving team starts after touchback):
#   ≤2015:  own 20-yard line
#   2016-2023: own 25-yard line
#   2024:   own 30-yard line  (new kickoff format, year 1)
#   2025+:  own 35-yard line  (new kickoff format, further incentivizes landing zone)
#
# New kickoff format (2024+):
#   Kicker at own 40, coverage lines up at opp 40, can't move until catch.
#   Landing zone = opp goal line to opp 20. Kick into/through EZ → touchback.
#   Kick short of landing zone → penalty, opponent ball at their 40.
# =============================================================================

TOUCHBACK_YARDLINE <- tribble(
  ~season_from, ~season_to, ~tb_yardline, ~kickoff_line, ~era_label,
  2010L,        2010L,      20L,          30L,           "pre_2011",
  2011L,        2015L,      20L,          35L,           "pre_2016",
  2016L,        2023L,      25L,          35L,           "pre_2024",
  2024L,        2024L,      30L,          40L,           "new_2024",
  2025L,        2099L,      35L,          40L,           "new_2025plus"
) |>
  # Expand to one row per season
  rowwise() |>
  mutate(season = list(season_from:season_to)) |>
  unnest(season) |>
  select(season, tb_yardline, kickoff_line, era_label)

cli::cli_alert_info("Kickoff rule eras:")
print(TOUCHBACK_YARDLINE)

# =============================================================================
# Load data
# =============================================================================

cli::cli_h1("Loading data")

pbp <- arrow::read_parquet(file.path(PROC_DIR, "pbp_clean.parquet"))
adj <- arrow::read_parquet(file.path(PROC_DIR, "adj_ratings_season.parquet"))

cli::cli_alert_info("Loaded {nrow(pbp)} plays")

# =============================================================================
# 1. Field Goal Model
# =============================================================================

cli::cli_h1("1. Field Goal Model")

fg_plays <- pbp |>
  filter(
    play_type == "field_goal",
    !is.na(kick_distance),
    !is.na(field_goal_result),
    kick_distance >= 17,          # spike the occasional 1-yarder data error
    kick_distance <= 70
  ) |>
  mutate(
    made      = as.integer(field_goal_result == "made"),
    is_dome   = as.integer(roof %in% c("dome", "retractable")),
    is_grass  = as.integer(surface %in% c("grass", "natural_grass", "dessograss"))
  )

cli::cli_alert_info("FG attempts: {nrow(fg_plays)} ({sum(fg_plays$made)} made)")

# Logistic regression: P(make) ~ distance + distance^2 + dome + grass
# Quadratic distance term captures the curve flattening near 65+ yards
fg_model <- glm(
  made ~ kick_distance + I(kick_distance^2) + is_dome + is_grass,
  data   = fg_plays,
  family = binomial(link = "logit")
)

cli::cli_h2("FG model summary")
tidy(fg_model) |>
  mutate(across(where(is.numeric), \(x) round(x, 5))) |>
  print()

# Expected make probability for each attempt
fg_plays <- fg_plays |>
  mutate(
    fg_exp_prob  = predict(fg_model, newdata = fg_plays, type = "response"),
    fg_exp_pts   = fg_exp_prob * 3,          # expected points from attempt
    fg_actual_pts = made * 3,                 # actual points (3 or 0)
    fg_pae        = fg_actual_pts - fg_exp_pts # points above expected
  )

# Team-season FG ratings
fg_team <- fg_plays |>
  group_by(season, team = posteam) |>
  summarise(
    fg_pae_total   = sum(fg_pae,  na.rm = TRUE),
    fg_pae_per_att = mean(fg_pae, na.rm = TRUE),
    fg_attempts    = n(),
    fg_made        = sum(made),
    fg_pct_actual  = mean(made),
    fg_pct_exp     = mean(fg_exp_prob),
    .groups = "drop"
  )

cli::cli_h2("Top 10 kicker seasons (FG PAE per attempt)")
fg_team |>
  slice_max(fg_pae_per_att, n = 10) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print()

cli::cli_h2("Bottom 10 kicker seasons")
fg_team |>
  slice_min(fg_pae_per_att, n = 10) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print()

# Sanity: Justin Tucker should dominate
cli::cli_h2("BAL kicker seasons")
fg_team |> filter(team == "BAL") |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

# =============================================================================
# 2. Punt Model
# =============================================================================

cli::cli_h1("2. Punt Model")

punt_plays <- pbp |>
  filter(
    play_type == "punt",
    !is.na(kick_distance),
    !is.na(yardline_100),
    coalesce(punt_blocked, 0) == 0,   # exclude blocked punts from model fitting
    kick_distance >= 10,
    kick_distance <= 80
  ) |>
  mutate(
    # Net field position: where does the opponent take over?
    # yardline_100 = yards from own end zone for punting team
    # After punt: return_yards already in pbp; use epa as outcome
    # We model net yards gained (field position change) as the outcome
    net_yards    = kick_distance - coalesce(return_yards, 0L),
    is_dome      = as.integer(roof %in% c("dome", "retractable")),
    # kick_from_own: yards from own goal line (100 - yardline_100)
    kick_from    = 100L - as.integer(yardline_100)
  ) |>
  filter(!is.na(net_yards), net_yards >= -10, net_yards <= 80)

cli::cli_alert_info("Punt plays for model: {nrow(punt_plays)}")

# OLS: net_yards ~ kick_distance + kick_from + dome
# kick_from captures the "coffin corner" opportunity — punters near their own
# end zone have more room to angle for the sideline
punt_model <- lm(
  net_yards ~ kick_distance + kick_from + is_dome,
  data = punt_plays
)

cli::cli_h2("Punt model summary")
tidy(punt_model) |>
  mutate(across(where(is.numeric), \(x) round(x, 4))) |>
  print()

punt_plays <- punt_plays |>
  mutate(
    punt_exp_net = predict(punt_model, newdata = punt_plays),
    punt_yae     = net_yards - punt_exp_net    # yards above expected
  )

# Team-season punt ratings
punt_team <- punt_plays |>
  group_by(season, team = posteam) |>
  summarise(
    punt_yae_total   = sum(punt_yae,  na.rm = TRUE),
    punt_yae_per_att = mean(punt_yae, na.rm = TRUE),
    punt_attempts    = n(),
    punt_net_avg     = mean(net_yards, na.rm = TRUE),
    punt_exp_avg     = mean(punt_exp_net, na.rm = TRUE),
    .groups = "drop"
  )

cli::cli_h2("Top 10 punter seasons (yards above expected per punt)")
punt_team |>
  slice_max(punt_yae_per_att, n = 10) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print()

# =============================================================================
# 3. Kickoff Model (era-split)
#
# Pre-2024: kick_distance was a meaningful signal — longer kicks → touchback
#   at the 25, shorter kicks gave returners a chance. We use raw EPA directly
#   since the OLS distance model was not statistically significant across the
#   full sample (touchback rates were already high and noisy). Coverage EPA
#   is the primary kicking-team signal.
#
# 2024+ (new kickoff rules): kicker lines up at own 40, coverage at opp 40,
#   can't move until catch. Touchbacks go to the 30 (worse for kicking team).
#   The kicker's job is PLACEMENT ACCURACY — landing in the "landing zone"
#   (goal line to opp 20) to create a live coverage play. Kicking it through
#   the end zone is now slightly bad (opponent at 30). Coverage EPA is the
#   dominant signal since the new formation creates genuine tackling contests.
#   Kicker placement proxy: open_field_return_pct (% of kicks that land in
#   the landing zone and produce a live return, vs. going into the end zone).
# =============================================================================

cli::cli_h1("3. Kickoff Model (era-split)")

NEW_RULES_SEASON <- 2024L

ko_plays <- pbp |>
  filter(
    play_type == "kickoff",
    !is.na(kick_distance),
    !is.na(epa),
    kick_distance >= 30,
    kick_distance <= 80
  ) |>
  left_join(TOUCHBACK_YARDLINE, by = "season") |>
  mutate(
    era          = if_else(season >= NEW_RULES_SEASON, "new_rules", "old_rules"),
    is_touchback = as.integer(coalesce(touchback, 0L) == 1),
    # open_field: kick landed in landing zone → live return, not in endzone, not TB
    # proxy: not a touchback AND not flagged as kickoff_in_endzone
    # (kickoff_inside_twenty is a coverage outcome, not a placement metric)
    is_open_field = as.integer(
      is_touchback == 0L &
      coalesce(kickoff_in_endzone, 0L) == 0L
    )
  )

cli::cli_alert_info("Kickoff plays: {nrow(ko_plays)} ({sum(ko_plays$era=='old_rules')} pre-2024, {sum(ko_plays$era=='new_rules')} 2024+)")

# --- Pre-2024: use raw EPA directly (distance model not significant) ---
# --- 2024+:    kicker placement = open_field_pct; coverage is primary signal ---

# Validate touchback yard line lookup against actual data
# League-average touchback EPA should track the increasing cost of touchbacks
cli::cli_h2("Touchback EPA by era (should worsen as tb_yardline increases)")
ko_plays |>
  filter(is_touchback == 1) |>
  group_by(era_label, tb_yardline) |>
  summarise(
    n             = n(),
    mean_tb_epa   = round(mean(epa, na.rm = TRUE), 4),
    .groups = "drop"
  ) |>
  arrange(tb_yardline) |>
  print()

# Kicker placement metric (2024+ only): open_field_return_pct
# EPA on kickoff plays is from the RECEIVING team's perspective (posteam).
# Therefore the KICKING team = defteam. Group by defteam for all kicking metrics.
# Higher open_field_pct for defteam = kicker is placing the ball in the landing
# zone more often (creating live returns rather than giving away the 35-yd touchback).
ko_placement_new <- ko_plays |>
  filter(era == "new_rules") |>
  group_by(season, team = defteam) |>
  summarise(
    ko_open_field_pct = mean(is_open_field, na.rm = TRUE),
    ko_touchback_pct  = mean(is_touchback,  na.rm = TRUE),
    ko_n              = n(),
    .groups = "drop"
  ) |>
  group_by(season) |>
  mutate(
    # Relative to league average: positive = more landing-zone kicks (better placement)
    ko_placement_vs_avg = ko_open_field_pct - mean(ko_open_field_pct, na.rm = TRUE)
  ) |>
  ungroup()

cli::cli_h2("2024+ kickoff placement (open field rate vs league avg)")
ko_placement_new |>
  arrange(season, desc(ko_placement_vs_avg)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

# Coverage ratings: how well does the kicking team limit return EPA?
# Split by era — coverage is more meaningful post-2024 (new formation)
ko_coverage_old <- ko_plays |>
  filter(era == "old_rules", !is.na(epa)) |>
  group_by(season, team = defteam) |>
  summarise(
    ko_coverage_epa = -mean(epa, na.rm = TRUE),   # flip: lower opp EPA = better
    ko_coverage_n   = n(),
    .groups = "drop"
  )

ko_coverage_new <- ko_plays |>
  filter(era == "new_rules", !is.na(epa)) |>
  group_by(season, team = defteam) |>
  summarise(
    ko_coverage_epa = -mean(epa, na.rm = TRUE),
    ko_coverage_n   = n(),
    .groups = "drop"
  )

ko_coverage <- bind_rows(ko_coverage_old, ko_coverage_new)

cli::cli_h2("Top 10 kickoff coverage seasons (all eras)")
ko_coverage |>
  slice_max(ko_coverage_epa, n = 10) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print()

# Kickoff return quality: how well does this team RETURN kicks?
# posteam = receiving/return team. Higher EPA = better return unit.
ko_return <- ko_plays |>
  filter(!is.na(epa), is_touchback == 0L) |>   # only plays with actual returns
  group_by(season, team = posteam) |>
  summarise(
    ko_return_epa = mean(epa, na.rm = TRUE),
    ko_return_n   = n(),
    .groups = "drop"
  )

# Pre-2024 kicking quality (leg strength era): touchback rate from kicking team
# defteam = kicking team. Higher touchback rate = stronger leg = better for kicker.
# Center within season (league avg touchback rate varies a lot by era/rule).
ko_kicking_old <- ko_plays |>
  filter(era == "old_rules") |>
  group_by(season, team = defteam) |>
  summarise(
    ko_touchback_rate = mean(is_touchback, na.rm = TRUE),
    ko_n              = n(),
    .groups = "drop"
  ) |>
  group_by(season) |>
  mutate(ko_touchback_vs_avg = ko_touchback_rate - mean(ko_touchback_rate, na.rm = TRUE)) |>
  ungroup()

# =============================================================================
# 4. Punt Return & Kick Return (coverage vs return unit)
# =============================================================================

cli::cli_h1("4. Return ratings")

# Punt returns
pr_plays <- pbp |>
  filter(
    play_type == "punt",
    !is.na(epa),
    coalesce(punt_blocked, 0) == 0
  )

# Return team (posteam = team fielding the punt)
pr_return <- pr_plays |>
  group_by(season, team = posteam) |>
  summarise(
    pr_epa_per_ret = mean(epa, na.rm = TRUE),
    pr_n           = n(),
    .groups = "drop"
  )

# Coverage team (defteam = team that punted)
pr_coverage <- pr_plays |>
  group_by(season, team = defteam) |>
  summarise(
    pr_coverage_epa = -mean(epa, na.rm = TRUE),
    pr_coverage_n   = n(),
    .groups = "drop"
  )

# =============================================================================
# 5. Combine into revised team-season ST ratings
# =============================================================================

cli::cli_h1("5. Building composite ST ratings")

# Scale each component to EPA-per-play units for comparability.
# FG:        PAE (points) → EPA-equiv via plays-per-drive denominator
# Punt:      yards above expected → EPA via yards-to-EPA conversion
# KO kicking (pre-2024): raw EPA per kick (small signal)
# KO kicking (2024+):    placement vs avg (open_field_pct − league_avg), scaled
# KO coverage:           raw EPA (flipped), consistent across eras
# Returns:               raw EPA

PLAYS_PER_DRIVE    <- 6.0    # normalize FG PAE
YARDS_TO_EPA       <- 0.05   # 1 yard field position ≈ 0.05 EPA
# Scaling factor for placement metric: open_field_pct is in [0,1] range;
# multiply so 1 SD (~0.05) maps to roughly 0.05 EPA — same order as other components
PLACEMENT_SCALE    <- 1.0

st_ratings <- adj |>
  select(season, team) |>

  # FG
  left_join(fg_team   |> select(season, team, fg_pae_per_att, fg_attempts),
            by = c("season", "team")) |>

  # Punt
  left_join(punt_team |> select(season, team, punt_yae_per_att, punt_attempts),
            by = c("season", "team")) |>

  # KO kicking: pre-2024 touchback rate vs avg (leg strength); 2024+ placement
  # Both group by defteam (= kicking team, since posteam = receiving team on KOs)
  left_join(ko_kicking_old   |> select(season, team, ko_touchback_vs_avg),
            by = c("season", "team")) |>
  left_join(ko_placement_new |> select(season, team, ko_placement_vs_avg),
            by = c("season", "team")) |>

  # KO coverage (all eras) — defteam = kicking team, -mean(receiving_EPA)
  left_join(ko_coverage |> select(season, team, ko_coverage_epa),
            by = c("season", "team")) |>

  # KO return quality — posteam = receiving team, mean(receiving_EPA) on live returns
  left_join(ko_return |> select(season, team, ko_return_epa),
            by = c("season", "team")) |>

  # Punt: posteam = punting team (punts from their possession), epa from their view
  left_join(pr_return   |> select(season, team, pr_epa_per_ret, pr_n),
            by = c("season", "team")) |>

  # Punt return quality: defteam = return team on punt plays, -mean(punting_EPA)
  left_join(pr_coverage |> select(season, team, pr_coverage_epa, pr_coverage_n),
            by = c("season", "team")) |>

  mutate(
    # --- Normalize to common EPA-per-play scale ---
    fg_epa_equiv   = coalesce(fg_pae_per_att,  0) / PLAYS_PER_DRIVE,
    punt_epa_equiv = coalesce(punt_yae_per_att, 0) * YARDS_TO_EPA,

    # KO kicking: era-appropriate metric (both already centered vs league avg)
    ko_kick_equiv  = case_when(
      season >= NEW_RULES_SEASON ~ coalesce(ko_placement_vs_avg,  0) * PLACEMENT_SCALE,
      TRUE                       ~ coalesce(ko_touchback_vs_avg,  0) * PLACEMENT_SCALE
    ),

    # KO coverage: weight 1.5x for 2024+ (new formation makes coverage primary signal)
    ko_cov_equiv   = coalesce(ko_coverage_epa, 0) *
                       if_else(season >= NEW_RULES_SEASON, 1.5, 1.0),

    # KO return: live returns only (touchbacks excluded); small sample per season
    ko_ret_equiv   = coalesce(ko_return_epa, 0),

    # Punt units
    pr_epa_equiv     = coalesce(pr_epa_per_ret,  0),
    pr_cov_epa_equiv = coalesce(pr_coverage_epa, 0),

    # Composite ST: equal-weight average of 7 sub-components
    # Weights will be tuned in predictive model (05_composite.R)
    st_composite = (
      fg_epa_equiv   +
      punt_epa_equiv +
      ko_kick_equiv  +
      ko_cov_equiv   +
      ko_ret_equiv   +
      pr_epa_equiv   +
      pr_cov_epa_equiv
    ) / 7
  )

# Center to league average = 0 within each season
st_ratings <- st_ratings |>
  group_by(season) |>
  mutate(st_composite = st_composite - mean(st_composite, na.rm = TRUE)) |>
  ungroup()

cli::cli_h2("Top 10 ST seasons (composite)")
st_ratings |>
  slice_max(st_composite, n = 10) |>
  select(season, team, fg_epa_equiv, punt_epa_equiv, ko_kick_equiv,
         ko_cov_equiv, ko_ret_equiv, pr_epa_equiv, pr_cov_epa_equiv, st_composite) |>
  mutate(across(where(is.numeric), \(x) round(x, 4))) |>
  print()

cli::cli_h2("Bottom 10 ST seasons")
st_ratings |>
  slice_min(st_composite, n = 10) |>
  select(season, team, fg_epa_equiv, punt_epa_equiv, ko_kick_equiv,
         ko_cov_equiv, ko_ret_equiv, pr_epa_equiv, pr_cov_epa_equiv, st_composite) |>
  mutate(across(where(is.numeric), \(x) round(x, 4))) |>
  print()

# =============================================================================
# 6. Merge improved ST back into main ratings, validate
# =============================================================================

cli::cli_h1("6. Updating season ratings with improved ST")

adj_v2 <- adj |>
  select(-st_adj_poss, -st_adj_cov, -st_adj_epa, -total_adj_epa) |>
  left_join(
    st_ratings |> select(season, team, st_composite,
                         fg_epa_equiv, punt_epa_equiv,
                         ko_kick_equiv, ko_cov_equiv, ko_ret_equiv,
                         pr_epa_equiv, pr_cov_epa_equiv),
    by = c("season", "team")
  ) |>
  mutate(
    st_adj_epa    = st_composite,
    total_adj_epa = off_adj_epa + def_adj_epa + (0.5 * st_adj_epa)
  )

# Validation
cor_v2 <- adj_v2 |>
  filter(!is.na(win_pct)) |>
  summarise(
    r_total   = cor(total_adj_epa, win_pct, use = "complete.obs") |> round(3),
    r_offense = cor(off_adj_epa,   win_pct, use = "complete.obs") |> round(3),
    r_defense = cor(def_adj_epa,   win_pct, use = "complete.obs") |> round(3),
    r_st      = cor(st_adj_epa,    win_pct, use = "complete.obs") |> round(3),
    r_fg      = cor(fg_epa_equiv,  win_pct, use = "complete.obs") |> round(3),
    r_punt    = cor(punt_epa_equiv,win_pct, use = "complete.obs") |> round(3),
    r_ko_kick = cor(ko_kick_equiv, win_pct, use = "complete.obs") |> round(3),
    r_ko_cov  = cor(ko_cov_equiv,  win_pct, use = "complete.obs") |> round(3)
  )

cli::cli_alert_info("Correlation with same-season win % (v2 with improved ST):")
print(cor_v2)

# =============================================================================
# Write outputs
# =============================================================================

cli::cli_h1("Writing outputs")

arrow::write_parquet(
  st_ratings,
  file.path(PROC_DIR, "st_component_ratings.parquet")
)
cli::cli_alert_success("Wrote st_component_ratings.parquet")

arrow::write_parquet(
  adj_v2,
  file.path(PROC_DIR, "adj_ratings_season_v2.parquet")
)
cli::cli_alert_success("Wrote adj_ratings_season_v2.parquet")

cli::cli_alert_success("Special teams model complete.")
