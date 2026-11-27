# Open Source NFL Team Ratings

An open-source NFL team rating system built on EPA (Expected Points Added) methodology, covering all three phases of the game — offense, defense, and special teams — from 2010 through the current season.

Existing NFL ratings systems (DVOA, QBR, QBERT) are closed-source. This project aims to replicate and extend their core ideas using publicly available data and transparent methodology.

---

## Ratings

Pre-built CSVs are published each week in [`output/ratings/`](output/ratings/).

| File | Description |
|---|---|
| `final_ratings_all_seasons.csv` | All seasons, all teams, both descriptive and predictive ratings |
| `ratings_{season}.csv` | Single-season ratings, sorted by descriptive rank |

### Column glossary

| Column | Description |
|---|---|
| `off_adj_pass` | Opponent-adjusted pass EPA per play |
| `off_adj_rush` | Opponent-adjusted rush EPA per play |
| `def_adj_epa` | Opponent-adjusted defensive EPA per play (higher = better defense) |
| `def_adj_pass` | Opponent-adjusted pass defense EPA per play |
| `def_adj_rush` | Opponent-adjusted rush defense EPA per play |
| `st_adj_epa` | Composite special teams rating (see below) |
| `total_descriptive` | Same-season composite: best for explaining current results |
| `total_predictive` | Shrinkage-weighted composite: best for forecasting next season |
| `rank_descriptive` | Team rank within season by descriptive rating |
| `rank_predictive` | Team rank within season by predictive rating |

---

## Methodology

### Data source

All play-by-play data comes from [nflfastR](https://www.nflfastpy.com/) / [nflverse](https://nflverse.nflverse.com/) via the `nflreadr` R package. EPA values are pre-computed by nflfastR's expected points model. Seasons covered: 2010–present.

### Opponent adjustment

Raw EPA per play is not opponent-adjusted — a good offense facing bad defenses all year looks better than it is. We correct for this using a **simultaneous two-way fixed-effects regression** (via `fixest::feols`):

```
epa ~ home_field | posteam + defteam
```

Each team's `posteam` fixed effect is their opponent-adjusted offensive contribution; each team's `defteam` fixed effect (sign-flipped) is their opponent-adjusted defensive contribution. This runs once per season and replaces iterative adjustment methods.

### Special teams

ST is split into six sub-components, each attributed to the correct team:

| Component | Method | Teams involved |
|---|---|---|
| Field goals | Logistic FG% model vs. expected (distance, dome, surface) → points above expected | Kicking team |
| Punts | OLS net yards above expected (kick distance, field position, dome) | Punting team |
| Punt returns | Raw EPA | Return team |
| Punt coverage | Raw EPA (flipped) | Punting team |
| Kickoff (pre-2024) | Touchback rate vs. league average (leg strength era) | Kicking team |
| Kickoff (2024+) | Open-field placement rate vs. league average | Kicking team |
| Kickoff coverage | Raw EPA (flipped) | Kicking team |
| Kickoff returns | Raw EPA on live returns | Return team |

**Kickoff era note:** EPA on kickoff plays is from the *receiving team's* perspective in nflfastR. The NFL changed the kickoff touchback yard line from the 25 (pre-2024) → 30 (2024) → 35 (2025+), and introduced a new kickoff formation in 2024. Kicker quality is now primarily placement accuracy (landing the ball in the goal line–to–20 "landing zone") rather than raw leg strength.

### Descriptive vs. predictive ratings

**Descriptive** ratings use opponent-adjusted EPA with equal-ish phase weights. They are best for explaining what happened in a given season.

**Predictive** ratings apply two adjustments before combining phases:

1. **Regression to mean (shrinkage):** Each component is shrunk toward zero by `n / (n + k)`, where `k` is calibrated from the component's year-to-year autocorrelation. Teams with fewer games get shrunk harder.

2. **Optimized phase weights:** Weights are fit to maximize correlation with *next-season* win% on training data (2010–2020) and validated on held-out seasons (2021–2025).

#### Year-to-year persistence (why weights differ)

| Component | Same-season r | Next-season r | Persistence |
|---|---|---|---|
| Pass offense | 0.71 | 0.34 | 0.46 |
| Rush offense | 0.42 | 0.22 | 0.25 |
| Defense | 0.49 | 0.18 | 0.32 |
| Special teams | 0.15 | 0.12 | 0.31 |

Defense is notably less persistent than offense year-to-year. This reflects how defensive performance depends heavily on scheme and exploiting an opponent's specific weaknesses — both of which change each season — whereas offensive quality is more anchored to QB continuity.

#### Validated predictive accuracy (2021–2025 holdout)

| Model | Next-season win% r |
|---|---|
| Predictive (optimized weights + shrinkage) | **0.377** |
| Descriptive (naive weights, no shrinkage) | 0.343 |
| Pass offense alone | 0.414 |
| Defense alone | 0.056 |
| Special teams alone | 0.003 |

Pass offense is the single best predictor of future team success, consistent with NFL analytics research showing QB quality as the dominant factor in team outcomes.

---

## Garbage time

Plays are excluded from EPA aggregations when:
- Score differential > 28 points
- In the 4th quarter
- Less than 5 minutes remaining

Thresholds are configurable in `R/01_ingest.R`.

---

## Reproducing the ratings

### Requirements

- R 4.5+ (tested on 4.6.2)
- Packages: `nflreadr`, `tidyverse`, `arrow`, `fixest`, `broom`, `glue`, `cli`

Install packages (run once in RStudio):
```r
install.packages(c("nflreadr","nflfastR","tidyverse","arrow","fixest","broom","glue","cli"))
```

### Run the pipeline

Scripts are numbered and must be run in order. The first run downloads ~16 seasons of play-by-play data (~500MB); subsequent runs use the local cache.

```r
source("R/01_ingest.R")          # download & clean play-by-play
source("R/02_aggregations.R")    # raw EPA by team, phase, week
source("R/03_opp_adjustments.R") # opponent adjustment via fixed effects
source("R/04_special_teams.R")   # FG, punt, kickoff component models
source("R/05_composite.R")       # weight optimization & final ratings
```

Or from the terminal (after adding R to PATH):
```bash
Rscript R/01_ingest.R
Rscript R/02_aggregations.R
Rscript R/03_opp_adjustments.R
Rscript R/04_special_teams.R
Rscript R/05_composite.R
```

### Data caching

Raw play-by-play parquet files are cached in `data/raw/` and not committed to git. Re-running any script skips already-cached seasons. To refresh a specific season, delete `data/raw/pbp_{season}.parquet`.

---

## Project structure

```
football_ratings/
├── R/
│   ├── 00_setup.R              # package installation
│   ├── 01_ingest.R             # data download & cleaning
│   ├── 02_aggregations.R       # raw EPA aggregations
│   ├── 03_opp_adjustments.R    # opponent adjustment
│   ├── 04_special_teams.R      # ST component models
│   └── 05_composite.R          # final ratings
├── data/
│   ├── raw/                    # cached parquet files (not committed)
│   └── processed/              # intermediate outputs (not committed)
├── output/
│   └── ratings/                # final CSV ratings (committed)
└── README.md
```

---

## Prior art and references

- [nflfastR](https://www.nflfastr.com/) — play-by-play data with pre-built EPA model
- [nflverse](https://nflverse.nflverse.com/) — broader nflverse data ecosystem
- [Ben Baldwin's nfl4th](https://github.com/nflverse/nfl4th) — open-source 4th down model
- [rbsdm.com](https://rbsdm.com) — open EPA-based QB/team metrics
- Football Outsiders DVOA — the original opponent-adjusted NFL rating (closed source)

---

## Contributing

Issues and PRs welcome. If you find a methodology error, have a suggestion for improving the ST models, or want to add QB-level ratings, open an issue.

---

## License

MIT
