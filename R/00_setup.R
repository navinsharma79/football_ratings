# =============================================================================
# 00_setup.R
# Install required packages (run once before anything else)
# =============================================================================

pkgs <- c(
  "nflreadr",    # nflfastR data loader (lighter than nflfastR itself)
  "nflfastR",    # EPA models, play-by-play
  "tidyverse",   # dplyr, ggplot2, purrr, etc.
  "arrow",       # parquet read/write
  "glue",        # string interpolation
  "cli",         # pretty console output
  "broom",       # tidy model output
  "fixest"       # fast fixed-effects OLS (used for opponent adjustment)
)

missing_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]

if (length(missing_pkgs) > 0) {
  message("Installing: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
} else {
  message("All packages already installed.")
}

# Verify
invisible(lapply(pkgs, library, character.only = TRUE))
message("Setup complete.")
