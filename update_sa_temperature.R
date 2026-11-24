#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# South Africa's daily temperature tracker.
#   Rscript update_sa_temperature.R            # download, rebuild charts
#   Rscript update_sa_temperature.R --offline  # rebuild from downloaded files
# The first run downloads the 1981 onwards record for 114 points (15-30
# minutes; repeated once a year). Weekly runs re-download only the last two
# years (about 5 minutes).
# Outputs:
#   data/processed/sa_temperature_daily.csv    national daily mean and 7-day mean
#   data/processed/sa_temperature_years.csv    yearly averages and anomalies
#   data/processed/sa_temperature_summary.json headline numbers
#   outputs/social/sa_temperature_*.png        charts in every format
# ---------------------------------------------------------------------------
# Work from the project folder, however the script was started (Rscript,
# source() or RStudio's Source button), so the relative paths below resolve.
local({
  here <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL)
  if (is.null(here)) {
    f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    if (length(f)) here <- dirname(normalizePath(f[1]))
  }
  for (d in c(here, dirname(here))) if (!is.null(d) && file.exists(file.path(d, "R", "theme_sain.R"))) {
    setwd(d); break
  }
  if (!file.exists("R/theme_sain.R")) stop("Run this from the sa-in-numbers project folder (setwd() to it first).")
})
# Offline: Rscript update_x.R --offline, or options(sain.offline = TRUE) before source()
OFFLINE <- "--offline" %in% commandArgs(trailingOnly = TRUE) || isTRUE(getOption("sain.offline"))
suppressPackageStartupMessages(library(ggplot2))
for (f in c("R/theme_sain.R", "R/fetch_power.R", "R/fetch_era5.R", "R/metrics_el_nino.R",
            "R/sa_temperature.R")) source(f)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
# The NASA POWER national series (used until 22 Nov 2026) is replaced by ERA5
unlink("data/sa_temperature/national_daily_history.csv")

d <- sa_temperature_series(offline = OFFLINE)
m <- sa_temperature_metrics(d)
s <- m$summary

out <- m$daily[, c("date", "t_c", "t7", "n_points")]
out$t7 <- round(out$t7, 3)
readr::write_csv(out, "data/processed/sa_temperature_daily.csv")
readr::write_csv(m$years, "data/processed/sa_temperature_years.csv")
s$lines <- c(
  sprintf("%d so far (to %s): %.1f°C on average, %s vs 1991-2020.", s$year, fmt_date(s$as_of),
          s$ytd_mean_c, fmt_c(s$ytd_anomaly_c)),
  sprintf("Rank: %s warmest of %d years since %d (same dates).", ordinal(s$ytd_rank_warmest), s$n_years, s$first_year),
  sprintf("Warmest full year: %d (%s).", s$record_year, fmt_c(s$record_year_anomaly_c)),
  sprintf("Trend: %+.2f°C a decade (95%% CI %.2f to %.2f), %s.", s$trend_c_per_decade,
          s$trend_ci_low, s$trend_ci_high, s$trend_years),
  sprintf("%d estimate: ~%.1f°C for the year.", s$year, s$year_estimate_c),
  sprintf("Warming: %s°C (%s).", sprintf("%.1f", s$warming_c), s$warming_periods))
jsonlite::write_json(s, "data/processed/sa_temperature_summary.json", auto_unbox = TRUE, pretty = TRUE)
cat("\n", paste(s$lines, collapse = "\n"), "\n\n")

save_social(plot_sa_temp_daily(m), "sa_temperature_daily")
save_social(plot_sa_temp_trend(m), "sa_temperature_trend")
save_social(plot_sa_temp_years(m), "sa_temperature_years")
message("Done. Next: quarto render")
