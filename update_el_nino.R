#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Weekly update for the El Nino summer tracker.
#   Rscript update_el_nino.R            # download fresh data, rebuild everything
#   Rscript update_el_nino.R --offline  # rebuild from files already in data/raw
# Outputs:
#   data/processed/*.csv        tidy data behind every chart (published on the site)
#   data/processed/summary.json headline numbers + auto-written summary lines
#   data/archive/dams_weekly.csv  growing weekly dam history
#   outputs/social/*.png        each chart in story / square / wide / web sizes
#   outputs/canva_card.csv      one-row file for Canva Bulk Create
# Then run:  quarto render   (or quarto publish gh-pages)
# Sites and regions: config/sites.csv and config/regions.csv
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
# Offline: Rscript update_el_nino.R --offline, or options(sain.offline = TRUE) before source()
OFFLINE <- "--offline" %in% commandArgs(trailingOnly = TRUE) || isTRUE(getOption("sain.offline"))

suppressPackageStartupMessages({
  library(ggplot2)
})
for (f in c("R/theme_sain.R", "R/fetch_enso.R", "R/fetch_power.R", "R/fetch_chirps.R",
            "R/fetch_dws.R", "R/metrics_el_nino.R", "R/plots_el_nino.R")) source(f)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
# Files from the first (8-site) version, replaced by the region files below
unlink(c("data/processed/rain_cumulative_selected_seasons.csv",
         "data/processed/rain_normal_band_1991_2020.csv",
         "data/processed/heat_days_selected_seasons.csv"))

# 1. ENSO ---------------------------------------------------------------
roni <- fetch_roni(offline = OFFLINE)
events <- roni_el_nino_events(roni)
readr::write_csv(roni, "data/processed/roni.csv")
readr::write_csv(events, "data/processed/el_nino_events.csv")

# 2. Rain (CHIRPS) and heat (NASA POWER) ---------------------------------
sites <- read_sites()
regions <- read_regions()
power <- fetch_power_all(sites, offline = OFFLINE)
power <- trim_to_complete(power)
message("NASA POWER temperature complete for all sites to ", max(power$date))
# The first run downloads every CHIRPS pentad since 1981 (a few hours, can be
# resumed); after that only new pentads are fetched.
rain <- fetch_rain_chirps(sites, regions, offline = OFFLINE)

# Keep only sites and regions that have data this run (offline tests, failed downloads)
sites <- sites[sites$site %in% unique(power$site) & sites$site %in% unique(rain$site), ]
missing_regions <- setdiff(regions$region_id, sites$region_id)
if (length(missing_regions)) warning("No site data for region(s): ", paste(missing_regions, collapse = ", "))
regions <- regions[regions$region_id %in% sites$region_id, ]
message(sprintf("%d sites in %d regions", nrow(sites), nrow(regions)))

cum    <- rain_cumulative(rain, sites, regions)
band   <- rain_normal_band(cum)
sindex <- rain_summer_index(cum, regions)
thr    <- heat_thresholds(power)
heat   <- heat_days(power, thr, sites)
heat_nat <- heat_national(heat)
cur_season <- max(sindex$season)
cur_rain <- rain[rain$date >= as.Date(sprintf("%d-10-01", cur_season)), ]
rain_prelim_from <- if (any(cur_rain$prelim)) min(cur_rain$date[cur_rain$prelim]) else NA
message("CHIRPS rain to ", max(cur_rain$date),
        if (!is.na(rain_prelim_from)) paste0(" (preliminary from ", rain_prelim_from, ")") else "")

keep <- function(d, extra = NULL) d[d$season %in% c(cur_season, COMPARE_SEASONS, extra), ]
w_last <- max(cum$season[cum$region_id %in% regions$region_id[regions$season == "winter"]], -Inf)
readr::write_csv(keep(cum, w_last), "data/processed/rain_cumulative_regions.csv")
readr::write_csv(band, "data/processed/rain_normal_band_regions_1991_2020.csv")
readr::write_csv(sindex, "data/processed/rain_summer_index_all_seasons.csv")
readr::write_csv(keep(heat), "data/processed/heat_days_regions.csv")
readr::write_csv(heat_nat, "data/processed/heat_days_all_regions_all_seasons.csv")
readr::write_csv(sites, "data/processed/sites.csv")
readr::write_csv(regions, "data/processed/regions.csv")

# 3. Dams ----------------------------------------------------------------
dams <- tryCatch(fetch_dws_all(offline = OFFLINE), error = function(e) {
  warning("Dam data failed: ", conditionMessage(e)); NULL })
dam_sum <- NULL
if (!is.null(dams) && nrow(dams) > 0) {
  dam_sum <- summarise_dams(dams)
  readr::write_csv(dams, "data/processed/dams_latest.csv")
  readr::write_csv(dam_sum, "data/processed/dams_summary_latest.csv")
  archive <- update_dam_archive(dam_sum)
  dam_date <- dam_sum$report_date[dam_sum$region == "National"]
  if (as.Date(dam_date) < Sys.Date() - 8)
    warning("Dam figures are from the DWS report of ", dam_date, " (older than a week)")
}

# 4. Headline numbers ------------------------------------------------------
s <- season_summary(cum, sindex, heat, heat_nat, regions, sites, roni, events, dam_sum,
                    rain_prelim_from = rain_prelim_from)
summer_regs <- Filter(function(z) z$season_type == "summer", s$regions)
driest <- summer_regs[[which.min(vapply(summer_regs, `[[`, 0, "rain_pct_of_normal"))]]
wc <- Filter(function(z) z$season_type == "winter", s$regions)
s$lines <- c(
  sprintf("Summer rain since 1 Oct: %d%% of normal (to %s).", s$rain_pct_of_normal, fmt_date(s$as_of)),
  sprintf("Driest region: %s, %d%% of normal.", driest$region, driest$rain_pct_of_normal),
  sprintf("Hot days so far: %.1f vs a normal %.1f (to %s).", s$heat_days, s$heat_days_normal, fmt_date(s$heat_as_of)),
  sprintf("Same date in %s: %d%% of normal rain, %s hot days.", s$compare[[2]]$season,
          s$compare[[2]]$rain_pct_of_normal, s$compare[[2]]$heat_days),
  sprintf("El Nino index (RONI, %s): %+.2f.", s$roni_latest_season, s$roni_latest),
  if (!is.na(s$dams_national_pct)) sprintf("Dams: %s%% full (a year ago %s%%).",
                                           s$dams_national_pct, s$dams_national_pct_last_year),
  if (length(wc)) sprintf("Western Cape winter %s (Apr-Sep) rain: %d%% of normal.",
                          wc[[1]]$rain_season, wc[[1]]$rain_pct_of_normal)
)
jsonlite::write_json(s, "data/processed/summary.json", auto_unbox = TRUE, pretty = TRUE, digits = NA)
cat("\n", paste(s$lines, collapse = "\n"), "\n\n")

# 5. Charts in every format ----------------------------------------------
save_social(plot_rain_summer(sindex, s, zoom = TRUE), "elnino_rain_sofar")
save_social(plot_rain_summer(sindex, s, zoom = FALSE), "elnino_rain_season")
save_social(plot_rain_regions(s), "elnino_rain_regions")
save_social(plot_rain_region_panels(cum, band, s, regions, ncol = 3), "elnino_rain_region_panels",
            formats = c("wide", "web"))
save_social(plot_rain_region_panels(cum, band, s, regions, ncol = 2), "elnino_rain_region_panels",
            formats = c("story", "square"))
save_social(plot_heat(s), "elnino_heat")
save_social(plot_heat_regions(s), "elnino_heat_regions")
save_social(plot_roni_events(events), "elnino_strength")
if (!is.null(dam_sum)) save_social(plot_dams(dam_sum), "elnino_dams")

# 6. One-row card for Canva Bulk Create -------------------------------------
card <- data.frame(
  week_ending = s$as_of, season = s$season,
  rain_pct_of_normal = paste0(s$rain_pct_of_normal, "%"),
  heat_days = s$heat_days, heat_days_normal = s$heat_days_normal,
  roni = sprintf("%+.1f", s$roni_latest),
  dams_pct = if (!is.na(s$dams_national_pct)) paste0(s$dams_national_pct, "%") else "",
  dams_pct_last_year = if (!is.na(s$dams_national_pct_last_year)) paste0(s$dams_national_pct_last_year, "%") else ""
)
for (z in summer_regs) card[[paste0("rain_", tolower(z$region_id))]] <- paste0(z$rain_pct_of_normal, "%")
readr::write_csv(card, "outputs/canva_card.csv")
message("Done. Next: quarto render")
