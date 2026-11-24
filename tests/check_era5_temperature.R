# ---------------------------------------------------------------------------
# Cross-check: is spring 2026 also the warmest since 1981 in ERA5?
#
# The temperature tracker uses NASA POWER (MERRA-2, with GEOS-IT for the latest
# weeks). Before calling 1 October to mid-November 2026 the warmest since 1981,
# this repeats the comparison in an independent reanalysis, ECMWF ERA5, and
# compares like with like: the same 30 points (every second point of the
# 1-degree grid, 2 degrees apart), the same dates each year, the same
# cos-latitude weights and the same 1991-2020 normal.
#
# ERA5 comes from the Open-Meteo historical API (free, no key; ~5 days behind
# real time). Open-Meteo's free tier is for non-commercial use, which covers a
# one-off check like this; if ERA5 ever becomes part of the published pipeline,
# take it from the Copernicus Climate Data Store instead. ERA5 data: Copernicus
# Climate Change Service, CC BY 4.0.
#
# Run (needs internet; ~10 minutes the first time, seconds after that):
#   source("tests/check_era5_temperature.R")      or   Rscript tests/check_era5_temperature.R
# Reruns use the saved downloads in data/raw/era5/ (not committed).
# Outputs: data/processed/era5_check_years.csv, outputs/era5_check.png and a
# verdict printed in the console.
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
OFFLINE <- "--offline" %in% commandArgs(trailingOnly = TRUE) || isTRUE(getOption("sain.offline"))

suppressPackageStartupMessages(library(ggplot2))
for (f in c("R/theme_sain.R", "R/fetch_power.R", "R/fetch_era5.R", "R/sa_temperature.R", "R/metrics_el_nino.R")) source(f)

WINDOW_START <- "10-01"
WINDOW_END   <- "11-15"            # ERA5 runs ~5 days behind; keep this fixed so reruns agree
YEARS        <- 1981:2026
NORMAL       <- 1991:2020
ERA5_DIR     <- "data/raw/era5"
ERA5_URL     <- "https://archive-api.open-meteo.com/v1/archive"

grid <- read_sa_grid()
pts <- grid[(round(grid$lat - 0.5) %% 2 == 0) & (round(grid$lon - 0.5) %% 2 == 0), ]
message(sprintf("Comparing %d of the %d grid points (2 degrees apart), %s to %s each year",
                nrow(pts), nrow(grid), WINDOW_START, WINDOW_END))

# --- ERA5: one request per year, all points at once ----------------------------
# Open-Meteo counts ~3 calls per point per request here (46 days), so a year is
# ~100 calls; 46 years ~4,600, under the free 5,000 an hour. Requests are paced
# and saved, and a refused request waits and retries.
era5_year <- function(yr) {
  dir.create(ERA5_DIR, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(ERA5_DIR, sprintf("era5_%d_%s_%s.json", yr, WINDOW_START, WINDOW_END))
  if (!file.exists(path) && !OFFLINE) {
    q <- list(latitude = paste(pts$lat, collapse = ","), longitude = paste(pts$lon, collapse = ","),
              start_date = sprintf("%d-%s", yr, WINDOW_START), end_date = sprintf("%d-%s", yr, WINDOW_END),
              daily = "temperature_2m_mean", models = "era5", timezone = "GMT")
    for (k in 1:4) {
      resp <- tryCatch(httr::GET(ERA5_URL, query = q, httr::timeout(120)), error = function(e) NULL)
      code <- if (is.null(resp)) NA else httr::status_code(resp)
      if (!is.na(code) && code == 200) { writeBin(httr::content(resp, as = "raw"), path); break }
      msg <- if (is.null(resp)) "no answer" else substr(httr::content(resp, as = "text", encoding = "UTF-8"), 1, 200)
      wait <- if (!is.na(code) && code == 429) 90 * k else 15 * k
      message(sprintf("ERA5 %d: HTTP %s (%s); waiting %d s", yr, code, msg, wait))
      if (k < 4) Sys.sleep(wait)
    }
    Sys.sleep(8)   # pacing: ~7 requests a minute
  }
  if (!file.exists(path)) { warning("ERA5 ", yr, ": no data"); return(NULL) }
  js <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!is.null(js$latitude)) js <- list(js)          # a single point comes back unwrapped
  do.call(rbind, lapply(seq_along(js), function(i) data.frame(
    point_id = pts$point_id[i],
    date = as.Date(unlist(js[[i]]$daily$time)),
    t_c = as.numeric(vapply(js[[i]]$daily$temperature_2m_mean, function(v) if (is.null(v)) NA_real_ else v, 0)))))
}
era5 <- do.call(rbind, lapply(YEARS, function(yr) { message("ERA5: ", yr); era5_year(yr) }))
if (is.null(era5)) stop("No ERA5 data downloaded")

# --- NASA POWER at the same points (saved downloads if present, else download) ---
power_points <- function(dir, start, end) {
  rows <- lapply(seq_len(nrow(pts)), function(i) {
    have <- file.exists(site_file(pts$point_id[i], dir))
    fetch_power_site(pts$point_id[i], pts$lat[i], pts$lon[i], start = start, end = end,
                     pars = "T2M", raw_dir = dir, offline = have || OFFLINE)
  })
  d <- do.call(rbind, rows)
  data.frame(point_id = d$site, date = d$date, t_c = d$T2M)
}
this_year <- as.integer(format(Sys.Date(), "%Y"))
power <- rbind(power_points("data/raw/power_grid/history", "19810101", sprintf("%d1231", this_year - 2)),
               power_points("data/raw/power_grid/recent", sprintf("%d0101", this_year - 1),
                            format(Sys.Date(), "%Y%m%d")))
power <- power[!duplicated(power[c("point_id", "date")], fromLast = TRUE), ]   # recent wins

# --- Same window, same weights, same normal ------------------------------------
window_means <- function(d, label, w = pts[c("point_id", "weight")]) {
  d <- d[!is.na(d$t_c), ]
  md <- format(d$date, "%m-%d")
  d <- d[md >= WINDOW_START & md <= WINDOW_END, ]
  d$year <- as.integer(format(d$date, "%Y"))
  d$w <- w$weight[match(d$point_id, w$point_id)]
  n_days <- as.integer(as.Date(paste0("2001-", WINDOW_END)) - as.Date(paste0("2001-", WINDOW_START))) + 1
  cover <- stats::aggregate(t_c ~ year, data = d, FUN = length)
  keep <- cover$year[cover$t_c >= 0.98 * n_days * nrow(w)]          # complete years only
  d <- d[d$year %in% keep, ]
  m <- stats::aggregate(cbind(wt = w * t_c, w = w) ~ year, data = d, FUN = sum)
  out <- data.frame(year = m$year, t_c = m$wt / m$w)
  out$anomaly <- out$t_c - mean(out$t_c[out$year %in% NORMAL])
  out$rank_warmest <- rank(-out$t_c, ties.method = "min")
  names(out)[-1] <- paste0(label, "_", names(out)[-1])
  out
}
yrs <- merge(window_means(era5, "era5"), window_means(power, "power"), by = "year", all = TRUE)

# (Until 22 Nov 2026 the tracker's own NASA POWER 114-point series was added here as a
# reference; the tracker now uses ERA5 itself, so that comparison no longer applies.)

yrs[] <- lapply(yrs, function(v) if (is.double(v)) round(v, 3) else v)
dir.create("data/processed", showWarnings = FALSE)
readr::write_csv(yrs, "data/processed/era5_check_years.csv")

# --- Verdict -------------------------------------------------------------------
both <- yrs[!is.na(yrs$era5_anomaly) & !is.na(yrs$power_anomaly), ]
r <- stats::cor(both$era5_anomaly, both$power_anomaly)
bias_recent <- with(both[both$year >= 2016, ], mean(power_anomaly - era5_anomaly))
top <- function(col) { o <- both[order(-both[[col]]), ]; o$year[1:3] }
cur <- both[both$year == max(both$year), ]
second_gap <- function(prefix) {
  v <- sort(both[[paste0(prefix, "_t_c")]], decreasing = TRUE); round(v[1] - v[2], 2)
}
cat("\n================ ERA5 check:", WINDOW_START, "to", WINDOW_END, "================\n")
cat(sprintf("Years compared: %d-%d (%d)\n", min(both$year), max(both$year), nrow(both)))
cat(sprintf("Agreement of yearly anomalies, ERA5 vs NASA POWER (same 30 points): r = %.2f\n", r))
cat(sprintf("POWER minus ERA5 anomaly, 2016 onward: %+.2f C a year on average\n", bias_recent))
cat(sprintf("%d: ERA5 %+.2f C (rank %d of %d); NASA POWER %+.2f C (rank %d)\n",
            cur$year, cur$era5_anomaly, cur$era5_rank_warmest, nrow(both), cur$power_anomaly,
            cur$power_rank_warmest))
cat(sprintf("Warmest three: ERA5 %s | POWER %s\n", paste(top("era5_t_c"), collapse = ", "),
            paste(top("power_t_c"), collapse = ", ")))
cat(sprintf("Margin of the warmest year over the next: ERA5 %.2f C, POWER %.2f C\n",
            second_gap("era5"), second_gap("power")))
verdict <- if (cur$era5_rank_warmest == 1 && cur$power_rank_warmest == 1) {
  "both datasets rank this window the warmest since 1981: the claim stands."
} else if (cur$era5_rank_warmest == cur$power_rank_warmest) {
  sprintf("both datasets rank this window %s, not the warmest: do not use 'warmest since 1981'.", ordinal(cur$era5_rank_warmest))
} else {
  sprintf("the datasets disagree (ERA5 %s, POWER %s): report both, or drop the ranking.",
          ordinal(cur$era5_rank_warmest), ordinal(cur$power_rank_warmest))
}
cat("VERDICT:", verdict, "\n")

# --- Chart -----------------------------------------------------------------------
long <- rbind(data.frame(year = both$year, anomaly = both$power_anomaly, set = "NASA POWER"),
              data.frame(year = both$year, anomaly = both$era5_anomaly, set = "ERA5 (ECMWF)"))   # ERA5 drawn on top
day_lab <- function(md) sub("^0", "", format(as.Date(paste0("2001-", md)), "%d %b"))
p <- ggplot(long, aes(year, anomaly, colour = set)) +
  geom_hline(yintercept = 0, colour = SAIN$axis, linewidth = 0.4) +
  geom_line(aes(linewidth = set)) + geom_point(size = 1.4) +
  scale_linewidth_manual(values = c("NASA POWER" = 1.1, "ERA5 (ECMWF)" = 0.6), guide = "none") +
  scale_colour_manual(values = c("ERA5 (ECMWF)" = SAIN$s2, "NASA POWER" = SAIN$s1)) +
  scale_y_continuous(labels = function(x) ifelse(abs(x) < 1e-9, "0\u00b0C", sprintf("%+.1f\u00b0C", x))) +
  labs(title = sprintf("%s to %s each year: ERA5 vs NASA POWER", day_lab(WINDOW_START), day_lab(WINDOW_END)),
       subtitle = sprintf("Mean temperature across South Africa vs the 1991\u20132020 average, same 30 points. r = %.2f", r),
       x = NULL, y = NULL,
       caption = sain_caption("ECMWF ERA5 via Open-Meteo (CC BY 4.0); NASA POWER (MERRA-2, GEOS-IT)")) +
  theme_sain()
dir.create("outputs", showWarnings = FALSE)
ggsave("outputs/era5_check.png", p, device = ragg::agg_png, width = 1400 / 150, height = 800 / 150,
       dpi = 150, bg = SAIN$surface)
message("Saved data/processed/era5_check_years.csv and outputs/era5_check.png")
