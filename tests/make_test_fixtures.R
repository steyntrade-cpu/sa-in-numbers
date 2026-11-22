# ---------------------------------------------------------------------------
# Builds SYNTHETIC raw files in the exact formats the real sources use, so the
# whole pipeline can be tested without internet:
#   Rscript tests/make_test_fixtures.R && Rscript update_el_nino.R --offline
# The numbers are fake. Never publish charts made from these files.
# Delete data/raw/ afterwards and run update_el_nino.R online for real data.
# ---------------------------------------------------------------------------
set.seed(42)
source("R/fetch_power.R")
source("R/fetch_dws.R")
end_date <- as.Date("2026-11-16")

# NASA POWER JSON, one file per site
dir.create("data/raw/power", recursive = TRUE, showWarnings = FALSE)
dates <- seq(as.Date("1981-01-01"), end_date, by = "day")
doy <- as.integer(format(dates, "%j"))
sites <- read_sites()
regions <- read_regions()
for (i in seq_len(nrow(sites))) {
  winter <- regions$season[regions$region_id == sites$region_id[i]] == "winter"
  peak <- if (winter) 196 else 15                                 # July or January peak
  seasonal <- pmax(0, cos(2 * pi * (doy - peak) / 365))
  wet <- runif(length(dates)) < (0.08 + 0.35 * seasonal)
  rain <- ifelse(wet, rgamma(length(dates), 1.1, 1 / (6 + 6 * seasonal)), 0)
  tmax <- 22 + 7 * cos(2 * pi * (doy - 20) / 365) + rnorm(length(dates), 0, 3)
  # make the synthetic current season drier and hotter
  cur <- dates >= as.Date("2026-10-01")
  rain[cur] <- rain[cur] * 0.6; tmax[cur] <- tmax[cur] + 1.5
  rain[length(rain)] <- -999                                    # mimic NRT lag
  js <- list(
    header = list(fill_value = -999),
    properties = list(parameter = list(
      PRECTOTCORR = as.list(setNames(round(rain, 2), format(dates, "%Y%m%d"))),
      T2M_MAX = as.list(setNames(round(tmax, 2), format(dates, "%Y%m%d"))))))
  jsonlite::write_json(js, site_file(sites$site[i]),
                       auto_unbox = TRUE, digits = NA)
}

# NOAA RONI text file
seas <- c("DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ")
grid <- expand.grid(season = seas, year = 1950:2026, stringsAsFactors = FALSE)
grid <- grid[order(grid$year, match(grid$season, seas)), ]
grid <- grid[!(grid$year == 2026 & match(grid$season, seas) > 9), ]   # to ASO 2026
t <- seq_len(nrow(grid))
grid$roni <- round(0.9 * sin(2 * pi * t / 47) + rnorm(nrow(grid), 0, 0.3), 2)
grid$roni[grid$year == 1997 & match(grid$season, seas) >= 5] <- c(1.2, 1.6, 2.0, 2.3, 2.5, 2.6, 2.6, 2.5)
grid$roni[grid$year == 2015 & match(grid$season, seas) >= 5] <- c(1.0, 1.3, 1.6, 1.9, 2.1, 2.3, 2.4, 2.4)
grid$roni[grid$year == 2026 & match(grid$season, seas) >= 4] <- c(0.6, 0.9, 1.3, 1.7, 2.0, 2.2)
lines <- c("SEAS  YR   RONI", sprintf("%4s %5d %7.2f", grid$season, grid$year, grid$roni))
writeLines(lines, "data/raw/RONI.ascii.txt")

# DWS drainage-region HTML pages (a few regions)
dir.create("data/raw/dws", recursive = TRUE, showWarnings = FALSE)
mk_region <- function(r, n) {
  rows <- vapply(seq_len(n), function(k) {
    fsc <- round(runif(1, 5, 2500), 1); tw <- round(runif(1, 55, 102), 1)
    sprintf("<tr><td>%s Dam %d</td><td>River %d</td><td>Photo</td><td>Indicators</td><td>%.1f</td><td>%.1f</td><td>%.1f</td><td>%.1f</td></tr>",
            r, k, k, fsc, tw, tw + runif(1, 0, 1.5), min(105, tw + runif(1, -8, 12)))
  }, "")
  # DWS ends each table with a "Total" row (capacity-weighted); the parser must drop it
  html <- paste0("<html><body><h3>", r, " Drainage Region State of Dams on 2026-11-16 |</h3>",
                 "<table><tr><th>Dam</th><th>River</th><th>Photo</th><th>Indicators</th><th>FSC</th>",
                 "<th>This Week</th><th>Last Week</th><th>Last Year</th></tr>",
                 paste(rows, collapse = ""), "</table></body></html>")
  writeLines(html, file.path("data/raw/dws", paste0("region_", r, ".html")))
  x <- parse_dws_region(file.path("data/raw/dws", paste0("region_", r, ".html")), r)
  tot <- sprintf("<tr><td>Total</td><td></td><td></td><td></td><td>%.1f</td><td>%.1f</td><td>%.1f</td><td>%.1f</td></tr>",
                 sum(x$fsc_mcm), weighted_storage(x$fsc_mcm, x$pct_this_week),
                 weighted_storage(x$fsc_mcm, x$pct_last_week), weighted_storage(x$fsc_mcm, x$pct_last_year))
  html <- sub("</table>", paste0(tot, "</table>"), html, fixed = TRUE)
  writeLines(html, file.path("data/raw/dws", paste0("region_", r, ".html")))
}
for (r in c("A", "B", "C", "D", "G", "H", "U", "V", "X")) mk_region(r, sample(4:9, 1))
writeLines("<html><body>No data available in F Drainage Region on 2026-11-16</body></html>",
           "data/raw/dws/region_F.html")
message("Synthetic fixtures written to data/raw/ - FAKE DATA, for testing only.")
