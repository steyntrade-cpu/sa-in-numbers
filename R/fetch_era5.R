# ---------------------------------------------------------------------------
# ECMWF ERA5 daily temperature: reading the saved series
#
# The data are fetched by scripts/era5_arco.py (Python), which reads hourly
# 2 m temperature straight from ECMWF's analysis-ready (ARCO) Zarr store, with
# no request queue, and turns it into daily values over South African days
# (UTC+2):
#   data/era5/national_t2m_daily.csv    date, t_c, n_points
#       daily mean over all 1,810 ERA5 grid cells (0.25 degrees, ~25 km) inside
#       South Africa (config/era5_sa_cells.csv), weighted by cos(latitude)
#   data/era5/sites_tmax_daily.csv.gz   site, date, tmax_c
#       daily maximum at the grid point nearest each site, for the hot days
# The weekly GitHub workflow runs it before the R scripts and commits both
# files, so on your computer `git pull` brings the latest data; R itself never
# downloads ERA5.
#
# Why ERA5: NASA POWER switches model for its latest 2-3 months (MERRA-2 to
# GEOS-IT), which made October-November 2026 read about 1.2 C too warm against
# ERA5 (tests/check_era5_temperature.R). ERA5 is one system throughout; its
# latest ~2-3 months are "ERA5T" (preliminary) and rarely change by more than a
# few hundredths of a degree when finalised.
# (The first attempt used Copernicus's daily-statistics service; its queue took
# ~5 hours per year of data, so it was replaced on 24 Nov 2026.)
#
# Licence: CC BY 4.0. Attribution: "Contains modified Copernicus Climate Change
# Service information <year>".
# ---------------------------------------------------------------------------

ERA5_FIRST_YEAR <- 1981L
ERA5_NATIONAL   <- "data/era5/national_t2m_daily.csv"
ERA5_SITES      <- "data/era5/sites_tmax_daily.csv.gz"
ERA5_SOURCE     <- "ECMWF ERA5 via the Copernicus Climate Data Store (CC BY 4.0)"
ERA5_NOTE       <- "Reanalysis (~25 km grid), not weather-station records. Latest weeks preliminary (ERA5T)."

era5_cells <- function(path = "config/era5_sa_cells.csv") utils::read.csv(path)

era5_read_caches <- function() {
  if (!file.exists(ERA5_NATIONAL) || !file.exists(ERA5_SITES))
    stop("No ERA5 data in data/era5/ yet. It is written by scripts/era5_arco.py, which the weekly ",
         "GitHub workflow runs; on your computer, `git pull` to get it.", call. = FALSE)
  nat <- utils::read.csv(ERA5_NATIONAL)
  st <- utils::read.csv(gzfile(ERA5_SITES))
  nat$date <- as.Date(nat$date); st$date <- as.Date(st$date)
  list(national = nat[order(nat$date), ], sites = st[order(st$site, st$date), ])
}

# Returns list(national, sites, last_day). Same name and arguments as before;
# `offline` no longer matters because R never downloads ERA5.
era5_update <- function(sites, offline = FALSE, today = Sys.Date(), first_year = ERA5_FIRST_YEAR, ...) {
  cache <- era5_read_caches()
  yr <- function(d) as.integer(format(d, "%Y"))
  last_full <- yr(max(cache$national$date)) - 1L
  n_nat <- table(yr(cache$national$date)); n_st <- table(yr(cache$sites$date))
  missing <- Filter(function(y) {
    k <- as.character(y)
    is.na(n_nat[k]) || n_nat[k] < 365 || is.na(n_st[k]) || n_st[k] < 365 * nrow(sites)
  }, first_year:last_full)
  if (length(missing))
    stop(sprintf("ERA5 data incomplete: %d year(s) missing or short (%s). Run scripts/era5_arco.py.",
                 length(missing), paste(range(missing), collapse = "-")), call. = FALSE)
  unknown <- setdiff(sites$site, unique(cache$sites$site))
  if (length(unknown))
    stop("No ERA5 maximum temperatures for site(s): ", paste(unknown, collapse = ", "),
         ". Run scripts/era5_arco.py --start ", first_year, "-01-01", call. = FALSE)
  lag <- as.integer(today - max(cache$national$date))
  if (lag > 14) warning("ERA5 data end on ", max(cache$national$date), " (", lag,
                        " days ago); run scripts/era5_arco.py or git pull", call. = FALSE)
  message("ERA5 to ", max(cache$national$date), " (national), ", max(cache$sites$date), " (sites)")
  list(national = cache$national, sites = cache$sites[cache$sites$site %in% sites$site, ],
       last_day = max(cache$national$date))
}
