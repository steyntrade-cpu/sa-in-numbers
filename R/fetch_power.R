# ---------------------------------------------------------------------------
# Temperature (and, for comparison only, rainfall): NASA POWER daily point API
#   PRECTOTCORR = bias-corrected precipitation (mm/day)
#   T2M_MAX     = daily maximum temperature at 2 m (deg C)
# Gridded values (MERRA-2 grid, 0.5 x 0.625 degrees, roughly 50 x 60 km), not
# station observations: good for anomalies against its own climatology, weaker
# for absolute local totals.
# The record is MERRA-2. POWER appends GEOS-IT near-real-time values for the
# latest weeks and swaps them for MERRA-2 every few months, so the current
# season's numbers are provisional and can be revised.
# ---------------------------------------------------------------------------

POWER_URL <- "https://power.larc.nasa.gov/api/temporal/daily/point"

# Sites and regions live in config/ so they can be edited without touching
# code. Keep them fixed once the tracker is public, so the normals stay
# comparable. Every site must sit in its own POWER grid cell.
read_sites <- function(path = "config/sites.csv") {
  utils::read.csv(path, stringsAsFactors = FALSE)
}
read_regions <- function(path = "config/regions.csv") {
  utils::read.csv(path, stringsAsFactors = FALSE)
}

site_file <- function(site, raw_dir = "data/raw/power") {
  file.path(raw_dir, paste0(gsub("[^A-Za-z0-9]", "_", site), ".json"))
}

fetch_power_site <- function(site, lat, lon, start = "19810101",
                             end = format(Sys.Date(), "%Y%m%d"),
                             pars = c("PRECTOTCORR", "T2M_MAX"),
                             raw_dir = "data/raw/power", offline = FALSE, tries = 3) {
  path <- site_file(site, raw_dir)
  if (!offline) {
    dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
    url <- sprintf("%s?parameters=%s&community=AG&longitude=%.4f&latitude=%.4f&start=%s&end=%s&format=JSON",
                   POWER_URL, paste(pars, collapse = ","), lon, lat, start, end)
    ok <- FALSE
    for (k in seq_len(tries)) {
      resp <- tryCatch(httr::GET(url, httr::timeout(180)), error = function(e) NULL)
      if (!is.null(resp) && httr::status_code(resp) == 200) {
        writeBin(httr::content(resp, as = "raw"), path)
        ok <- TRUE
        break
      }
      Sys.sleep(5 * k)  # back off, then retry
    }
    if (!ok) warning("POWER ", site, ": download failed",
                     if (file.exists(path)) "; using the previous download" else "")
    Sys.sleep(1)  # be polite to the API
  }
  if (!file.exists(path)) {
    warning("No POWER data for ", site, ": site skipped")
    return(NULL)
  }
  parse_power(path, site)
}

parse_power <- function(path, site) {
  js <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  fill <- js$header$fill_value
  if (is.null(fill)) fill <- -999
  pars <- js$properties$parameter
  cols <- lapply(pars, function(p) {
    v <- as.numeric(unlist(p))
    v[v == fill] <- NA
    v
  })
  dates <- as.Date(names(pars[[1]]), format = "%Y%m%d")
  out <- data.frame(site = site, date = dates, cols, stringsAsFactors = FALSE)
  names(out) <- sub("^PRECTOTCORR$", "rain_mm", names(out))
  names(out) <- sub("^T2M_MAX$", "tmax_c", names(out))
  out
}

fetch_power_all <- function(sites = read_sites(), offline = FALSE, ...) {
  rows <- lapply(seq_len(nrow(sites)), function(i) {
    message("POWER: ", sites$site[i])
    fetch_power_site(sites$site[i], sites$lat[i], sites$lon[i], offline = offline, ...)
  })
  do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
}

# Last date for which every site has max temperature (rain now comes from
# CHIRPS). A site whose data stop more than `max_lag` days before the others
# (e.g. a failed download) is dropped for this run, with a warning, so one bad
# site can't freeze the whole tracker.
trim_to_complete <- function(power, max_lag = 14) {
  ok <- power[!is.na(power$tmax_c), ]
  last <- tapply(ok$date, ok$site, max)
  last <- as.Date(last, origin = "1970-01-01")
  lagging <- names(last)[last < max(last) - max_lag]
  if (length(lagging)) {
    warning("Dropped for this run (data more than ", max_lag, " days behind): ",
            paste(lagging, collapse = ", "))
    power <- power[!power$site %in% lagging, ]
    last <- last[!names(last) %in% lagging]
  }
  power[power$date <= min(last), ]
}
