# ---------------------------------------------------------------------------
# Rainfall: CHIRPS v3 (Climate Hazards Center, UC Santa Barbara)
#   Satellite rainfall estimates blended with rain-gauge records, 0.05 degree
#   (~5 km), 1981 to present, built as 5-day "pentads" (days 1-5, 6-10,
#   11-15, 16-20, 21-25, 26-end of month) and months.
#   Final pentads and months: about three weeks after the month ends.
#   Preliminary pentads: 2 days after each pentad ends.
#
# Values are the mean of a 5 x 5 pixel window (0.25 degrees) around each site,
# read from the CHC server without downloading whole files where possible.
# Everything read is cached in data/chirps/ (committed to the repository), so
# each run only fetches new pentads, and swaps preliminary for final values
# once the final files appear.
#
# Confirmed layout (21 Nov 2026):
#   final pentads   pentads/africa/tifs/chirps-v3.0.YYYY.MM.P.tif
#   prelim pentads  prelim/pentads/africa/tifs/chirps-v3.0.YYYY.MM.P.tif
#   final months    monthly/{africa|global}/tifs/chirps-v3.0.YYYY.MM.tif
# The server rate-limits (HTTP 429): requests are paced, and retried after a
# wait when the server is busy.
# ---------------------------------------------------------------------------

CHIRPS_ROOT <- Sys.getenv("CHIRPS_ROOT", "https://data.chc.ucsb.edu/products/CHIRPS/v3.0/")
CHIRPS_UA_STRING <- "SA-in-Numbers/1.0 (R; climate tracker)"
CHIRPS_UA <- httr::user_agent(CHIRPS_UA_STRING)
CHIRPS_CACHE <- "data/chirps/chirps_sites.csv"
SA_EXTENT <- c(16, 33.5, -35.5, -21.5)   # xmin, xmax, ymin, ymax

# {Y} year, {M} two-digit month, {P} pentad 1-6. First one that exists is used.
CHIRPS_TEMPLATES <- list(
  pentad = c("pentads/africa/tifs/chirps-v3.0.{Y}.{M}.{P}.tif",
             "pentads/global/tifs/chirps-v3.0.{Y}.{M}.{P}.tif",
             "pentads/africa/tifs/chirps-v3.0.pentad.{Y}.{M}.{P}.tif"),
  pentad_prelim = c("prelim/pentads/africa/tifs/chirps-v3.0.{Y}.{M}.{P}.tif",
                    "prelim/pentads/global/tifs/chirps-v3.0.{Y}.{M}.{P}.tif"),
  month = c("monthly/africa/tifs/chirps-v3.0.{Y}.{M}.tif",
            "monthly/global/tifs/chirps-v3.0.{Y}.{M}.tif"))

.chirps_log <- new.env()

# --- HTTP ------------------------------------------------------------------------
# Request that waits and retries when the server is busy (429, 5xx)
chc_request <- function(url, method = "GET", ..., tries = 5) {
  r <- NULL
  no_answer <- 0
  for (k in seq_len(tries)) {
    r <- tryCatch(if (method == "HEAD") httr::HEAD(url, CHIRPS_UA, httr::timeout(60), ...)
                  else httr::GET(url, CHIRPS_UA, httr::timeout(300), ...), error = function(e) NULL)
    st <- if (is.null(r)) NA else httr::status_code(r)
    if (!is.na(st) && !st %in% c(429, 500, 502, 503, 504)) return(r)
    if (is.na(st)) no_answer <- no_answer + 1
    if (k == tries || no_answer >= 2) break   # no connection at all: don't keep waiting
    wait <- if (!is.null(r)) suppressWarnings(as.numeric(httr::headers(r)[["retry-after"]])) else NA
    if (length(wait) != 1 || is.na(wait)) wait <- 15 * 2^(k - 1)
    wait <- if (is.na(st)) 10 else min(wait, 120)
    message(sprintf("CHIRPS server busy (%s); waiting %d s", if (is.na(st)) "no answer" else st, round(wait)))
    Sys.sleep(wait)
  }
  r
}

url_exists <- function(url) {
  r <- chc_request(url, "HEAD")
  st <- if (is.null(r)) NA else httr::status_code(r)
  if (!is.na(st) && st %in% c(403, 405, 501)) {   # HEAD not allowed: ask for the first byte instead
    r <- chc_request(url, "GET", httr::add_headers(Range = "bytes=0-0"))
    st <- if (is.null(r)) NA else httr::status_code(r)
  }
  ok <- !is.na(st) && st %in% c(200, 206)
  assign(url, if (ok) "exists" else paste("not found", st), envir = .chirps_log)
  ok
}

write_chirps_log <- function(path = "data/raw/chirps/listing.txt") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  keys <- ls(.chirps_log)
  writeLines(unlist(lapply(keys, function(k) c(paste0("== ", k), get(k, envir = .chirps_log), ""))), path)
}

# --- File addresses --------------------------------------------------------------
chirps_file <- function(tpl, y, m, p = 1) {
  vapply(seq_along(y), function(i) {
    out <- gsub("{Y}", y[i], tpl, fixed = TRUE)
    out <- gsub("{M}", sprintf("%02d", as.integer(m[i])), out, fixed = TRUE)
    gsub("{P}", p[i], out, fixed = TRUE)
  }, character(1))
}

# The template that works, tested on a few reference dates
resolve_template <- function(templates, ref) {
  for (tpl in templates) for (i in seq_len(nrow(ref))) {
    if (url_exists(paste0(CHIRPS_ROOT, chirps_file(tpl, ref$year[i], ref$month[i], ref$pentad[i])))) return(tpl)
  }
  NA_character_
}

pentad_dates <- function(y, m, p) {
  start <- as.Date(sprintf("%d-%02d-%02d", y, m, (p - 1L) * 5L + 1L))
  month_end <- as.Date(vapply(seq_along(y), function(i)
    seq(as.Date(sprintf("%d-%02d-01", y[i], m[i])), by = "month", length.out = 2)[2] - 1, numeric(1)),
    origin = "1970-01-01")
  end <- ifelse(p < 6, start + 4, month_end)
  list(start = start, end = as.Date(end, origin = "1970-01-01"))
}

# Pentads that ended at least `lag` days before `today`, most recent first
recent_pentads <- function(today = Sys.Date(), n = 6, lag = 2) {
  d <- seq(today - 60, today, by = "day")
  key <- unique(data.frame(year = as.integer(format(d, "%Y")), month = as.integer(format(d, "%m")),
                           pentad = pmin(6L, (as.integer(format(d, "%d")) - 1L) %/% 5L + 1L)))
  key <- key[pentad_dates(key$year, key$month, key$pentad)$end <= today - lag, ]
  utils::head(key[nrow(key):1, ], n)
}

# --- Reading ------------------------------------------------------------------------
site_boxes <- function(sites, half = 0.125) {
  polys <- lapply(seq_len(nrow(sites)), function(i) terra::as.polygons(
    terra::ext(sites$lon[i] - half, sites$lon[i] + half, sites$lat[i] - half, sites$lat[i] + half),
    crs = "EPSG:4326"))
  do.call(rbind, polys)
}

# Mean rain (mm) in each site's window, from one CHIRPS file
read_chirps_sites <- function(url, boxes) {
  gz <- grepl("\\.gz$", url)
  download_local <- function() {
    tf <- tempfile(fileext = if (gz) ".tif.gz" else ".tif")
    r <- chc_request(url, "GET", httr::write_disk(tf, overwrite = TRUE))
    if (is.null(r) || httr::status_code(r) != 200)
      stop("download failed (", if (is.null(r)) "no answer" else httr::status_code(r), ")")
    if (gz) paste0("/vsigzip/", tf) else tf
  }
  # Read only the needed part over HTTP; if that fails, download the whole file
  src <- if (gz) download_local() else paste0("/vsicurl/", url)
  r <- tryCatch(terra::crop(terra::rast(src), terra::ext(SA_EXTENT)),
                error = function(e) if (gz) stop(e) else {
                  assign(url, paste("partial read failed:", conditionMessage(e)), envir = .chirps_log)
                  terra::crop(terra::rast(download_local()), terra::ext(SA_EXTENT))
                })
  r <- terra::classify(r, cbind(-Inf, 0, NA), right = FALSE)   # nodata (-9999) and negatives
  terra::extract(r, boxes, fun = mean, na.rm = TRUE, ID = FALSE)[[1]]
}

read_chirps_cache <- function(path = CHIRPS_CACHE) {
  if (file.exists(path)) return(utils::read.csv(path, stringsAsFactors = FALSE))
  # carry over values downloaded by the first comparison script
  old <- "data/raw/chirps/pentads_sites.csv"
  if (file.exists(old)) {
    x <- utils::read.csv(old, stringsAsFactors = FALSE)
    x$step <- "pentad"
    x$file <- sub(CHIRPS_ROOT, "", x$url, fixed = TRUE)
    return(x[, c("step", "year", "month", "pentad", "prelim", "site", "rain_mm", "file")])
  }
  NULL
}

write_chirps_cache <- function(x, path = CHIRPS_CACHE) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  x <- x[order(x$step, x$year, x$month, x$pentad, x$prelim, x$site), ]
  x <- x[!duplicated(x[, c("step", "year", "month", "pentad", "site")]), ]
  x$rain_mm <- round(x$rain_mm, 3)
  utils::write.csv(x, path, row.names = FALSE)
  x
}

# --- Fetch -------------------------------------------------------------------------
# need: data.frame(year, month, pentad) (pentad = 0 for step "month").
# Returns cached + new values for these sites: step, year, month, pentad,
# prelim, site, rain_mm, file.
fetch_chirps_values <- function(sites, need, step = c("pentad", "month"), offline = FALSE) {
  step <- match.arg(step)
  empty <- data.frame(step = character(), year = integer(), month = integer(), pentad = integer(),
                      prelim = logical(), site = character(), rain_mm = numeric(), file = character())
  cache <- read_chirps_cache()
  if (is.null(cache)) cache <- empty
  key <- function(d) paste(d$year, d$month, d$pentad)
  sel <- function(x) x[x$step == step & x$site %in% sites$site, ]
  have <- sel(cache)
  if (offline || !nrow(need)) return(have[key(have) %in% key(need), ])

  terra::setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
  terra::setGDALconfig("GDAL_HTTP_USERAGENT", CHIRPS_UA_STRING)
  terra::setGDALconfig("CPL_VSIL_CURL_CHUNK_SIZE", "2097152")       # fewer, larger requests
  terra::setGDALconfig("GDAL_HTTP_MERGE_CONSECUTIVE_RANGES", "YES")
  terra::setGDALconfig("GDAL_HTTP_MAX_RETRY", "3")                  # wait and retry when busy
  terra::setGDALconfig("GDAL_HTTP_RETRY_DELAY", "10")

  # A pentad/month is done when every site has a final value for it
  fin <- have[!have$prelim, ]
  n_fin <- if (nrow(fin)) table(key(fin)) else integer()
  done <- names(n_fin)[n_fin >= nrow(sites)]
  todo <- need[!key(need) %in% done, ]
  if (!nrow(todo)) return(have[key(have) %in% key(need), ])

  message(sprintf("CHIRPS (%s): %d to fetch; looking for files on %s", step, nrow(todo), CHIRPS_ROOT))
  root <- chc_request(CHIRPS_ROOT, "HEAD", tries = 2)
  if (is.null(root) || httr::status_code(root) >= 500) {
    warning("CHIRPS server not reachable; using the values already downloaded")
    return(have[key(have) %in% key(need), ])
  }
  if (step == "pentad") {
    final_tpl <- resolve_template(CHIRPS_TEMPLATES$pentad, data.frame(year = c(2020, 2010), month = 10, pentad = 1))
    prelim_tpl <- resolve_template(CHIRPS_TEMPLATES$pentad_prelim, recent_pentads())
  } else {
    final_tpl <- resolve_template(CHIRPS_TEMPLATES$month, data.frame(year = c(2020, 2010), month = 6, pentad = 0))
    prelim_tpl <- NA_character_
  }
  message("  final: ", final_tpl, " | preliminary: ", prelim_tpl)
  if (is.na(final_tpl)) {
    write_chirps_log()
    warning("CHIRPS ", step, " files not found; see data/raw/chirps/listing.txt. Using cached values only.")
    return(have[key(have) %in% key(need), ])
  }

  boxes <- site_boxes(sites)
  cached_prelim <- if (nrow(have)) unique(paste(key(have[have$prelim, ]), have$file[have$prelim])) else character()
  # files for recent months may not exist yet: check before reading
  recent <- as.Date(sprintf("%d-%02d-01", todo$year, todo$month)) >= Sys.Date() - 75
  new <- list()
  for (i in seq_len(nrow(todo))) {
    f <- todo[i, ]
    final_file <- chirps_file(final_tpl, f$year, f$month, f$pentad)
    use <- NULL
    if (!recent[i] || url_exists(paste0(CHIRPS_ROOT, final_file))) {
      use <- list(file = final_file, prelim = FALSE)
    } else if (!is.na(prelim_tpl)) {
      use <- list(file = chirps_file(prelim_tpl, f$year, f$month, f$pentad), prelim = TRUE)
      if (paste(key(f), use$file) %in% cached_prelim) next   # same preliminary file already read
      if (!url_exists(paste0(CHIRPS_ROOT, use$file))) next  # not published yet
    } else next
    message(sprintf("CHIRPS %d/%d: %d-%02d%s%s", i, nrow(todo), f$year, f$month,
                    if (step == "pentad") paste0(" pentad ", f$pentad) else "",
                    if (use$prelim) " (preliminary)" else ""))
    vals <- tryCatch(read_chirps_sites(paste0(CHIRPS_ROOT, use$file), boxes), error = function(e) NULL)
    Sys.sleep(1.5)   # pace requests so the server does not throttle us
    if (is.null(vals)) { warning("CHIRPS: could not read ", use$file); next }
    new[[length(new) + 1]] <- data.frame(step = step, year = f$year, month = f$month, pentad = f$pentad,
                                         prelim = use$prelim, site = sites$site, rain_mm = vals,
                                         file = use$file, stringsAsFactors = FALSE)
    if (length(new) %% 25 == 0) cache <- write_chirps_cache(rbind(cache, do.call(rbind, new)))  # resumable
  }
  if (length(new)) cache <- write_chirps_cache(rbind(cache, do.call(rbind, new)))
  write_chirps_log()
  have <- sel(cache)
  out <- have[key(have) %in% key(need), ]
  missing <- need[!key(need) %in% key(out), ]
  if (nrow(missing)) message(sprintf("CHIRPS (%s): %d not available yet, e.g. %s", step, nrow(missing),
                                     paste(utils::head(key(missing), 3), collapse = "; ")))
  out
}

# Spread pentad / monthly totals evenly over their days, so the daily
# season code can be used. Cumulative totals are exact at pentad ends.
chirps_to_daily <- function(v) {
  if (is.null(v) || !nrow(v)) return(NULL)
  is_p <- v$step == "pentad"
  start <- as.Date(sprintf("%d-%02d-01", v$year, v$month))
  end <- as.Date(vapply(seq_len(nrow(v)), function(i)
    seq(start[i], by = "month", length.out = 2)[2] - 1, numeric(1)), origin = "1970-01-01")
  if (any(is_p)) {
    pd <- pentad_dates(v$year[is_p], v$month[is_p], v$pentad[is_p])
    start[is_p] <- pd$start; end[is_p] <- pd$end
  }
  n <- as.integer(end - start) + 1L
  idx <- rep(seq_len(nrow(v)), n)
  data.frame(site = v$site[idx],
             date = start[idx] + sequence(n) - 1L,
             rain_mm = (v$rain_mm / n)[idx],
             prelim = v$prelim[idx], stringsAsFactors = FALSE)
}

# Everything the tracker needs: pentads (1 Oct - 30 Apr) for summer-rainfall
# sites since 1981/82; months (Apr - Sep) for winter-rainfall sites.
fetch_rain_chirps <- function(sites, regions, offline = FALSE, first_season = 1981, today = Sys.Date()) {
  season_of <- stats::setNames(regions$season, regions$region_id)
  s_sites <- sites[season_of[sites$region_id] == "summer", ]
  w_sites <- sites[season_of[sites$region_id] == "winter", ]
  cur <- as.integer(format(today, "%Y")) - (as.integer(format(today, "%m")) < 10)

  # summer pentads that have ended (preliminary data appear 2 days later)
  ps <- merge(data.frame(season = first_season:cur),
              data.frame(m = c(10:12, 1:4), off = c(0, 0, 0, 1, 1, 1, 1)))
  ps <- merge(data.frame(year = ps$season + ps$off, month = ps$m), data.frame(pentad = 1:6))
  ps <- ps[pentad_dates(ps$year, ps$month, ps$pentad)$end <= today - 2, ]
  # winter months that have ended
  wm <- merge(data.frame(year = first_season:as.integer(format(today, "%Y"))), data.frame(month = 4:9))
  wm$pentad <- 0L
  wm <- wm[as.Date(sprintf("%d-%02d-01", wm$year, wm$month)) + 31 <= today, ]

  out <- list()
  if (nrow(s_sites)) out$s <- fetch_chirps_values(s_sites, ps, "pentad", offline)
  if (nrow(w_sites)) out$w <- fetch_chirps_values(w_sites, wm, "month", offline)
  do.call(rbind, lapply(out, chirps_to_daily))
}
