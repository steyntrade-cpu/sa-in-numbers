# ---------------------------------------------------------------------------
# ENSO: NOAA CPC Relative Oceanic Nino Index (RONI)
# Since 1 Feb 2026 CPC uses RONI as its official ENSO index.
# File format: header line, then three columns: Season (e.g. "DJF"), Year, RONI
# ---------------------------------------------------------------------------

RONI_URL <- "https://www.cpc.ncep.noaa.gov/data/indices/RONI.ascii.txt"

# Centre month of each 3-month season, used to place values on a time axis
SEASON_CENTRE_MONTH <- c(DJF = 1, JFM = 2, FMA = 3, MAM = 4, AMJ = 5, MJJ = 6,
                         JJA = 7, JAS = 8, ASO = 9, SON = 10, OND = 11, NDJ = 12)

fetch_roni <- function(raw_dir = "data/raw", offline = FALSE) {
  path <- file.path(raw_dir, "RONI.ascii.txt")
  if (!offline) {
    dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
    utils::download.file(RONI_URL, path, quiet = TRUE, mode = "wb")
  }
  if (!file.exists(path)) stop("No RONI file at ", path, " (run once with offline = FALSE)")
  parse_roni(path)
}

parse_roni <- function(path) {
  x <- utils::read.table(path, skip = 1, header = FALSE, stringsAsFactors = FALSE,
                         col.names = c("season", "year", "roni"))
  x$season <- toupper(trimws(x$season))
  x <- x[x$season %in% names(SEASON_CENTRE_MONTH) & !is.na(x$roni), ]
  x$month  <- unname(SEASON_CENTRE_MONTH[x$season])
  x$date   <- as.Date(sprintf("%d-%02d-15", x$year, x$month))
  x <- x[order(x$date), c("date", "year", "season", "roni")]
  rownames(x) <- NULL
  x
}

# Identify El Nino events using the CPC rule: at least 5 consecutive
# overlapping seasons with RONI >= +0.5. Returns one row per event with its
# peak, the season it peaked, and a "1997/98"-style label for the Southern
# Hemisphere summer it affected.
roni_el_nino_events <- function(roni, threshold = 0.5, min_run = 5) {
  warm <- roni$roni >= threshold
  r <- rle(warm)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  keep <- which(r$values & r$lengths >= min_run)
  # A run that touches the last row is still under way; keep it even if short,
  # flagged as ongoing, so the current event can be compared "so far".
  last_run <- length(r$lengths)
  if (r$values[last_run] && !(last_run %in% keep)) keep <- c(keep, last_run)

  out <- lapply(keep, function(i) {
    seg <- roni[starts[i]:ends[i], ]
    pk <- seg[which.max(seg$roni), ]
    # Southern-hemisphere summer label ("1997/98"): the summer (Nov-Feb) that
    # holds most of the event's months; on a tie, the summer of the peak (or the
    # earlier one). An event with no Nov-Feb months yet (still building in
    # September) gets the summer that follows its last month.
    summer_year <- function(d) {
      y <- as.integer(format(d, "%Y"))
      ifelse(as.integer(format(d, "%m")) >= 7, y, y - 1L)
    }
    m <- as.integer(format(seg$date, "%m"))
    in_summer <- m %in% c(11, 12, 1, 2)
    if (any(in_summer)) {
      tab <- table(summer_year(seg$date[in_summer]))
      cand <- as.integer(names(tab)[tab == max(tab)])
      yr0 <- if (summer_year(pk$date) %in% cand) summer_year(pk$date) else min(cand)
    } else {
      yr0 <- summer_year(seg$date[nrow(seg)])
    }
    data.frame(
      start = seg$date[1], end = seg$date[nrow(seg)],
      peak_date = pk$date, peak = pk$roni,
      label = sprintf("%d/%02d", yr0, (yr0 + 1) %% 100),
      ongoing = ends[i] == nrow(roni),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

# Rank of the current (ongoing) event among past events by peak, with ties
# reported (values are given to 2 decimals, so equal at 2 dp = tied).
roni_current_rank <- function(events) {
  cur <- events[events$ongoing, ]
  past <- events[!events$ongoing, ]
  if (!nrow(cur)) return(list(rank = NA, tied = FALSE, n = nrow(events)))
  list(rank = 1L + sum(round(past$peak, 2) > round(cur$peak, 2)),
       tied = any(round(past$peak, 2) == round(cur$peak, 2)),
       n = nrow(events))
}
