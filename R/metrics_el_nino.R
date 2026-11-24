# ---------------------------------------------------------------------------
# Season metrics for the El Nino summer tracker, by climate region.
#
# Regions (config/regions.csv) group sites (config/sites.csv) that share a
# rainfall regime. Five summer-rainfall regions are tracked over the summer
# season; the Western Cape (winter rainfall) is tracked over its own winter
# season for rain, and over the summer season for heat.
#
# Normal = the 30 seasons starting 1991 to 2020 (WMO 1991-2020 period).
# Region index = mean of its sites, day by day (sites in a region share a
# rainfall regime, so averaging millimetres is reasonable).
# Rain comes from CHIRPS v3 pentads (spread evenly over their 5 days); heat
# from ERA5 daily maximum temperature (NASA POWER until 22 Nov 2026).
# Summer-rainfall index = mean over the five summer regions of "rain so far as a
# % of the region's normal full-season total", so each region counts equally
# and the wet east does not swamp the dry west.
# ---------------------------------------------------------------------------

SEASONS <- list(
  summer = list(start_month = 10, length = 212, label = "1 October to 30 April"),
  winter = list(start_month = 4,  length = 183, label = "1 April to 30 September")
)
NORMAL_SEASONS <- 1991:2020         # season start years
COMPARE_SEASONS <- c(1997, 2015)    # very strong El Nino summers: near-normal vs drought

season_label <- function(y, type = "summer") {
  if (type == "winter") as.character(y) else sprintf("%d/%02d", y, (y + 1) %% 100)
}

# Adds season (start year) and season_day; drops days outside the season and
# seasons that are not covered from their first day (e.g. 1980/81 here).
add_season <- function(df, type = "summer") {
  sp <- SEASONS[[type]]
  y <- as.integer(format(df$date, "%Y")); m <- as.integer(format(df$date, "%m"))
  df$season <- ifelse(m >= sp$start_month, y, y - 1L)
  df$season_day <- as.integer(df$date - as.Date(sprintf("%d-%02d-01", df$season, sp$start_month))) + 1L
  df <- df[df$season_day <= sp$length, ]
  starts <- unique(df$season[df$season_day == 1])
  df[df$season %in% starts, ]
}

with_regions <- function(power, sites) {
  merge(power, sites[, c("site", "region_id")], by = "site")
}

# --- Rainfall ----------------------------------------------------------------
# Cumulative regional rain through each season, in that region's own season.
rain_cumulative <- function(power, sites, regions) {
  p <- with_regions(power[!is.na(power$rain_mm), c("site", "date", "rain_mm")], sites)
  out <- lapply(seq_len(nrow(regions)), function(i) {
    r <- regions[i, ]
    d <- p[p$region_id == r$region_id, ]
    if (!nrow(d)) return(NULL)
    d <- add_season(d, r$season)
    idx <- stats::aggregate(rain_mm ~ season + season_day, data = d, FUN = mean)
    idx <- idx[order(idx$season, idx$season_day), ]
    idx$cum_mm <- stats::ave(idx$rain_mm, idx$season, FUN = cumsum)
    data.frame(region_id = r$region_id, idx)
  })
  do.call(rbind, out)
}

quantile_band <- function(d, value, group = "region_id") {
  n <- d[d$season %in% NORMAL_SEASONS, ]
  do.call(rbind, lapply(split(n, n[[group]]), function(x) {
    qs <- vapply(c(0.1, 0.25, 0.5, 0.75, 0.9), function(p)
      tapply(x[[value]], x$season_day, stats::quantile, p, names = FALSE), numeric(length(unique(x$season_day))))
    out <- data.frame(x[[group]][1], as.integer(rownames(qs)), qs)
    names(out) <- c(group, "season_day", "p10", "p25", "p50", "p75", "p90")
    out
  }))
}

rain_normal_band <- function(cum) quantile_band(cum, "cum_mm")

# Summer-rainfall index: each summer region's rain so far as a % of its normal
# (median) full-season total, averaged over regions with equal weight.
rain_summer_index <- function(cum, regions) {
  ids <- regions$region_id[regions$season == "summer"]
  d <- cum[cum$region_id %in% ids, ]
  L <- SEASONS$summer$length
  norm_total <- vapply(split(d, d$region_id), function(x)
    stats::median(x$cum_mm[x$season %in% NORMAL_SEASONS & x$season_day == L]), numeric(1))
  d$pct_season <- 100 * d$cum_mm / norm_total[d$region_id]
  idx <- stats::aggregate(pct_season ~ season + season_day, data = d, FUN = mean)
  n_reg <- stats::aggregate(region_id ~ season + season_day, data = d, FUN = length)
  idx <- merge(idx, n_reg, by = c("season", "season_day"))
  idx <- idx[idx$region_id == length(norm_total), c("season", "season_day", "pct_season")]
  idx$index <- "summer"
  idx[order(idx$season, idx$season_day), ]
}

# --- Heat: days above the local 90th percentile of daily max -----------------
heat_thresholds <- function(power, window = 7) {
  base <- power[as.integer(format(power$date, "%Y")) %in% NORMAL_SEASONS & !is.na(power$tmax_c), ]
  base$doy <- as.integer(format(base$date, "%j"))
  do.call(rbind, lapply(split(base, base$site), function(s) {
    thr <- vapply(1:366, function(d) {
      dist <- pmin(abs(s$doy - d), 366 - abs(s$doy - d))
      stats::quantile(s$tmax_c[dist <= window], 0.9, names = FALSE)
    }, numeric(1))
    data.frame(site = s$site[1], doy = 1:366, tmax_p90 = thr)
  }))
}

# Hot days through the summer season (1 Oct - 30 Apr) for every region,
# including the Western Cape: summer is its heat and fire season too.
heat_days <- function(power, thresholds, sites) {
  p <- add_season(with_regions(power[!is.na(power$tmax_c), ], sites), "summer")
  p$doy <- as.integer(format(p$date, "%j"))
  p <- merge(p, thresholds, by = c("site", "doy"))
  p$hot <- p$tmax_c > p$tmax_p90
  d <- stats::aggregate(hot ~ region_id + season + season_day, data = p, FUN = mean)
  d <- d[order(d$region_id, d$season, d$season_day), ]
  d$cum_hot_days <- stats::ave(d$hot, d$region_id, d$season, FUN = cumsum)
  d
}

# All-regions heat index: mean over regions (equal weight)
heat_national <- function(heat) {
  d <- stats::aggregate(cum_hot_days ~ season + season_day, data = heat, FUN = mean)
  d[order(d$season, d$season_day), ]
}

# --- Headline numbers ----------------------------------------------------------
# Position of value v among a reference set: rank (1 = lowest / highest)
rank_low  <- function(v, ref) 1L + sum(ref < v)
rank_high <- function(v, ref) 1L + sum(ref > v)

# Rain status for one region at a given season day (or at the season end)
region_rain_status <- function(cum, region_id, season, day) {
  d <- cum[cum$region_id == region_id & cum$season_day == day, ]
  cur <- d$cum_mm[d$season == season]
  normal <- d$cum_mm[d$season %in% NORMAL_SEASONS]
  others <- d$cum_mm[d$season != season]
  at <- function(s) { v <- d$cum_mm[d$season == s]; if (length(v)) round(v, 1) else NA }
  list(rain_mm = round(cur, 1),
       rain_normal_mm = round(stats::median(normal), 1),
       rain_pct_of_normal = round(100 * cur / stats::median(normal)),
       rain_percentile = round(100 * mean(normal < cur)),
       rain_rank_driest = rank_low(cur, others),
       n_seasons = length(others) + 1L,
       rain_mm_2015 = at(2015), rain_mm_1997 = at(1997))
}

season_summary <- function(cum, sindex, heat, heat_nat, regions, sites, roni, roni_events,
                           dams_summary = NULL, rain_prelim_from = NA) {
  L <- SEASONS$summer$length
  cur <- max(sindex$season)
  day <- max(sindex$season_day[sindex$season == cur])

  # Summer-rainfall index, expressed as % of normal to date
  si <- sindex[sindex$season_day == day, ]
  si_cur <- si$pct_season[si$season == cur]
  si_norm <- stats::median(si$pct_season[si$season %in% NORMAL_SEASONS])
  si_others <- si$pct_season[si$season != cur]
  pct_of_normal_at <- function(s) round(100 * si$pct_season[si$season == s] / si_norm)

  # Heat runs to the latest ERA5 day, which can differ from the rain date
  hday <- max(heat_nat$season_day[heat_nat$season == cur])
  hn <- heat_nat[heat_nat$season_day == hday, ]
  h_cur <- hn$cum_hot_days[hn$season == cur]

  cmp <- function(s) list(
    season = season_label(s),
    rain_pct_of_normal = pct_of_normal_at(s),
    heat_days = round(hn$cum_hot_days[hn$season == s], 1))

  # Regions
  reg <- lapply(seq_len(nrow(regions)), function(i) {
    r <- regions[i, ]
    n_sites <- sum(sites$region_id == r$region_id)
    hr <- heat[heat$region_id == r$region_id & heat$season_day == hday, ]
    heat_part <- list(
      heat_days = round(hr$cum_hot_days[hr$season == cur], 1),
      heat_days_normal = round(mean(hr$cum_hot_days[hr$season %in% NORMAL_SEASONS]), 1),
      heat_days_2015 = round(hr$cum_hot_days[hr$season == 2015], 1),
      heat_rank_hottest = rank_high(hr$cum_hot_days[hr$season == cur], hr$cum_hot_days[hr$season != cur]))
    if (r$season == "summer") {
      rain_part <- c(list(rain_period = sprintf("1 October to %s", fmt_date(as.Date(sprintf("%d-10-01", cur)) + day - 1)),
                          rain_season = season_label(cur), rain_complete = FALSE),
                     region_rain_status(cum, r$region_id, cur, day))
    } else {
      # Winter region: its most recent season, complete or so far
      rc <- cum[cum$region_id == r$region_id, ]
      ws <- max(rc$season)
      wday <- max(rc$season_day[rc$season == ws])
      rain_part <- c(list(rain_period = sprintf("1 April to %s", fmt_date(as.Date(sprintf("%d-04-01", ws)) + wday - 1)),
                          rain_season = season_label(ws, "winter"),
                          rain_complete = wday == SEASONS$winter$length),
                     region_rain_status(cum, r$region_id, ws, wday))
    }
    c(list(region_id = r$region_id, region = r$region, season_type = r$season, n_sites = n_sites),
      rain_part, heat_part)
  })

  rk <- roni_current_rank(roni_events)
  ev_cur <- roni_events[roni_events$ongoing, ]
  dams_nat <- if (!is.null(dams_summary)) dams_summary[dams_summary$region == "National", ] else NULL

  list(
    season = season_label(cur),
    as_of = as.character(as.Date(sprintf("%d-10-01", cur)) + day - 1),
    season_day = day,
    rain_prelim_from = if (is.na(rain_prelim_from)) NA else as.character(rain_prelim_from),
    heat_as_of = as.character(as.Date(sprintf("%d-10-01", cur)) + hday - 1),
    heat_season_day = hday,
    rain_pct_of_normal = round(100 * si_cur / si_norm),
    rain_pct_of_season = round(si_cur, 1),
    rain_pct_of_season_normal = round(si_norm, 1),
    rain_percentile_vs_1991_2020 = round(100 * mean(si$pct_season[si$season %in% NORMAL_SEASONS] < si_cur)),
    rain_rank_driest = rank_low(si_cur, si_others),
    n_seasons = length(si_others) + 1L,
    heat_days = round(h_cur, 1),
    heat_days_normal = round(mean(hn$cum_hot_days[hn$season %in% NORMAL_SEASONS]), 1),
    heat_rank_hottest = rank_high(h_cur, hn$cum_hot_days[hn$season != cur]),
    compare = lapply(COMPARE_SEASONS, cmp),
    regions = reg,
    roni_latest = utils::tail(roni$roni, 1),
    roni_latest_season = paste(utils::tail(roni$season, 1), utils::tail(roni$year, 1)),
    roni_peak_so_far = if (nrow(ev_cur)) ev_cur$peak else NA,
    roni_rank_among_events = rk$rank,
    roni_rank_tied = rk$tied,
    roni_n_events = rk$n,
    dams_national_pct = if (!is.null(dams_nat)) round(dams_nat$pct_this_week, 1) else NA,
    dams_national_pct_last_year = if (!is.null(dams_nat)) round(dams_nat$pct_last_year, 1) else NA,
    dams_report_date = if (!is.null(dams_nat)) as.character(dams_nat$report_date) else NA,
    dams_n = if (!is.null(dams_nat)) dams_nat$n_dams else NA
  )
}

fmt_date <- function(d) trimws(format(as.Date(d), "%e %B %Y"))
ordinal <- function(n) paste0(n, if (n %% 100 %in% 11:13) "th" else c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1])
