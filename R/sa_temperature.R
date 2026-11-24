# ---------------------------------------------------------------------------
# South Africa's daily temperature, every year since 1981
#
# Daily mean air temperature at 2 m from ECMWF ERA5 (Copernicus Climate Data
# Store), averaged over all 1,810 ERA5 grid cells (0.25 degrees, ~25 km) inside
# South Africa, weighted by cos(latitude) so each cell counts by its area
# (config/era5_sa_cells.csv; Lesotho and Eswatini excluded). Days run midnight
# to midnight South African time. Reanalysis data, not weather-station records;
# the latest ~2-3 months are ERA5T (preliminary).
#
# Until 22 November 2026 this used NASA POWER at 114 points; NASA POWER's
# latest months come from a different model (GEOS-IT) and read about 1.2 C too
# warm against ERA5 for October-November 2026 (tests/check_era5_temperature.R).
# Downloading and caching: R/fetch_era5.R.
# ---------------------------------------------------------------------------

SA_GRID_FILE <- "config/sa_grid.csv"          # the old 1-degree points (used by the ERA5 check)
SA_TEMP_FIRST_YEAR <- 1981

read_sa_grid <- function(path = SA_GRID_FILE) utils::read.csv(path, stringsAsFactors = FALSE)

# The national daily series (date, t_c, n_points) from ERA5
sa_temperature_series <- function(offline = FALSE, today = Sys.Date(), sites = read_sites()) {
  d <- era5_update(sites, offline = offline, today = today)$national
  d <- d[d$date >= as.Date(sprintf("%d-01-01", SA_TEMP_FIRST_YEAR)), ]
  d <- d[order(d$date), ]
  gaps <- which(diff(d$date) > 1)
  if (length(gaps)) warning("Gaps in the national temperature series after: ",
                            paste(utils::head(d$date[gaps], 5), collapse = ", "))
  d
}

# 7-day average, calendar position, rankings
sa_temperature_metrics <- function(d) {
  d <- d[order(d$date), ]
  d$t7 <- as.numeric(stats::filter(d$t_c, rep(1 / 7, 7), sides = 1))   # trailing 7-day mean
  d$year <- as.integer(format(d$date, "%Y"))
  d$md <- format(d$date, "%m-%d")
  d$x <- as.Date(paste0("2000-", d$md))                                 # 2000 is a leap year
  cur <- max(d$year)
  last <- max(d$date)
  normal_years <- 1991:2020

  normal_day <- stats::aggregate(t7 ~ md, data = d[d$year %in% normal_years, ], FUN = mean)
  normal_day$x <- as.Date(paste0("2000-", normal_day$md))

  # Year to date: 1 January to the latest date, every year
  ytd <- d[d$md <= format(last, "%m-%d"), ]
  n_ytd <- table(ytd$year)
  ytd <- stats::aggregate(t_c ~ year, data = ytd, FUN = mean)
  ytd <- ytd[n_ytd[as.character(ytd$year)] >= max(n_ytd) - 1, ]   # complete years only (leap day aside)
  ytd_norm <- mean(ytd$t_c[ytd$year %in% normal_years])
  ytd_cur <- ytd$t_c[ytd$year == cur]
  others <- ytd$t_c[ytd$year != cur]

  # Full calendar years
  n_days <- table(d$year)
  full <- as.integer(names(n_days)[n_days >= 365])
  annual <- stats::aggregate(t_c ~ year, data = d[d$year %in% full, ], FUN = mean)
  ann_norm <- mean(annual$t_c[annual$year %in% normal_years])
  annual$anomaly <- annual$t_c - ann_norm
  rec <- annual[which.max(annual$t_c), ]
  last10 <- utils::tail(annual$year, 10)
  first10 <- utils::head(annual$year, 10)
  years <- rbind(data.frame(year = annual$year, t_c = round(annual$t_c, 2), anomaly = round(annual$anomaly, 2),
                            period = "full year"),
                 data.frame(year = cur, t_c = round(ytd_cur, 2), anomaly = round(ytd_cur - ytd_norm, 2),
                            period = sprintf("1 January to %s", fmt_date(last))))
  years <- years[!(years$year == cur & years$period == "full year"), ]
  # Linear trend through complete years (ordinary least squares, 95% CI)
  fit <- stats::lm(t_c ~ year, data = annual)
  ci <- stats::confint(fit)["year", ] * 10

  list(
    daily = d, normal_day = normal_day, years = years,
    summary = list(
      year = cur, as_of = as.character(last),
      ytd_mean_c = round(ytd_cur, 2), ytd_anomaly_c = round(ytd_cur - ytd_norm, 2),
      ytd_rank_warmest = 1L + sum(others > ytd_cur), n_years = length(others) + 1L,
      first_year = min(d$year),
      record_year = rec$year, record_year_mean_c = round(rec$t_c, 2),
      record_year_anomaly_c = round(rec$anomaly, 2),
      warming_c = round(mean(annual$t_c[annual$year %in% last10]) - mean(annual$t_c[annual$year %in% first10]), 2),
      trend_c_per_decade = round(unname(stats::coef(fit)[2]) * 10, 3),
      trend_ci_low = round(unname(ci[1]), 3), trend_ci_high = round(unname(ci[2]), 3),
      trend_r2 = round(summary(fit)$r.squared, 2),
      trend_years = sprintf("%d-%d", min(annual$year), max(annual$year)),
      year_estimate_c = round(ann_norm + ytd_cur - ytd_norm, 2),
      warming_periods = sprintf("%d-%d vs %d-%d", min(last10), max(last10), min(first10), max(first10)),
      n_points = as.integer(stats::median(d$n_points, na.rm = TRUE))
    )
  )
}

TEMP_SOURCE <- ERA5_SOURCE
TEMP_NOTE <- "Average over South Africa's land area (%d grid cells of ~25 km). Reanalysis, not weather stations; latest weeks preliminary."
fmt_c <- function(x) ifelse(abs(x) < 0.05, "0.0\u00b0C", sprintf("%+.1f\u00b0C", x))

plot_sa_temp_daily <- function(m) {
  s <- m$summary
  d <- m$daily[!is.na(m$daily$t7), ]
  cur_lab <- as.character(s$year)
  rec_lab <- sprintf("%d (warmest full year)", s$record_year)
  d$group <- ifelse(d$year == s$year, cur_lab, ifelse(d$year == s$record_year, rec_lab, "Other years"))
  nd <- m$normal_day; nd$group <- "1991-2020 average"
  last <- d[d$date == max(d$date), ]
  cols <- stats::setNames(c(SAIN$s1, SAIN$s2, SAIN$ink2, SAIN$axis),
                          c(cur_lab, rec_lab, "1991-2020 average", "Other years"))
  rank_txt <- if (s$ytd_rank_warmest == 1) "warmest" else paste(ordinal(s$ytd_rank_warmest), "warmest")
  ggplot() +
    geom_line(data = d[d$group == "Other years", ], aes(x, t7, group = year, colour = group),
              linewidth = 0.35, alpha = 0.7) +
    geom_line(data = nd, aes(x, t7, colour = group), linewidth = 0.8, linetype = "22") +
    geom_line(data = d[d$group == rec_lab, ], aes(x, t7, colour = group), linewidth = 0.9) +
    geom_line(data = d[d$group == cur_lab, ], aes(x, t7, colour = group), linewidth = 1.5) +
    geom_point(data = last, aes(x, t7), colour = SAIN$s1, size = 2.8) +
    scale_colour_manual(values = cols, breaks = names(cols)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(labels = function(v) paste0(v, "°C")) +
    labs(
      title = sprintf("%d so far: South Africa's %s year since %d", s$year, rank_txt, s$first_year),
      subtitle = sprintf(paste0("Daily average air temperature across South Africa (7-day average), every year since %d.\n",
                                "%d to %s: %s compared with the 1991-2020 average for the same dates."),
                         s$first_year, s$year, fmt_date(s$as_of), fmt_c(s$ytd_anomaly_c)),
      x = NULL, y = NULL,
      caption = sain_caption(TEMP_SOURCE, sprintf(TEMP_NOTE, s$n_points))
    ) +
    theme_sain() +
    guides(colour = guide_legend(nrow = 2, override.aes = list(linewidth = 1.3, alpha = 1)))
}

plot_sa_temp_years <- function(m) {
  s <- m$summary
  y <- m$years
  y$cur <- y$year == s$year
  ggplot(y, aes(year, anomaly)) +
    geom_hline(yintercept = 0, colour = SAIN$axis, linewidth = 0.5) +
    geom_col(aes(fill = cur), width = 0.75, show.legend = FALSE) +
    geom_text(data = y[y$cur, ], aes(x = year + 0.6, label = paste0(fmt_c(anomaly), "\nso far")),
              hjust = 0, vjust = 0.5, colour = SAIN$s1, fontface = "bold", size = 3.8, lineheight = 0.9) +
    scale_fill_manual(values = c(`FALSE` = SAIN$muted, `TRUE` = SAIN$s1)) +
    scale_x_continuous(breaks = seq(1985, 2025, 10), expand = expansion(add = c(0.8, 3.5))) +
    scale_y_continuous(labels = fmt_c, expand = expansion(mult = c(0.05, 0.2))) +
    labs(
      title = sprintf("South Africa is %s°C warmer than in the 1980s", sprintf("%.1f", s$warming_c)),
      subtitle = sprintf(paste0("Each year's average air temperature compared with 1991-2020.\n",
                                "Warming = %s. %d: 1 Jan to %s, against the same dates."),
                         sub(" vs ", " average minus ", s$warming_periods), s$year,
                         format(as.Date(s$as_of), "%d %b")),
      x = NULL, y = NULL,
      caption = sain_caption(TEMP_SOURCE, sprintf(TEMP_NOTE, s$n_points))
    ) +
    theme_sain() +
    coord_cartesian(clip = "off")
}

# Yearly average as a time series, with a linear trend and its 95% confidence band.
# The current year is shown as an estimate: the 1991-2020 annual average plus
# this year's anomaly so far (a January-November mean would not be comparable,
# because it leaves out December).
plot_sa_temp_trend <- function(m) {
  s <- m$summary
  y <- m$years
  full <- y[y$period == "full year", ]
  fit <- stats::lm(t_c ~ year, data = full)
  slope <- s$trend_c_per_decade
  ci <- c(s$trend_ci_low, s$trend_ci_high)
  band <- data.frame(year = seq(min(full$year), max(full$year), length.out = 100))
  band <- cbind(band, stats::predict(fit, band, interval = "confidence"))
  est <- data.frame(year = s$year, t_c = s$year_estimate_c)
  rec <- full[full$year == s$record_year, ]
  ggplot() +
    geom_ribbon(data = band, aes(year, ymin = lwr, ymax = upr), fill = SAIN$band_inner) +
    geom_line(data = band, aes(year, fit), colour = SAIN$ink2, linewidth = 0.9, linetype = "22") +
    geom_line(data = full, aes(year, t_c), colour = SAIN$axis, linewidth = 0.7) +
    geom_point(data = full, aes(year, t_c), colour = SAIN$muted, size = 1.9) +
    geom_point(data = rec, aes(year, t_c), colour = SAIN$s2, size = 3.4) +
    geom_text(data = rec, aes(year, t_c, label = sprintf("%d: %.1f°C\n(warmest)", year, t_c)),
              colour = SAIN$s2, fontface = "bold", size = 3.7, vjust = -0.35, lineheight = 0.9) +
    geom_point(data = est, aes(year, t_c), colour = SAIN$s1, fill = SAIN$surface, shape = 21,
               size = 3.4, stroke = 1.6) +
    geom_text(data = est, aes(year + 0.6, t_c, label = sprintf("%d so far:\n~%.1f°C", year, t_c)),
              colour = SAIN$s1, fontface = "bold", size = 3.7, hjust = 0, lineheight = 0.9) +
    scale_x_continuous(breaks = seq(1985, 2025, 10), expand = expansion(add = c(1, 4.5))) +
    scale_y_continuous(labels = function(v) paste0(v, "°C"), expand = expansion(mult = c(0.08, 0.15))) +
    coord_cartesian(clip = "off") +
    labs(
      title = sprintf("South Africa is warming by about %.2f°C a decade", slope),
      subtitle = sprintf(paste0("Average air temperature each year since %d. Dashed: linear trend; shaded: its 95%% ",
                                "confidence interval\n(%.2f to %.2f°C a decade). %d: estimate from 1 January to %s."),
                         s$first_year, ci[1], ci[2], s$year, format(as.Date(s$as_of), "%d %B")),
      x = NULL, y = NULL,
      caption = sain_caption(TEMP_SOURCE, sprintf(TEMP_NOTE, s$n_points))
    ) +
    theme_sain()
}
