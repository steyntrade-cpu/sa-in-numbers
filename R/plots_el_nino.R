# ---------------------------------------------------------------------------
# Charts for the El Nino summer tracker. Each function returns a ggplot that
# works on the web page and, via save_social(), in every social format.
# Colour identity is fixed across charts:
#   this season = s1 (blue), 1997/98 = s2 (orange), 2015/16 = s3 (aqua),
#   normal / context = grey.
# ---------------------------------------------------------------------------

library(ggplot2)

SERIES_COLOURS <- function(cur_label) {
  stats::setNames(c(SAIN$s1, SAIN$s2, SAIN$s3),
                  c(cur_label, season_label(COMPARE_SEASONS)))
}

# Heat charts: ERA5 daily maximum (NASA POWER until 22 Nov 2026; names kept)
POWER_SOURCE <- ERA5_SOURCE
POWER_NOTE <- ERA5_NOTE
CHIRPS_SOURCE <- "CHIRPS v3 (Climate Hazards Center, UC Santa Barbara)"
CHIRPS_NOTE <- "Satellite estimates blended with rain gauges, ~5 km, in 5-day totals. Latest weeks preliminary."

# Put season days on a calendar axis using a reference season
season_axis_date <- function(day, type = "summer") {
  as.Date(sprintf("2001-%02d-01", SEASONS[[type]]$start_month)) + day - 1
}

rank_phrase <- function(rank, n, word = "driest") {
  if (is.na(rank) || rank > 3) return("")
  sprintf("%s %s of %d", if (rank == 1) "" else ordinal(rank), word, n) |> trimws()
}

# --- Rain: summer-rainfall index (all five summer regions, equal weight) -----
# y = rain so far as % of a normal full-season total
plot_rain_summer <- function(sindex, s, zoom = FALSE) {
  L <- SEASONS$summer$length
  cur <- max(sindex$season)
  cur_lab <- season_label(cur)
  band <- quantile_band(sindex, "pct_season", group = "index")
  lines <- sindex[sindex$season %in% c(cur, COMPARE_SEASONS), ]
  lines$label <- factor(season_label(lines$season),
                        levels = c(cur_lab, season_label(COMPARE_SEASONS)))
  lines$x <- season_axis_date(lines$season_day)
  band$x <- season_axis_date(band$season_day)
  cur_day <- max(lines$season_day[lines$season == cur])
  last <- lines[lines$season == cur & lines$season_day == cur_day, ]
  x_end <- if (zoom) min(L, max(61, cur_day + 30)) else L
  if (zoom) {
    lines <- lines[lines$season_day <= x_end, ]
    band <- band[band$season_day <= x_end, ]
  }
  lab_day <- min(x_end, round(x_end * 0.87))

  ggplot() +
    geom_ribbon(data = band, aes(x, ymin = p10, ymax = p90), fill = SAIN$band_outer) +
    geom_ribbon(data = band, aes(x, ymin = p25, ymax = p75), fill = SAIN$band_inner) +
    geom_line(data = band, aes(x, p50), colour = SAIN$muted, linetype = "22", linewidth = 0.6) +
    geom_line(data = lines[lines$season != cur, ], aes(x, pct_season, colour = label), linewidth = 0.8) +
    geom_line(data = lines[lines$season == cur, ], aes(x, pct_season, colour = label), linewidth = 1.6,
              lineend = "round") +
    geom_point(data = last, aes(x, pct_season), colour = SAIN$s1, size = 3.2,
               shape = 21, fill = SAIN$s1, stroke = 1.2) +
    geom_text(data = last, aes(x, pct_season, label = paste0(s$rain_pct_of_normal, "% of\nnormal")),
              colour = SAIN$ink, fontface = "bold", size = 4.2, hjust = -0.25, vjust = 0.5,
              lineheight = 0.9) +
    annotate("text", x = season_axis_date(lab_day), y = band$p90[band$season_day == lab_day],
             label = "Normal range\n(1991-2020)", colour = SAIN$muted, size = 3.4,
             hjust = 1, vjust = -0.3, lineheight = 0.9) +
    scale_colour_manual(values = SERIES_COLOURS(cur_lab), drop = FALSE) +
    scale_x_date(date_breaks = if (zoom) "2 weeks" else "1 month",
                 date_labels = if (zoom) "%e %b" else "%b", expand = expansion(mult = c(0, 0.02)),
                 limits = season_axis_date(c(1, x_end))) +
    scale_y_continuous(labels = scales::label_percent(scale = 1),
                       expand = expansion(mult = c(0, 0.10))) +
    coord_cartesian(clip = "off") +
    labs(
      title = sprintf("Summer rain so far: %d%% of normal", s$rain_pct_of_normal),
      subtitle = sprintf(paste0("Rain since 1 October, to %s, as a share of a normal season's total.\n",
                                "Five summer-rainfall regions, each counted equally.\n",
                                "Grey bands: middle 50%% and 80%% of 1991-2020 seasons. Dashed: median."),
                         fmt_date(s$as_of)),
      x = NULL, y = NULL,
      caption = sain_caption(CHIRPS_SOURCE, CHIRPS_NOTE)
    ) +
    theme_sain() +
    guides(colour = guide_legend(override.aes = list(linewidth = 1.4)))
}

# --- Rain by region: % of normal so far -----------------------------------
plot_rain_regions <- function(s) {
  r <- do.call(rbind, lapply(s$regions, function(z) if (z$season_type == "summer") data.frame(
    region = z$region, pct = z$rain_pct_of_normal, pct_2015 = 100 * z$rain_mm_2015 / z$rain_normal_mm,
    note = rank_phrase(z$rain_rank_driest, z$n_seasons), stringsAsFactors = FALSE)))
  r <- r[order(-r$pct), ]
  r$region <- factor(r$region, levels = r$region)
  r$lab <- ifelse(nzchar(r$note), sprintf("%d%%  · %s", r$pct, r$note), sprintf("%d%%", r$pct))
  n <- s$regions[[1]]$n_seasons
  ggplot(r, aes(y = region)) +
    geom_vline(xintercept = 100, colour = SAIN$axis, linetype = "22", linewidth = 0.5) +
    annotate("text", x = 100, y = Inf, label = "normal", colour = SAIN$muted, size = 3.3,
             vjust = -0.5) +
    geom_col(aes(x = pct, fill = "This season"), width = 0.62) +
    geom_point(aes(x = pct_2015, colour = "2015/16, same date"), size = 3.4, shape = 21,
               fill = SAIN$surface, stroke = 1.6) +
    geom_text(aes(x = pmax(pct, pct_2015) + 0.05 * max(pct, pct_2015, 100), label = lab), hjust = 0,
              colour = SAIN$ink, size = 4.2, fontface = "bold") +
    scale_fill_manual(values = c("This season" = SAIN$s1)) +
    scale_colour_manual(values = c("2015/16, same date" = SAIN$s3)) +
    guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2)) +
    scale_x_continuous(labels = scales::label_percent(scale = 1),
                       expand = expansion(mult = c(0, 0.45))) +
    coord_cartesian(clip = "off") +
    labs(
      title = sprintf("Rain so far by region: %d%% to %d%% of normal", min(r$pct), max(r$pct)),
      subtitle = sprintf(paste0("Rain from 1 October to %s, as %% of the 1991-2020 median for the same dates.\n",
                                "Rank among the %d seasons since 1981/82 shown where in the driest three."),
                         fmt_date(s$as_of), n),
      x = NULL, y = NULL,
      caption = sain_caption(CHIRPS_SOURCE, CHIRPS_NOTE)
    ) +
    theme_sain() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = SAIN$grid, linewidth = 0.3),
          axis.text.y = element_text(colour = SAIN$ink2, size = rel(0.9)),
          plot.margin = margin(30, 28, 18, 24))
}

# --- Rain by region: cumulative panels --------------------------------------
plot_rain_region_panels <- function(cum, band, s, regions, zoom = TRUE, ncol = 3) {
  L <- SEASONS$summer$length
  ids <- regions$region_id[regions$season == "summer"]
  ids <- ids[ids %in% unique(cum$region_id)]
  cur <- max(cum$season[cum$region_id %in% ids])
  cur_lab <- season_label(cur)
  reg_stat <- Filter(function(z) z$region_id %in% ids, s$regions)
  strip <- stats::setNames(vapply(reg_stat, function(z) sprintf("%s\n%d%% of normal so far", z$region, z$rain_pct_of_normal), ""),
                           vapply(reg_stat, `[[`, "", "region_id"))
  cur_day <- s$season_day
  x_end <- if (zoom) min(L, max(61, cur_day + 30)) else L
  lines <- cum[cum$region_id %in% ids & cum$season %in% c(cur, COMPARE_SEASONS) & cum$season_day <= x_end, ]
  lines$label <- factor(season_label(lines$season), levels = c(cur_lab, season_label(COMPARE_SEASONS)))
  b <- band[band$region_id %in% ids & band$season_day <= x_end, ]
  lines$x <- season_axis_date(lines$season_day); b$x <- season_axis_date(b$season_day)
  lines$panel <- factor(strip[lines$region_id], levels = strip)
  b$panel <- factor(strip[b$region_id], levels = strip)
  last <- lines[lines$season == cur & lines$season_day == cur_day, ]

  ggplot() +
    geom_ribbon(data = b, aes(x, ymin = p10, ymax = p90), fill = SAIN$band_outer) +
    geom_ribbon(data = b, aes(x, ymin = p25, ymax = p75), fill = SAIN$band_inner) +
    geom_line(data = b, aes(x, p50), colour = SAIN$muted, linetype = "22", linewidth = 0.5) +
    geom_line(data = lines[lines$season != cur, ], aes(x, cum_mm, colour = label), linewidth = 0.7) +
    geom_line(data = lines[lines$season == cur, ], aes(x, cum_mm, colour = label), linewidth = 1.3) +
    geom_point(data = last, aes(x, cum_mm), colour = SAIN$s1, size = 2.2) +
    facet_wrap(~panel, ncol = ncol, scales = "free_y") +
    scale_colour_manual(values = SERIES_COLOURS(cur_lab), drop = FALSE) +
    scale_x_date(date_breaks = if (zoom) "1 month" else "2 months", date_labels = "%b",
                 expand = expansion(mult = c(0, 0.02))) +
    scale_y_continuous(labels = scales::label_number(suffix = " mm"),
                       expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = "Rain so far, region by region",
      subtitle = sprintf(paste0("Rain since 1 October, to %s, with %% of normal for the same dates.\n",
                                "Grey bands: middle 50%% and 80%% of 1991-2020 seasons. Dashed: median."),
                         fmt_date(s$as_of)),
      x = NULL, y = NULL,
      caption = sain_caption(CHIRPS_SOURCE, CHIRPS_NOTE)
    ) +
    theme_sain(base_size = 13) +
    theme(strip.text = element_text(colour = SAIN$ink, face = "bold", hjust = 0, size = rel(0.9),
                                    lineheight = 1.05, margin = margin(b = 4)),
          panel.spacing = unit(14, "pt")) +
    guides(colour = guide_legend(override.aes = list(linewidth = 1.3)))
}

# --- Heat ----------------------------------------------------------------------
plot_heat <- function(s) {
  cur_lab <- s$season
  d <- data.frame(
    label = c("Normal (1991-2020 average)", vapply(s$compare, `[[`, "", "season"), cur_lab),
    days  = c(s$heat_days_normal, vapply(s$compare, function(z) z$heat_days, 0), s$heat_days)
  )
  d$label <- factor(d$label, levels = d$label)
  fills <- c(SAIN$axis, SAIN$s2, SAIN$s3, SAIN$s1)
  ggplot(d, aes(days, label)) +
    geom_col(aes(fill = label), width = 0.62, show.legend = FALSE) +
    geom_text(aes(label = format(round(days, 1), nsmall = 1)), hjust = -0.25,
              colour = SAIN$ink, size = 4.6, fontface = "bold") +
    scale_fill_manual(values = stats::setNames(fills, levels(d$label))) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = sprintf("Hot days so far: %s (normal %s)", format(s$heat_days, nsmall = 1),
                      format(s$heat_days_normal, nsmall = 1)),
      subtitle = sprintf(paste0("Days since 1 October (to %s) hotter than the local 90th percentile\n",
                                "for that time of year. Average of %d regions; normal is about 1 day in 10."),
                         fmt_date(s$heat_as_of), length(s$regions)),
      x = NULL, y = NULL,
      caption = sain_caption(POWER_SOURCE, "Same date in each season, so comparisons are like for like.")
    ) +
    theme_sain() +
    theme(panel.grid.major.y = element_blank(), axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text.y = element_text(colour = SAIN$ink2, size = rel(0.9)))
}

plot_heat_regions <- function(s) {
  r <- do.call(rbind, lapply(s$regions, function(z) data.frame(
    region = z$region, days = z$heat_days, normal = z$heat_days_normal, d2015 = z$heat_days_2015,
    note = rank_phrase(z$heat_rank_hottest, z$n_seasons, "hottest"), stringsAsFactors = FALSE)))
  r <- r[order(r$days), ]
  r$region <- factor(r$region, levels = r$region)
  r$lab <- ifelse(nzchar(r$note), sprintf("%.1f  · %s", r$days, r$note), sprintf("%.1f", r$days))
  ggplot(r, aes(y = region)) +
    geom_col(aes(x = days, fill = "This season"), width = 0.62) +
    geom_point(aes(x = normal, colour = "Normal (1991-2020)"), size = 3.2, shape = 21,
               fill = SAIN$surface, stroke = 1.5) +
    geom_point(aes(x = d2015, colour = "2015/16, same date"), size = 3.2, shape = 21,
               fill = SAIN$surface, stroke = 1.5) +
    geom_text(aes(x = pmax(days, d2015, normal) + 0.05 * max(days, d2015, normal), label = lab),
              hjust = 0, colour = SAIN$ink, size = 4.2, fontface = "bold") +
    scale_fill_manual(values = c("This season" = SAIN$s1)) +
    scale_colour_manual(values = c("Normal (1991-2020)" = SAIN$ink2, "2015/16, same date" = SAIN$s3)) +
    guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.45))) +
    coord_cartesian(clip = "off") +
    labs(
      title = "Hot days so far, by region",
      subtitle = sprintf(paste0("Days since 1 October (to %s) above the local 90th percentile of daily maximum.\n",
                                "Rank among the %d seasons since 1981/82 shown where in the hottest three."),
                         fmt_date(s$heat_as_of), s$regions[[1]]$n_seasons),
      x = NULL, y = NULL,
      caption = sain_caption(POWER_SOURCE, POWER_NOTE)
    ) +
    theme_sain() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = SAIN$grid, linewidth = 0.3),
          axis.text.y = element_text(colour = SAIN$ink2, size = rel(0.9)))
}

# --- El Nino strength ----------------------------------------------------------
plot_roni_events <- function(events, top_n = 10) {
  hist_ev <- events[!events$ongoing, ]
  hist_ev <- hist_ev[order(-hist_ev$peak), ][seq_len(min(top_n, nrow(hist_ev))), ]
  cur <- events[events$ongoing, ]
  d <- rbind(hist_ev, cur)
  d$label <- ifelse(d$ongoing, paste0(d$label, " (so far)"), d$label)
  dup <- duplicated(d$label) | duplicated(d$label, fromLast = TRUE)
  d$label[dup] <- paste0(d$label[dup], " (", format(as.Date(d$peak_date[dup]), "%b %Y"), ")")
  d <- d[order(d$peak, d$ongoing), ]
  d$label <- factor(d$label, levels = d$label)
  rk <- roni_current_rank(events)

  ggplot(d, aes(peak, label)) +
    geom_vline(xintercept = c(1.5, 2.0), colour = SAIN$axis, linetype = "22", linewidth = 0.4) +
    annotate("text", x = c(1.5, 2.0), y = Inf, label = c("strong", "very strong"),
             colour = SAIN$muted, size = 3.3, vjust = -0.4, hjust = 0.5) +
    geom_col(aes(fill = ongoing), width = 0.62, show.legend = FALSE) +
    geom_text(aes(label = sprintf("+%.2f", peak)), hjust = -0.2, colour = SAIN$ink,
              size = 4.2, fontface = "bold") +
    scale_fill_manual(values = c(`FALSE` = SAIN$axis, `TRUE` = SAIN$s1)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.14))) +
    coord_cartesian(clip = "off") +
    labs(
      title = if (!is.na(rk$rank)) sprintf("This El Niño so far: %s%s strongest of %d since 1950",
                                           if (rk$tied) "joint " else "", ordinal(rk$rank), rk$n) else
        "The strongest El Niño events since 1950",
      subtitle = paste0("Peak Relative Oceanic Niño Index (°C above the tropical average),\n",
                        "the strongest events since 1950. Current event: peak so far, still rising."),
      x = NULL, y = NULL,
      caption = sain_caption("NOAA Climate Prediction Center, RONI (ERSST)",
                             "Recent values are provisional and can be revised for up to two months.")
    ) +
    theme_sain() +
    theme(panel.grid.major.y = element_blank(), axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text.y = element_text(colour = SAIN$ink2, size = rel(0.9)),
          plot.margin = margin(30, 28, 18, 24))
}

# --- Dams ----------------------------------------------------------------------
# Primary drainage regions (DWS)
DRAINAGE_NAMES <- c(
  A = "Limpopo", B = "Olifants (Limpopo)", C = "Vaal", D = "Orange", E = "Olifants (W Cape)",
  F = "Namaqualand", G = "Berg", H = "Breede", J = "Gouritz", K = "Southern Cape coast",
  L = "Gamtoos", M = "Algoa", N = "Sundays", P = "Albany coast", Q = "Great Fish",
  R = "Buffalo-Keiskamma", S = "Great Kei", T = "Mzimvubu", U = "Mvoti-Mgeni",
  V = "Thukela", W = "Pongola-Usutu", X = "Komati-Inkomati")

plot_dams <- function(dams_summary) {
  nat <- dams_summary[dams_summary$region == "National", ]
  d <- dams_summary[dams_summary$region != "National" & dams_summary$fsc_mcm >= 50, ]
  d$name <- ifelse(d$region %in% names(DRAINAGE_NAMES), DRAINAGE_NAMES[d$region], d$region)
  d <- d[order(d$pct_this_week), ]
  d$name <- factor(d$name, levels = d$name)
  ggplot(d, aes(y = name)) +
    geom_col(aes(x = pct_this_week, fill = "This week"), width = 0.62) +
    geom_point(aes(x = pct_last_year, colour = "A year ago"), size = 3, shape = 21,
               fill = SAIN$surface, stroke = 1.4) +
    scale_fill_manual(values = c("This week" = SAIN$s1)) +
    scale_colour_manual(values = c("A year ago" = SAIN$ink2)) +
    guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2)) +
    scale_x_continuous(labels = scales::label_percent(scale = 1), limits = c(0, 110),
                       breaks = c(0, 25, 50, 75, 100), expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = sprintf("Dams: %.0f%% full nationally (a year ago: %.0f%%)",
                      nat$pct_this_week, nat$pct_last_year),
      subtitle = sprintf(paste0("Storage by primary drainage region, weighted by dam capacity.\n",
                                "DWS report of %s. Regions with at least 50 million m³ capacity."),
                         fmt_date(nat$report_date)),
      x = NULL, y = NULL,
      caption = sain_caption("Department of Water and Sanitation, Weekly State of Reservoirs",
                             sprintf("%d dams on the DWS drainage-region pages, including Lesotho's Katse and Mohale.", nat$n_dams))
    ) +
    theme_sain() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = SAIN$grid, linewidth = 0.3),
          axis.text.y = element_text(colour = SAIN$ink2, size = rel(0.85)))
}

plot_dam_history <- function(archive) {
  nat <- archive[archive$region == "National", ]
  if (nrow(nat) < 2) return(NULL)
  ggplot(nat, aes(as.Date(report_date), pct_this_week)) +
    geom_line(colour = SAIN$s1, linewidth = 1.4) +
    geom_point(colour = SAIN$s1, size = 2.6) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    labs(title = "National dam storage, week by week",
         subtitle = "Since this tracker started. FSC-weighted % full.",
         x = NULL, y = NULL,
         caption = sain_caption("Department of Water and Sanitation, Weekly State of Reservoirs")) +
    theme_sain()
}
