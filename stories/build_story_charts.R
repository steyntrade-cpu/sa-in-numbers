# ---------------------------------------------------------------------------
# Charts for the November 2026 stories
#
# Stories are snapshots, so their numbers are frozen. The first run saves each
# story's input numbers to data.csv in the story folder; later runs redraw the
# charts from that file, so the weekly data updates never change a published
# story. To rebuild a story from current data, delete its data.csv first.
#
# Run from anywhere:  source(".../stories/build_story_charts.R")
#                     or  Rscript stories/build_story_charts.R
# Rain needs the CHIRPS cache (data/chirps/chirps_sites.csv); nothing is downloaded.
# ---------------------------------------------------------------------------

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

suppressPackageStartupMessages(library(ggplot2))
for (f in c("R/theme_sain.R", "R/fetch_power.R", "R/fetch_chirps.R", "R/metrics_el_nino.R"))
  source(f)

NORMAL <- 1991:2020
GREY <- "#c3c2b7"
fmt_anom <- function(x) ifelse(abs(x) < 0.005, "0\u00b0C", sprintf("%+.1f\u00b0C", x))

# Web version for the page (1400 x 900) plus the three social formats
save_story <- function(plot, dir, stem) {
  ggsave(file.path(dir, paste0(stem, ".png")), plot, device = ragg::agg_png,
         width = 1400 / 150, height = 900 / 150, dpi = 150, bg = SAIN$surface)
  save_social(plot, stem, out_dir = file.path(dir, "social"), formats = c("story", "square", "wide"),
              phone_text = 1.35)
}

# ===========================================================================
# 1. The warmest 1 October to 15 November since 1981
# ===========================================================================
T_DIR <- "stories/2026-11-warmest-start"
T_END <- "11-15"                         # same cut-off as the rain story and the ERA5 check
T_YEAR <- 2026L
t_file <- file.path(T_DIR, "data.csv")

if (!file.exists(t_file)) {
  d <- read.csv("data/processed/sa_temperature_daily.csv")
  d$date <- as.Date(d$date)
  d$year <- as.integer(format(d$date, "%Y"))
  d$md <- format(d$date, "%m-%d")
  part <- function(from, to) {
    w <- d[d$md >= from & d$md <= to, ]
    a <- stats::aggregate(t_c ~ year, data = w, FUN = mean)
    a$anomaly <- a$t_c - mean(a$t_c[a$year %in% NORMAL])
    a
  }
  win <- part("10-01", T_END); oct <- part("10-01", "10-31"); nov <- part("11-01", T_END)
  t_data <- data.frame(year = win$year, mean_c = round(win$t_c, 3), anomaly_c = round(win$anomaly, 3),
                       oct_anomaly_c = round(oct$anomaly[match(win$year, oct$year)], 3),
                       nov_anomaly_c = round(nov$anomaly[match(win$year, nov$year)], 3))
  t_data$rank_warmest <- rank(-t_data$mean_c, ties.method = "min")
  write.csv(t_data, t_file, row.names = FALSE)
  message("Saved ", t_file, " (frozen; delete it to rebuild from current data)")
}
t_data <- read.csv(t_file)

t_source <- sain_caption(
  "NASA POWER, air temperature at 2 m, mean of 114 points 1\u00b0 apart (MERRA-2; latest weeks GEOS-IT, provisional)",
  "Average: 1991\u20132020.")

hi <- function(y) ifelse(y == T_YEAR, "now", ifelse(y == 2015, "cmp", "other"))
t_data$hi <- hi(t_data$year)
lab <- t_data[t_data$year %in% c(T_YEAR, 2015), ]

p_years <- ggplot(t_data, aes(year, anomaly_c, fill = hi)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = 0, colour = SAIN$axis, linewidth = 0.4) +
  geom_text(data = lab, aes(label = paste0(year, "\n", fmt_anom(anomaly_c)),
                            colour = hi), vjust = -0.3, size = 3.6, lineheight = 0.9, fontface = "bold") +
  scale_fill_manual(values = c(now = SAIN$s1, cmp = SAIN$s2, other = GREY), guide = "none") +
  scale_colour_manual(values = c(now = SAIN$s1, cmp = SAIN$s2), guide = "none") +
  scale_y_continuous(labels = fmt_anom, expand = expansion(mult = c(0.05, 0.2))) +
  scale_x_continuous(breaks = seq(1985, 2025, 10), expand = expansion(add = c(0.8, 2.5))) +
  labs(title = "The warmest 1 October to 15 November since 1981",
       subtitle = "Average temperature across South Africa on those dates each year,\ncompared with the 1991\u20132020 average",
       x = NULL, y = NULL, caption = t_source) +
  theme_sain()

notable <- t_data[t_data$year %in% c(2015, 2022, 2004, 1997), ]
cur <- t_data[t_data$year == T_YEAR, ]
p_both <- ggplot(t_data, aes(oct_anomaly_c, nov_anomaly_c)) +
  geom_hline(yintercept = 0, colour = SAIN$axis, linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = SAIN$axis, linewidth = 0.4) +
  geom_point(data = t_data[t_data$year != T_YEAR, ], colour = GREY, size = 2.6) +
  geom_text(data = notable, aes(label = year), colour = SAIN$ink2, size = 3.4, hjust = 1.25) +
  geom_point(data = cur, colour = SAIN$s1, size = 4.2) +
  geom_text(data = cur, aes(label = year), colour = SAIN$s1, size = 4.2, fontface = "bold", hjust = 1.3) +
  scale_x_continuous(labels = fmt_anom, breaks = seq(-4, 4, 2), expand = expansion(mult = 0.12)) +
  scale_y_continuous(labels = fmt_anom, breaks = seq(-4, 4, 2), expand = expansion(mult = 0.12)) +
  labs(title = "Other years had a hot October or a hot early November.\n2026 had both.",
       subtitle = "Each dot is one year since 1981, compared with the 1991\u20132020 average",
       x = "October", y = "1\u201315 November", caption = t_source) +
  theme_sain() +
  theme(panel.grid.major.x = element_line(colour = SAIN$grid, linewidth = 0.3))

save_story(p_years, T_DIR, "chart-years")
save_story(p_both, T_DIR, "chart-both-halves")

# ===========================================================================
# 2. The driest October in 46 years on the Highveld and in Limpopo
# ===========================================================================
R_DIR <- "stories/2026-11-driest-october"
R_SEASON <- 2026L
r_file <- file.path(R_DIR, "data.csv")
regions <- read_regions()

if (!file.exists(r_file)) {
  sites <- read_sites()
  rain <- fetch_rain_chirps(sites, regions, offline = TRUE)
  oct_rain <- rain[format(rain$date, "%m") == "10", ]
  if (any(oct_rain$prelim[format(oct_rain$date, "%Y") == R_SEASON]))
    warning("October ", R_SEASON, " includes preliminary CHIRPS data")
  cum <- rain_cumulative(rain, sites, regions)
  oct <- cum[cum$season_day == 31 & cum$region_id %in% regions$region_id[regions$season == "summer"],
             c("region_id", "season", "cum_mm")]
  r_data <- do.call(rbind, lapply(split(oct, oct$region_id), function(x) {
    med <- stats::median(x$cum_mm[x$season %in% NORMAL])
    data.frame(region_id = x$region_id, october = x$season, rain_mm = round(x$cum_mm, 1),
               normal_mm = round(med, 1), pct_of_normal = round(100 * x$cum_mm / med),
               rank_driest = rank(x$cum_mm, ties.method = "min"), n_years = nrow(x))
  }))
  write.csv(r_data, r_file, row.names = FALSE)
  message("Saved ", r_file, " (frozen; delete it to rebuild from current data)")
}
r_data <- read.csv(r_file)
r_data$region <- regions$region[match(r_data$region_id, regions$region_id)]

r_source <- sain_caption(
  "CHIRPS v3 (Climate Hazards Center, UC Santa Barbara), final data; mean of the sites in each region",
  "Normal: 1991\u20132020 median.")

two <- r_data[r_data$region_id %in% c("EHV", "LPL"), ]
prev <- do.call(rbind, lapply(split(two[two$october != R_SEASON, ], two$region_id[two$october != R_SEASON]),
                              function(x) x[which.min(x$rain_mm), ]))
cur2 <- two[two$october == R_SEASON, ]
strip <- stats::setNames(sprintf("%s\n%d: %.0f mm  \u00b7  previous driest: %.0f mm in %d",
                                 cur2$region, R_SEASON, cur2$rain_mm,
                                 prev$rain_mm[match(cur2$region_id, prev$region_id)],
                                 prev$october[match(cur2$region_id, prev$region_id)]),
                         cur2$region_id)
two$panel <- factor(strip[two$region_id], levels = strip[c("EHV", "LPL")])
two$hi <- ifelse(two$october == R_SEASON, "now",
                 ifelse(paste(two$region_id, two$october) %in% paste(prev$region_id, prev$october), "prev", "other"))
norm_lines <- unique(two[c("panel", "normal_mm")])
lab_now <- two[two$october == R_SEASON, ]

p_oct <- ggplot(two, aes(october, rain_mm, fill = hi)) +
  geom_col(width = 0.75) +
  geom_hline(data = norm_lines, aes(yintercept = normal_mm), linetype = "dashed",
             colour = SAIN$ink2, linewidth = 0.4) +
  geom_text(data = lab_now, aes(x = october + 0.7, label = october), colour = SAIN$s1,
            hjust = 0, vjust = 0, size = 3.6, fontface = "bold") +
  facet_wrap(~panel, ncol = 1) +
  scale_fill_manual(values = c(now = SAIN$s1, prev = SAIN$muted, other = GREY), guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, " mm"), breaks = seq(0, 400, 100),
                     expand = expansion(mult = c(0, 0.08))) +
  scale_x_continuous(breaks = seq(1985, 2025, 10), expand = expansion(add = c(0.8, 6))) +
  labs(title = "The driest October in 46 years on the Highveld and in Limpopo",
       subtitle = "October rainfall each year since 1981. Dashed line: the 1991\u20132020 median",
       x = NULL, y = NULL, caption = r_source) +
  theme_sain() +
  theme(strip.text = element_text(colour = SAIN$ink2, hjust = 0, size = rel(0.85), lineheight = 1.1,
                                  margin = margin(t = 4, b = 6)))

now <- r_data[r_data$october == R_SEASON, ]
now$label <- with(now, ifelse(rank_driest == 1, sprintf("%d%%\ndriest of %d", pct_of_normal, n_years),
                       ifelse(pct_of_normal < 100,
                              sprintf("%d%%\n%s driest of %d", pct_of_normal, vapply(rank_driest, ordinal, ""), n_years),
                              sprintf("%d%%\nabove normal", pct_of_normal))))
now$region <- factor(now$region, levels = now$region[order(-now$pct_of_normal)])
p_regions <- ggplot(now, aes(pct_of_normal, region)) +
  geom_col(fill = SAIN$s1, width = 0.6) +
  geom_vline(xintercept = 100, linetype = "dashed", colour = SAIN$ink2, linewidth = 0.4) +
  geom_label(aes(x = pct_of_normal + 4, label = label), hjust = 0, size = 3.5, colour = SAIN$ink,
             fill = SAIN$surface, label.size = 0, label.padding = unit(0.15, "lines"), lineheight = 0.95) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 200), breaks = seq(0, 200, 100),
                     expand = expansion(mult = c(0, 0))) +
  coord_cartesian(clip = "off") +
  labs(title = "October 2026 rainfall by region",
       subtitle = "% of the 1991\u20132020 median October total (dashed line),\nand rank among the 46 Octobers since 1981",
       x = NULL, y = NULL, caption = r_source) +
  theme_sain() +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = SAIN$grid, linewidth = 0.3),
        axis.text.y = element_text(colour = SAIN$ink, size = rel(0.9)), axis.line.x = element_blank())

save_story(p_oct, R_DIR, "chart-october")
save_story(p_regions, R_DIR, "chart-regions")
message("Story charts saved in ", T_DIR, " and ", R_DIR)
