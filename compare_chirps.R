#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Check the tracker's rainfall (NASA POWER) against CHIRPS v3, an independent
# dataset that blends satellite estimates with rain-gauge records.
#   Rscript compare_chirps.R        (run update_el_nino.R first)
#
# Window: 1 October to 15 November (nine CHIRPS pentads), every season since
# 1981/82, same sites and regions as the tracker. For each summer-rainfall
# region it reports, from both datasets: rain this season, % of the 1991-2020
# median, and rank among all seasons (1 = driest). It also reports how well
# the two datasets agree from year to year.
# Uses the same CHIRPS cache as the tracker (data/chirps/), so after the
# tracker's first run nothing new needs downloading.
# Outputs: data/processed/chirps_check_regions.csv, outputs/chirps_check.png
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
suppressPackageStartupMessages(library(ggplot2))
for (f in c("R/theme_sain.R", "R/fetch_power.R", "R/metrics_el_nino.R", "R/fetch_chirps.R")) source(f)

FIRST_SEASON <- as.integer(Sys.getenv("CHIRPS_FIRST_SEASON", "1981"))
today <- Sys.Date()
cur <- as.integer(format(today, "%Y")) - (as.integer(format(today, "%m")) < 10)
seasons <- FIRST_SEASON:cur

regions <- read_regions()
regions <- regions[regions$season == "summer", ]
sites <- read_sites()
sites <- sites[sites$region_id %in% regions$region_id, ]

# --- CHIRPS: site totals for 1 Oct - 15 Nov -------------------------------
window <- data.frame(month = c(rep(10, 6), rep(11, 3)), pentad = c(1:6, 1:3))   # 1 Oct - 15 Nov
ch <- fetch_chirps_values(sites, merge(data.frame(year = seasons), window), "pentad")
ch <- ch[ch$month %in% c(10, 11) & !(ch$month == 11 & ch$pentad > 3), ]
n_pent <- stats::aggregate(rain_mm ~ site + year, data = ch, FUN = length)
ch_tot <- stats::aggregate(rain_mm ~ site + year, data = ch, FUN = sum)
ch_tot <- ch_tot[n_pent$rain_mm == nrow(window), ]
prelim_cur <- any(ch$prelim[ch$year == cur])

# --- NASA POWER: same sites, same window ----------------------------------
pw <- suppressWarnings(fetch_power_all(sites, offline = TRUE))
pw$season <- as.integer(format(pw$date, "%Y"))
md <- format(pw$date, "%m-%d")
pw <- pw[md >= "10-01" & md <= "11-15" & !is.na(pw$rain_mm), ]
n_days <- stats::aggregate(rain_mm ~ site + season, data = pw, FUN = length)
pw_tot <- stats::aggregate(rain_mm ~ site + season, data = pw, FUN = sum)
pw_tot <- pw_tot[n_days$rain_mm == 46, ]
names(pw_tot)[2] <- "year"

# --- Region series (mean of sites; seasons where every site has data) ------
region_series <- function(tot) {
  d <- merge(tot, sites[, c("site", "region_id")], by = "site")
  n <- stats::aggregate(rain_mm ~ region_id + year, data = d, FUN = length)
  r <- stats::aggregate(rain_mm ~ region_id + year, data = d, FUN = mean)
  k <- table(sites$region_id)
  r[n$rain_mm == k[r$region_id], ]
}
series <- rbind(cbind(dataset = "NASA POWER", region_series(pw_tot)),
                cbind(dataset = "CHIRPS v3", region_series(ch_tot)))
# Summer-rainfall index: mean of region % of their own median (equal weights)
series <- do.call(rbind, lapply(split(series, list(series$dataset, series$region_id), drop = TRUE), function(d) {
  d$pct <- 100 * d$rain_mm / stats::median(d$rain_mm[d$year %in% NORMAL_SEASONS]); d }))
idx <- stats::aggregate(pct ~ dataset + year, data = series, FUN = mean)
n_reg <- stats::aggregate(pct ~ dataset + year, data = series, FUN = length)
idx <- idx[n_reg$pct == nrow(regions), ]
series <- rbind(series, data.frame(dataset = idx$dataset, region_id = "ALL", year = idx$year,
                                   rain_mm = NA, pct = idx$pct))

# --- Statistics per region and dataset ----------------------------------------
reg_names <- c(stats::setNames(regions$region, regions$region_id), ALL = "All summer regions")
at <- function(d, col, y, digits = 0) { v <- d[[col]][d$year == y]; if (length(v) == 1) round(v, digits) else NA }
stat <- do.call(rbind, lapply(split(series, list(series$dataset, series$region_id), drop = TRUE), function(d) {
  v <- at(d, "pct", cur, 3)
  others <- d$pct[d$year != cur]
  data.frame(region_id = d$region_id[1], region = reg_names[[d$region_id[1]]], dataset = d$dataset[1],
             rain_mm = at(d, "rain_mm", cur, 1),
             pct_of_normal = round(v),
             rank_driest = if (!is.na(v)) 1L + sum(others < v) else NA,
             n_seasons = length(others) + !is.na(v),
             season_2015_pct = at(d, "pct", 2015))
}))
# Year-to-year agreement between the datasets (seasons before this one)
agree <- do.call(rbind, lapply(names(reg_names), function(id) {
  a <- series[series$region_id == id & series$year < cur, ]
  w <- stats::reshape(a[, c("dataset", "year", "pct")], idvar = "year", timevar = "dataset", direction = "wide")
  w <- w[stats::complete.cases(w), ]
  if (nrow(w) < 10) return(NULL)
  data.frame(region_id = id, r_power_chirps = round(stats::cor(w[[2]], w[[3]], method = "spearman"), 2),
             n_compared = nrow(w))
}))
stat <- merge(stat, agree, by = "region_id", all.x = TRUE)
stat$chirps_preliminary <- stat$dataset == "CHIRPS v3" & prelim_cur
ord <- c(regions$region_id, "ALL")
stat <- stat[order(match(stat$region_id, ord), stat$dataset), ]
utils::write.csv(stat, "data/processed/chirps_check_regions.csv", row.names = FALSE)

cat(sprintf("\nRain 1 October to 15 November %d: NASA POWER vs CHIRPS v3%s\n", cur,
            if (prelim_cur) " (CHIRPS this season: preliminary)" else ""))
print(stat[, c("region", "dataset", "rain_mm", "pct_of_normal", "rank_driest", "n_seasons",
               "r_power_chirps")], row.names = FALSE)
wc <- stat[stat$dataset == "CHIRPS v3" & !is.na(stat$rank_driest), ]
pc <- stat[stat$dataset == "NASA POWER" & !is.na(stat$rank_driest), ]
both1 <- intersect(wc$region[wc$rank_driest == 1], pc$region[pc$rank_driest == 1])
cat("\nDriest start on record in both datasets:", if (length(both1)) paste(both1, collapse = "; ") else "none", "\n")

# --- Chart ------------------------------------------------------------------
d <- stat[!is.na(stat$pct_of_normal), ]
d$region <- factor(d$region, levels = rev(reg_names[ord]))
d$lab <- ifelse(d$rank_driest <= 3,
                sprintf("%d%% (%s)", d$pct_of_normal, ifelse(d$rank_driest == 1, "driest",
                                                       paste(vapply(d$rank_driest, ordinal, ""), "driest"))),
                sprintf("%d%%", d$pct_of_normal))
cols <- c("NASA POWER" = SAIN$s1, "CHIRPS v3" = SAIN$ink2)
g <- ggplot(d, aes(pct_of_normal, region)) +
  geom_vline(xintercept = 100, colour = SAIN$axis, linetype = "22") +
  geom_line(aes(group = region), colour = SAIN$axis, linewidth = 1) +
  geom_point(aes(colour = dataset), size = 3.6) +
  geom_text(data = d[d$dataset == "CHIRPS v3", ], aes(label = lab), colour = SAIN$ink2,
            size = 3.6, vjust = -1.1) +
  geom_text(data = d[d$dataset == "NASA POWER", ], aes(label = lab), colour = SAIN$s1,
            size = 3.6, vjust = 2.1) +
  scale_colour_manual(values = cols) +
  scale_x_continuous(labels = scales::label_percent(scale = 1), limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Do two independent datasets tell the same story?",
       subtitle = sprintf(paste0("Rain from 1 October to 15 November %d as %% of the 1991-2020 median, with rank\n",
                                 "among %d seasons since 1981/82 where in the driest three. %s"),
                          cur, length(seasons), if (prelim_cur) "CHIRPS for this season is preliminary." else ""),
       x = NULL, y = NULL,
       caption = sain_caption("NASA POWER (MERRA-2 / GEOS-IT); CHIRPS v3 (Climate Hazards Center, UCSB)",
                              "CHIRPS: satellite estimates blended with rain gauges, 0.05 degree; mean of 0.25 degree around each site.")) +
  theme_sain() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = SAIN$grid, linewidth = 0.3),
        axis.text.y = element_text(colour = SAIN$ink2, size = rel(0.9)))
dir.create("outputs", showWarnings = FALSE)
ggplot2::ggsave("outputs/chirps_check.png", g, device = ragg::agg_png, width = 1400 / 150,
                height = 1000 / 150, dpi = 150, bg = SAIN$surface)
cat("Wrote data/processed/chirps_check_regions.csv and outputs/chirps_check.png\n")
