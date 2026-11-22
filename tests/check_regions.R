# ---------------------------------------------------------------------------
# Do the regions in config/ hold together climatically?
#   Rscript update_el_nino.R        (downloads the site data first)
#   Rscript tests/check_regions.R
#
# Uses each site's CHIRPS seasonal rain totals (1 Oct - 30 Apr; 1 Apr - 30 Sep
# for the winter-rainfall region) since 1981 - the same rain data as the
# tracker - and checks that:
#   1. each site moves with its own region more than with any other region
#      (Spearman correlation of seasonal totals between sites);
#   2. a blind cluster analysis (average linkage on 1 - r) finds similar groups;
#   3. the season really is that region's wet season (share of annual rain).
# Also reports how strongly each region's summer rain tracks El Nino (RONI).
# Annual rainfall and the summer share (seasonality) come from NASA POWER, as
# CHIRPS is only downloaded for each region's own season.
# Writes: data/processed/region_check_sites.csv, region_check_regions.csv,
#         outputs/region_check_correlation.png
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
for (f in c("R/theme_sain.R", "R/fetch_power.R", "R/fetch_chirps.R", "R/fetch_enso.R", "R/metrics_el_nino.R")) source(f)

sites <- read_sites(); regions <- read_regions()
power <- suppressWarnings(fetch_power_all(sites, offline = TRUE))
sites <- sites[sites$site %in% unique(power$site), ]
regions <- regions[regions$region_id %in% sites$region_id, ]
p <- with_regions(power[!is.na(power$rain_mm), c("site", "date", "rain_mm")], sites)
p$year <- as.integer(format(p$date, "%Y")); p$month <- as.integer(format(p$date, "%m"))

# Seasonality, 1991-2020
base <- p[p$year %in% NORMAL_SEASONS, ]
ann <- tapply(base$rain_mm, base$site, sum) / length(NORMAL_SEASONS)
oct_apr <- tapply(base$rain_mm[base$month %in% c(10:12, 1:4)], base$site[base$month %in% c(10:12, 1:4)], sum) /
  length(NORMAL_SEASONS)

# CHIRPS rain, as used by the tracker (from the local cache)
pc <- fetch_rain_chirps(sites, read_regions(), offline = TRUE)
pc <- pc[pc$site %in% sites$site, c("site", "date", "rain_mm")]

# Seasonal totals per site, complete seasons only
season_totals <- function(type) {
  d <- add_season(pc, type)
  n <- stats::aggregate(rain_mm ~ site + season, data = d, FUN = length)
  t <- stats::aggregate(rain_mm ~ site + season, data = d, FUN = sum)
  t <- t[n$rain_mm >= SEASONS[[type]]$length, ]
  w <- stats::reshape(t, idvar = "season", timevar = "site", direction = "wide")
  names(w) <- sub("^rain_mm\\.", "", names(w))
  w <- w[order(w$season), ]; rownames(w) <- w$season; w$season <- NULL
  w[stats::complete.cases(w), , drop = FALSE]
}
sum_ids <- regions$region_id[regions$season == "summer"]
win_ids <- regions$region_id[regions$season == "winter"]
Ws <- season_totals("summer")[, sites$site[sites$region_id %in% sum_ids], drop = FALSE]
C <- stats::cor(Ws, method = "spearman")

region_of <- stats::setNames(sites$region_id, sites$site)
site_rows <- lapply(colnames(C), function(s) {
  own <- setdiff(names(region_of)[region_of == region_of[s]], s)
  own <- own[own %in% colnames(C)]
  by_reg <- vapply(sum_ids, function(r) {
    m <- setdiff(names(region_of)[region_of == r], s); m <- m[m %in% colnames(C)]
    if (length(m)) mean(C[s, m]) else NA_real_
  }, numeric(1))
  other <- by_reg[names(by_reg) != region_of[s]]
  data.frame(site = s, region_id = region_of[[s]],
             annual_mm = round(ann[[s]]), oct_apr_share = round(100 * oct_apr[[s]] / ann[[s]]),
             r_own_region = if (length(own)) round(mean(C[s, own]), 2) else NA,
             best_other_region = if (all(is.na(other))) NA else names(which.max(other)),
             r_best_other = if (all(is.na(other))) NA else round(max(other, na.rm = TRUE), 2))
})
site_tab <- do.call(rbind, site_rows)
site_tab$fits <- with(site_tab, is.na(r_own_region) | is.na(r_best_other) | r_own_region >= r_best_other)

# Winter region: correlation of Apr-Sep totals among its own sites
win_sites <- sites$site[sites$region_id %in% win_ids]
if (length(win_sites) >= 2) {
  Ww <- season_totals("winter")[, win_sites, drop = FALSE]
  Cw <- stats::cor(Ww, method = "spearman")
}
for (s in win_sites) {
  site_tab <- rbind(site_tab, data.frame(
    site = s, region_id = region_of[[s]], annual_mm = round(ann[[s]]),
    oct_apr_share = round(100 * oct_apr[[s]] / ann[[s]]),
    r_own_region = if (length(win_sites) >= 2) round(mean(Cw[s, setdiff(win_sites, s)]), 2) else NA,
    best_other_region = NA, r_best_other = NA, fits = NA))
}

# Region summary, incl. El Nino link (Spearman r of region-mean seasonal total
# with RONI: NDJ for summer regions, JJA for the winter region)
roni <- parse_roni("data/raw/RONI.ascii.txt")
reg_rows <- lapply(seq_len(nrow(regions)), function(i) {
  r <- regions[i, ]; m <- sites$site[sites$region_id == r$region_id]
  W <- if (r$season == "summer") Ws[, m, drop = FALSE] else season_totals("winter")[, m, drop = FALSE]
  Cr <- if (r$season == "summer") C[m, m, drop = FALSE] else stats::cor(W, method = "spearman")
  within <- if (length(m) > 1) mean(Cr[upper.tri(Cr)]) else NA
  between <- if (r$season == "summer") {
    o <- colnames(C)[!colnames(C) %in% m]
    if (length(o)) max(vapply(setdiff(sum_ids, r$region_id), function(q) {
      mm <- intersect(names(region_of)[region_of == q], colnames(C))
      if (length(mm)) mean(C[m, mm]) else NA_real_ }, numeric(1)), na.rm = TRUE) else NA
  } else NA
  idx <- rowMeans(W)
  rs <- roni[roni$season == if (r$season == "summer") "NDJ" else "JJA", ]
  yrs <- intersect(as.integer(names(idx)), rs$year)
  enso <- stats::cor(idx[as.character(yrs)], rs$roni[match(yrs, rs$year)], method = "spearman")
  data.frame(region_id = r$region_id, region = r$region, season = r$season, n_sites = length(m),
             annual_mm = round(mean(ann[m])), season_share = round(100 * mean(if (r$season == "summer")
               oct_apr[m] / ann[m] else 1 - oct_apr[m] / ann[m])),
             r_within = round(within, 2), r_nearest_other = round(between, 2),
             enso_r = round(enso, 2),
             verdict = if (is.na(within)) "too few sites" else if (within >= 0.6 && (is.na(between) || within > between)) "OK" else "CHECK")
})
reg_tab <- do.call(rbind, reg_rows)

# Blind clustering of the summer sites
k <- length(sum_ids)
if (ncol(C) > k) {
  hc <- stats::hclust(stats::as.dist(1 - C), method = "average")
  cl <- stats::cutree(hc, k = k)
  site_tab$cluster <- cl[site_tab$site]
  cat("\nBlind clusters (rows) vs assigned regions (columns):\n")
  print(table(cluster = cl, region = region_of[names(cl)]))
}

utils::write.csv(site_tab, "data/processed/region_check_sites.csv", row.names = FALSE)
utils::write.csv(reg_tab, "data/processed/region_check_regions.csv", row.names = FALSE)
cat("\nRegions:\n"); print(reg_tab, row.names = FALSE)
cat("\nSites that move more with another region than their own:\n")
bad <- site_tab[!is.na(site_tab$fits) & !site_tab$fits, ]
if (nrow(bad)) print(bad, row.names = FALSE) else cat("none\n")

# Correlation heatmap, sites ordered by region
ord <- sites$site[sites$region_id %in% sum_ids]
ord <- ord[order(match(region_of[ord], sum_ids))]
ord <- ord[ord %in% colnames(C)]
hm <- expand.grid(a = ord, b = ord, stringsAsFactors = FALSE)
hm$r <- C[cbind(hm$a, hm$b)]
hm$a <- factor(hm$a, levels = ord); hm$b <- factor(hm$b, levels = rev(ord))
g <- ggplot(hm, aes(a, b, fill = r)) + geom_tile(colour = SAIN$surface) +
  scale_fill_gradient(low = SAIN$surface, high = SAIN$s1, limits = c(0, 1), name = "r",
                      oob = scales::squish) +
  labs(title = "Do the regions hold together?",
       subtitle = "Spearman correlation of 1 Oct - 30 Apr CHIRPS rain totals between sites, 1981/82 onwards.\nSites ordered by region; strong blocks on the diagonal mean coherent regions. r below 0 shown as 0.",
       x = NULL, y = NULL, caption = sain_caption("CHIRPS v3 (Climate Hazards Center, UCSB)")) +
  theme_sain(base_size = 11) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "right",
        panel.grid.major.y = element_blank())
dir.create("outputs", showWarnings = FALSE)
ggplot2::ggsave("outputs/region_check_correlation.png", g, device = ragg::agg_png,
                width = 11, height = 10, dpi = 120, bg = SAIN$surface)
cat("\nWrote data/processed/region_check_*.csv and outputs/region_check_correlation.png\n")
