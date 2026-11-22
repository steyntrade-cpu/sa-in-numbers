# ---------------------------------------------------------------------------
# Dam levels: DWS Weekly State of Reservoirs, drainage-region pages
#   https://www.dws.gov.za/Hydrology/Weekly/DrainageWeek.aspx?region=<letter>
# Table columns: Dam | River | Photo | Indicators | FSC | This Week | Last Week | Last Year
# FSC = full supply capacity (million m3); the week columns are % full.
# Each page ends with a "Total" row, which is dropped (it would double the
# capacity) and used only as a check. "#" marks a reading carried over from an
# earlier week; it is kept and flagged in this_week_carried_over.
# The pages only show this week / last week / last year, so we keep our own
# weekly archive to build a history from the day the tracker starts.
# CHECK: page layout can change; re-run tests/test_dws_parser.R if scraping fails.
# ---------------------------------------------------------------------------

DWS_URL <- "https://www.dws.gov.za/Hydrology/Weekly/DrainageWeek.aspx?region=%s"
DWS_REGIONS <- c("A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L",
                 "M", "N", "P", "Q", "R", "S", "T", "U", "V", "W", "X")

DWS_UA <- httr::user_agent("Mozilla/5.0 (compatible; SA-in-Numbers tracker; weekly dam levels)")

# Download one region page; TRUE if it worked
download_dws_region <- function(region, path, tries = 2) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  for (k in seq_len(tries)) {
    resp <- tryCatch(httr::GET(sprintf(DWS_URL, region), DWS_UA, httr::timeout(45)),
                     error = function(e) NULL)
    if (!is.null(resp) && httr::status_code(resp) == 200) {
      writeBin(httr::content(resp, as = "raw"), path)
      Sys.sleep(1)  # be polite to the server
      return(TRUE)
    }
    if (k < tries) Sys.sleep(5)
  }
  FALSE
}

fetch_dws_region <- function(region, raw_dir = "data/raw/dws", offline = FALSE) {
  path <- file.path(raw_dir, paste0("region_", region, ".html"))
  if (!offline && !download_dws_region(region, path))
    warning("DWS region ", region, ": download failed",
            if (file.exists(path)) "; using the previous download" else "")
  if (!file.exists(path)) return(NULL)
  parse_dws_region(path, region)
}

parse_dws_region <- function(path, region) {
  page <- xml2::read_html(path)
  txt  <- rvest::html_text2(page)
  report_date <- as.Date(regmatches(txt, regexpr("\\d{4}-\\d{2}-\\d{2}", txt)))
  if (length(report_date) == 0) report_date <- as.Date(NA)
  # Some regions (e.g. F, Namaqualand) have no listed dams
  if (grepl("No data available", txt, ignore.case = TRUE)) {
    message("DWS region ", region, ": no dams listed")
    return(NULL)
  }

  tables <- rvest::html_table(page, fill = TRUE)
  # pick the table that has an FSC column
  hit <- which(vapply(tables, function(t) any(grepl("^FSC", names(t))) ||
                        any(grepl("^FSC", unlist(t[1, ]))), logical(1)))
  if (length(hit) == 0) { warning("DWS region ", region, ": no dam table found"); return(NULL) }
  # if several tables match (e.g. nested layout tables), use the longest
  hit <- hit[which.max(vapply(tables[hit], nrow, integer(1)))]
  t <- as.data.frame(tables[[hit]], stringsAsFactors = FALSE)
  if (!any(grepl("^FSC", names(t)))) { names(t) <- unlist(t[1, ]); t <- t[-1, ] }

  col <- function(pattern) t[[grep(pattern, names(t), ignore.case = TRUE)[1]]]
  num <- function(x) suppressWarnings(readr::parse_number(as.character(x)))
  this_week_raw <- as.character(col("This Week"))
  out <- data.frame(
    region = region,
    report_date = report_date,
    dam = trimws(col("^Dam")),
    fsc_mcm = num(col("^FSC")),
    pct_this_week = num(this_week_raw),
    pct_last_week = num(col("Last Week")),
    pct_last_year = num(col("Last Year")),
    # DWS marks a reading carried over from an earlier week with "#"
    this_week_carried_over = grepl("#", this_week_raw, fixed = TRUE),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$fsc_mcm) & nzchar(out$dam), ]

  # DWS adds a "Total" row to each region. Drop it (it would double-count
  # capacity) but use it to check our own capacity-weighted figure.
  is_total <- grepl("^total", out$dam, ignore.case = TRUE)
  if (any(is_total)) {
    tot <- out[is_total, ][1, ]
    out <- out[!is_total, ]
    ours <- weighted_storage(out$fsc_mcm, out$pct_this_week)
    # Tiny regions (a dam or two) differ by rounding in DWS's own total, so
    # only check regions of 50 million m3 or more.
    if (tot$fsc_mcm >= 50 && (abs(ours - tot$pct_this_week) > 0.5 ||
                              abs(sum(out$fsc_mcm) - tot$fsc_mcm) > 0.01 * tot$fsc_mcm))
      warning(sprintf("DWS region %s: our total (%.1f%% of %.0f) differs from DWS's (%.1f%% of %.0f)",
                      region, ours, sum(out$fsc_mcm), tot$pct_this_week, tot$fsc_mcm))
  }
  out
}

fetch_dws_all <- function(regions = DWS_REGIONS, offline = FALSE, raw_dir = "data/raw/dws") {
  # If DWS stops answering (two regions in a row fail), stop trying and use the
  # previous download for the rest. DWS publishes weekly, and the report date
  # on each page shows which week the figures are from.
  fails <- 0
  rows <- lapply(regions, function(r) {
    message("DWS: region ", r)
    path <- file.path(raw_dir, paste0("region_", r, ".html"))
    if (!offline && fails < 2) {
      ok <- download_dws_region(r, path)
      fails <<- if (ok) 0 else fails + 1
      if (!ok) warning("DWS region ", r, ": download failed",
                       if (file.exists(path)) "; using the previous download" else "")
      if (fails == 2) warning("DWS is not responding; using previous downloads for the remaining regions")
    }
    if (!file.exists(path)) return(NULL)
    parse_dws_region(path, r)
  })
  do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
}

# FSC-weighted storage (% full) for a set of dams, ignoring dams without data
weighted_storage <- function(fsc, pct) {
  ok <- !is.na(fsc) & !is.na(pct)
  100 * sum(fsc[ok] * pct[ok] / 100) / sum(fsc[ok])
}

summarise_dams <- function(dams) {
  dams <- dams[!grepl("^total", dams$dam, ignore.case = TRUE), ]  # belt and braces
  by_region <- do.call(rbind, lapply(split(dams, dams$region), function(d) data.frame(
    region = d$region[1], report_date = max(d$report_date, na.rm = TRUE),
    n_dams = nrow(d), fsc_mcm = sum(d$fsc_mcm, na.rm = TRUE),
    pct_this_week = weighted_storage(d$fsc_mcm, d$pct_this_week),
    pct_last_week = weighted_storage(d$fsc_mcm, d$pct_last_week),
    pct_last_year = weighted_storage(d$fsc_mcm, d$pct_last_year))))
  national <- data.frame(
    region = "National", report_date = max(dams$report_date, na.rm = TRUE),
    n_dams = nrow(dams), fsc_mcm = sum(dams$fsc_mcm, na.rm = TRUE),
    pct_this_week = weighted_storage(dams$fsc_mcm, dams$pct_this_week),
    pct_last_week = weighted_storage(dams$fsc_mcm, dams$pct_last_week),
    pct_last_year = weighted_storage(dams$fsc_mcm, dams$pct_last_year))
  rbind(national, by_region)
}

# Append this week's summary to the archive (one row per region per report date)
update_dam_archive <- function(summary, path = "data/archive/dams_weekly.csv") {
  if (file.exists(path)) {
    old <- readr::read_csv(path, show_col_types = FALSE)
    old <- old[!(old$report_date %in% summary$report_date), ]
    summary <- rbind(as.data.frame(old), summary)
  }
  summary <- summary[order(summary$report_date, summary$region), ]
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(summary, path)
  invisible(summary)
}
