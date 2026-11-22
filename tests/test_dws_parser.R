# Quick check that the DWS scraper still understands the live page.
#   Rscript tests/test_dws_parser.R
# Downloads one drainage region (C = Vaal) and prints what was parsed.
source("R/fetch_dws.R")
x <- fetch_dws_region("C", raw_dir = tempdir())
if (is.null(x) || nrow(x) == 0) {
  stop("No dams parsed. Open https://www.dws.gov.za/Hydrology/Weekly/DrainageWeek.aspx?region=C ",
       "and compare the table headings with parse_dws_region().")
}
print(utils::head(x, 10))
cat("\nReport date:", as.character(x$report_date[1]), " Dams parsed:", nrow(x),
    "\nRegion storage:", round(weighted_storage(x$fsc_mcm, x$pct_this_week), 1), "% full\n")
