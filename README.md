# SA in Numbers

South African data, with the uncertainty shown. Open data, open code.

This repository builds the SA in Numbers website (Quarto + R) and the social media
graphics that go with it. The first story is the **El Niño summer tracker 2026/27**.

## Folder layout

```
_quarto.yml                 website settings
index.qmd, about.qmd        home and methods pages
styles.scss                 website colours and the headline "stat" boxes
trackers/el-nino-2026/      the tracker page
update_el_nino.R            weekly script: fetch -> compute -> charts -> data files
update_sa_temperature.R     weekly script for the national temperature tracker
compare_chirps.R            check: NASA POWER rain vs CHIRPS rain
scripts/era5_arco.py        fetches ERA5 temperatures (Python; run weekly by GitHub)
stories/                    dated story pages (one folder each) + build_story_charts.R
config/regions.csv          the six climate regions (five summer-rainfall + Western Cape)
config/sites.csv            45 sites with their region (rain windows and heat grid points)
config/era5_sa_cells.csv    the 1,810 ERA5 grid cells inside South Africa (national temperature)
config/sa_grid.csv          114 points one degree apart (old NASA POWER grid; used by the ERA5 check)
R/
  setup.R                   installs the R packages (run once)
  theme_sain.R              house chart style + save_social() for all post sizes
  fetch_enso.R              NOAA RONI (El Niño index)
  fetch_era5.R              reads the saved ERA5 daily series (national mean, site maximum)
  fetch_power.R             site and region lists; NASA POWER download (now only for checks)
  fetch_chirps.R            CHIRPS v3 rain (pentads and months), cached in data/chirps/
  sa_temperature.R          South Africa's national daily temperature: data and charts
  fetch_dws.R               DWS weekly dam levels (scraped) + weekly archive
  metrics_el_nino.R         region season totals, normal range, hot days, headline numbers
  plots_el_nino.R           the charts
data/processed/             tidy CSVs behind every chart (published with the site)
data/archive/               dam history built up week by week
outputs/social/             PNGs: _story (9:16), _square (1:1), _wide (16:9), _web
outputs/canva_card.csv      one row of headline numbers for Canva Bulk Create
tests/make_test_fixtures.R  fake data in the real formats, for offline testing
tests/test_dws_parser.R     checks the dam scraper against the live DWS page
tests/check_regions.R       checks that each region's sites rise and fall together
.github/workflows/          optional weekly automatic update
```

## First run (on your computer)

1. Install **R** (4.3 or newer), **Quarto** (quarto.org) and **Git** or GitHub Desktop.
   RStudio or Positron is optional but handy.
2. Open a terminal in this folder and install the packages once:
   `Rscript R/setup.R`
3. Set up Copernicus access once (free): create an account at
   https://cds.climate.copernicus.eu, accept the ERA5 licence (CC BY 4.0) on any ERA5
   dataset page (Download tab, Terms of use), copy your Personal Access Token from your
   CDS profile and save it as the GitHub repository secret `CDS_PAT`.
4. Fetch the data and build everything. ERA5 temperatures come from
   `scripts/era5_arco.py` (Python), which the weekly GitHub workflow runs first; its first
   run reads every year since 1981 and commits `data/era5/`. On your computer, `git pull`
   to get it, then `Rscript update_sa_temperature.R` and `Rscript update_el_nino.R`.
   To run the Python step yourself: `pip install -r scripts/requirements.txt`, set the
   environment variable `CDS_PAT` to your token, then `python scripts/era5_arco.py`.
5. Check the regions hold together: `Rscript tests/check_regions.R`. Every region
   should say OK. It also writes `outputs/region_check_correlation.png`.
6. Check the dam data came through: open `data/processed/dams_summary_latest.csv`.
   If it is missing, the DWS page layout may have changed. See "If something breaks".
7. Look at the site locally: `quarto preview`
8. Publish: push to GitHub; the weekly workflow renders and publishes to GitHub Pages.

## Rain: CHIRPS v3

Rain comes from CHIRPS v3 (satellite estimates blended with rain gauges, ~5 km,
5-day totals). Until 21 November 2026 the tracker used NASA POWER rain; a check
(`compare_chirps.R`) showed that NASA POWER's near-real-time rain missed the
early-November storms, so rain was switched to CHIRPS. Heat moved to ERA5 on 22 November 2026.

- **First run:** downloads every October-April pentad since 1981 for the summer
  sites, and April-September monthly totals for the Western Cape (about 1,800
  files; a few hours, because the server limits request rates). It saves as it
  goes, so if it stops, run it again and it carries on.
- **Every week after that:** only the new pentads (1-2 files), plus swapping
  preliminary values for final ones when CHIRPS publishes them.
- The values are kept in `data/chirps/chirps_sites.csv`, which is committed to the
  repository so the weekly GitHub run doesn't download the history again.
- `Rscript compare_chirps.R` repeats the NASA POWER vs CHIRPS check (1 Oct - 15 Nov).

## South Africa's temperature tracker

```
Rscript update_sa_temperature.R
```

Daily mean temperature at 2 m from ECMWF ERA5, averaged over all 1,810 ERA5 grid cells
inside South Africa into one national series since 1981. `scripts/era5_arco.py` reads hourly
ERA5 straight from ECMWF's analysis-ready (ARCO) Zarr store (no request queue) and writes
daily values over South African days: the national series to
`data/era5/national_t2m_daily.csv` and the site daily maxima for the hot days to
`data/era5/sites_tmax_daily.csv.gz`. `R/fetch_era5.R` reads them. Page:
`trackers/sa-temperature/index.qmd`.

Until 22 November 2026 both temperature and hot days came from NASA POWER. Its latest
two to three months come from a different model (GEOS-IT) and read about 1.2 °C too warm
against ERA5 for October to mid-November 2026 (`tests/check_era5_temperature.R`), so both
moved to ERA5.

## Stories

Stories are dated snapshots, unlike the trackers. Each lives in `stories/<yyyy-mm-slug>/` with
its page (`index.qmd`), charts (`chart-*.png`, plus `social/` in three post sizes), its frozen
numbers (`data.csv`) and social captions (`_captions.md`; the underscore keeps Quarto from
publishing it).

- `Rscript stories/build_story_charts.R` draws the charts. The first run saves each story's
  numbers to `data.csv`; later runs redraw from that file, so weekly data updates never change a
  published story. Delete `data.csv` to rebuild a story from current data.
- New stories start with `draft: true`: `quarto preview` shows them, but the published site does
  not. Change it to `draft: false` and push to publish.
- `tests/check_era5_temperature.R` repeats a temperature ranking in ECMWF ERA5 (via Open-Meteo,
  ~10 minutes, needs internet) before a "warmest since 1981" claim goes out.

## Every week (Monday)

```
Rscript update_el_nino.R
quarto publish gh-pages
```

Then take the PNGs from `outputs/social/` into Canva. For a templated weekly card,
use Canva Bulk Create with `outputs/canva_card.csv`: map each column to a text box
in your template once, and reuse it every week.

Once this works by hand, you can switch on `.github/workflows/update-el-nino.yml`
(GitHub repo > Settings > Pages > source: `gh-pages` branch; the workflow then runs
every Monday 06:00 SAST). The social images are available as a download from each run.

## Testing without internet

```
Rscript tests/make_test_fixtures.R      # writes FAKE data into data/raw/
Rscript update_el_nino.R --offline
quarto render
```

Never publish charts made from the fixtures. Delete `data/raw/` and run online afterwards.

## Choices to check before publishing

- **Regions and sites** (`config/regions.csv`, `config/sites.csv`): six climate regions,
  5-12 sites each. After any change, run `Rscript tests/check_regions.R`: every region
  should say OK, and no site should move more with another region than its own. Keep
  the list fixed once the tracker is public, so the normals stay comparable.
- **Data sources**: rain CHIRPS v3 (satellite plus gauges, ~5 km); temperature ECMWF ERA5
  (reanalysis, ~25 km). Both are gridded estimates that suit comparing a season with its
  own past; SAWS station data would be the upgrade for local values. ERA5 needs the
  attribution line on every page that uses it (CC BY 4.0).
- **Drainage-region names** (`R/plots_el_nino.R`, `DRAINAGE_NAMES`): check against DWS.
- **Comparison seasons**: 1997/98 and 2015/16 (`COMPARE_SEASONS`), chosen before the
  season because they are the two very strong El Niño summers with opposite outcomes.

## If something breaks

- **NOAA, CHIRPS or Copernicus download fails:** usually temporary; rerun later. A
  Copernicus error that mentions a licence or returns 401/403 means the token is missing
  or the ERA5 licence has not been accepted on the CDS website (see First run, step 3). Rerun with
  `--offline` to rebuild from the last downloaded files.
- **DWS dam table not found:** run `Rscript tests/test_dws_parser.R`. If it fails, the DWS page changed. Open
  `https://www.dws.gov.za/Hydrology/Weekly/DrainageWeek.aspx?region=C` in a browser and
  compare the table headings with `parse_dws_region()` in `R/fetch_dws.R`. The rest of
  the tracker still builds without dam data.

## Licences

Code: MIT. Charts, text and processed data: CC BY 4.0. Source data: their publishers' terms.
