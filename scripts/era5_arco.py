#!/usr/bin/env python3
"""
ERA5 daily temperature for South Africa, read directly from ECMWF's
analysis-ready (ARCO) Zarr store. There is no request queue: the data are read
as they are stored.

Store: ERA5 hourly data on single levels, geo-chunked (built for long time
series over small areas). Hourly 2 m temperature, 0.25 degrees, updated daily
about 5 days behind real time; the latest 2-3 months are ERA5T (preliminary).
Access needs a Copernicus Climate Data Store (CDS) Personal Access Token in the
environment variable CDS_PAT (GitHub: the CDS_PAT secret). ERA5 licence: CC BY 4.0.

Writes the two files the trackers read (R/fetch_era5.R):
  data/era5/national_t2m_daily.csv    date, t_c, n_points
      daily mean over all ERA5 cells inside South Africa (config/era5_sa_cells.csv),
      weighted by cos(latitude)
  data/era5/sites_tmax_daily.csv.gz   site, date, tmax_c
      daily maximum at the grid point nearest each site (config/sites.csv)
Days run midnight to midnight South African time (UTC+2), from 24 hourly values;
an incomplete last day is left out.

Usage (from the project folder):
  python scripts/era5_arco.py            # first run: 1981 onward; then this year (and last year until April)
  python scripts/era5_arco.py --start 2026-01-01
  python scripts/era5_arco.py --info     # show the store's layout and stop
"""
import argparse
import datetime as dt
import gzip
import os
import sys
import time

import numpy as np
import pandas as pd
import xarray as xr

URL = os.environ.get(
    "ERA5_ARCO_URL",
    "https://arco.datastores.ecmwf.int/cadl-arco-geo-002/arco/reanalysis_era5_single_levels/sfc/geoChunked.zarr")
FIRST = os.environ.get("ERA5_FIRST_DATE", "1981-01-01")
BOX = dict(north=-22.0, south=-35.0, west=16.0, east=33.0)
NAT = "data/era5/national_t2m_daily.csv"
SITES = "data/era5/sites_tmax_daily.csv.gz"
CELLS = "config/era5_sa_cells.csv"
SITES_CFG = "config/sites.csv"
TZ = np.timedelta64(2, "h")              # South African Standard Time
MAX_BLOCK = 40_000_000                   # values read at once (~160 MB as float32)


def log(*a):
    print(time.strftime("%H:%M:%S"), *a, flush=True)


def token():
    for v in ("CDS_PAT", "ecmwfr_PAT"):
        if os.environ.get(v):
            return os.environ[v].strip()
    rc = os.path.expanduser("~/.cdsapirc")
    if os.path.exists(rc):
        for line in open(rc):
            if line.strip().startswith("key:"):
                return line.split(":", 1)[1].strip()
    sys.exit("No Copernicus token: set CDS_PAT (GitHub: repository secret CDS_PAT).")


def open_store(url, tok):
    kw = dict(consolidated=True, chunks=None)
    if url.startswith("http"):
        kw["storage_options"] = {"headers": {"Authorization": f"Bearer {tok}"}}
    try:
        return xr.open_zarr(url, **kw)
    except Exception as e:                                   # e.g. no consolidated metadata
        log("open with consolidated metadata failed:", repr(e)[:200], "- retrying without")
        kw["consolidated"] = False
        return xr.open_zarr(url, **kw)


def pick(ds):
    """Name of the 2 m temperature variable and of its time/lat/lon dimensions."""
    var = next((v for v in ("t2m", "2m_temperature", "2t") if v in ds.data_vars), None)
    if var is None:
        var = next((v for v in ds.data_vars
                    if "2 metre temperature" in str(ds[v].attrs.get("long_name", "")).lower()), None)
    if var is None:
        sys.exit(f"No 2 m temperature in the store; variables: {list(ds.data_vars)[:30]}")
    dims = ds[var].dims
    tdim = next(d for d in dims if "time" in d)
    ydim = next(d for d in dims if d.startswith("lat"))
    xdim = next(d for d in dims if d.startswith("lon"))
    return var, tdim, ydim, xdim


def chunk_sizes(da, dims):
    enc = da.encoding.get("preferred_chunks") or {}
    ch = da.encoding.get("chunks")
    out = {}
    for i, d in enumerate(da.dims):
        out[d] = enc.get(d) or (ch[i] if ch else da.shape[i])
    return [out[d] for d in dims]


def read_retry(da, sel, tries=4):
    for k in range(tries):
        try:
            return np.asarray(da.isel(sel).values, dtype="float32")
        except Exception as e:
            if k == tries - 1:
                raise
            wait = 20 * (k + 1)
            log(f"read failed ({repr(e)[:150]}); retrying in {wait} s")
            time.sleep(wait)


def caches_complete(first_year, last_full_year, n_sites):
    if not (os.path.exists(NAT) and os.path.exists(SITES)):
        return False
    nat = pd.read_csv(NAT, parse_dates=["date"])
    st = pd.read_csv(SITES, parse_dates=["date"])
    ny = nat["date"].dt.year.value_counts()
    sy = st["date"].dt.year.value_counts()
    return all(ny.get(y, 0) >= 365 and sy.get(y, 0) >= 365 * n_sites
               for y in range(first_year, last_full_year + 1))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--start", help="first date (YYYY-MM-DD); default: automatic")
    ap.add_argument("--end", help="last date (YYYY-MM-DD); default: latest available")
    ap.add_argument("--info", action="store_true", help="print the store's layout and stop")
    a = ap.parse_args()

    t_start = time.time()
    ds = open_store(URL, token())
    var, tdim, ydim, xdim = pick(ds)
    da = ds[var]
    cT, cY, cX = chunk_sizes(da, (tdim, ydim, xdim))
    times = ds[tdim].values.astype("datetime64[h]")
    lats = ds[ydim].values.astype(float)
    lons = ds[xdim].values.astype(float)
    lons_e = np.where(lons > 180, lons - 360, lons)
    log(f"store: {var} {dict(zip(da.dims, da.shape))}; chunks time={cT} lat={cY} lon={cX}; "
        f"{times[0]} to {times[-1]} UTC")
    if a.info:
        print(ds)
        return

    sites = pd.read_csv(SITES_CFG)
    cells = pd.read_csv(CELLS)
    today = dt.date.today()
    if a.start:
        start = np.datetime64(a.start, "D")
    else:
        first_year = int(FIRST[:4])
        if caches_complete(first_year, today.year - 1, len(sites)):
            y = today.year - 1 if today.month <= 3 else today.year     # ERA5T -> final until April
            start = np.datetime64(f"{y}-01-01", "D")
        else:
            start = np.datetime64(FIRST, "D")
            log("history not complete yet: reading everything from", FIRST)
    end = np.datetime64(a.end, "D") if a.end else (times[-1] + TZ).astype("datetime64[D]")

    # hours that make up SAST days start..end
    t0 = np.datetime64(start, "h") - TZ
    t1 = np.datetime64(end + np.timedelta64(1, "D"), "h") - TZ - np.timedelta64(1, "h")
    i0, i1 = np.searchsorted(times, t0), np.searchsorted(times, t1, side="right")
    if i1 <= i0:
        sys.exit(f"No hours between {start} and {end} in the store")
    tsel = times[i0:i1]
    day = ((tsel + TZ).astype("datetime64[D]") - start).astype(int)   # day index of every hour
    ndays = int(day.max()) + 1
    hours_per_day = np.bincount(day, minlength=ndays)

    # South Africa box, cells and sites
    yi = np.where((lats <= BOX["north"] + 1e-6) & (lats >= BOX["south"] - 1e-6))[0]
    xi = np.where((lons_e >= BOX["west"] - 1e-6) & (lons_e <= BOX["east"] + 1e-6))[0]
    ykey = {round(v, 3): i for i, v in enumerate(lats)}
    xkey = {round(v, 3): i for i, v in enumerate(lons_e)}
    cell_y = np.array([ykey.get(round(v, 3), -1) for v in cells["lat"]])
    cell_x = np.array([xkey.get(round(v, 3), -1) for v in cells["lon"]])
    ok = (cell_y >= 0) & (cell_x >= 0)
    if ok.mean() < 0.99:
        log(f"warning: only {ok.sum()} of {len(cells)} South African cells found on the grid")
    cell_y, cell_x, cell_w = cell_y[ok], cell_x[ok], cells["weight"].values[ok]
    site_y = np.array([int(np.argmin(np.abs(lats - v))) for v in sites["lat"]])
    site_x = np.array([int(np.argmin(np.abs(lons_e - v))) for v in sites["lon"]])

    wsum = np.zeros(ndays)                         # sum over cells and hours of w * T
    smax = np.full((len(sites), ndays), -np.inf)
    y_blocks = range(yi.min() // cY * cY, yi.max() + 1, cY)
    x_blocks = range(xi.min() // cX * cX, xi.max() + 1, cX)
    n_tiles = len(y_blocks) * len(x_blocks)
    log(f"reading {start} to {end}: {i1 - i0} hours, {n_tiles} tile(s) of {cY}x{cX} grid cells")

    k = 0
    for yb in y_blocks:
        for xb in x_blocks:
            k += 1
            ya, yz = max(yb, yi.min()), min(yb + cY, yi.max() + 1)
            xa, xz = max(xb, xi.min()), min(xb + cX, xi.max() + 1)
            in_c = (cell_y >= ya) & (cell_y < yz) & (cell_x >= xa) & (cell_x < xz)
            in_s = (site_y >= ya) & (site_y < yz) & (site_x >= xa) & (site_x < xz)
            if not in_c.any() and not in_s.any():
                continue
            cells_per_t = (yz - ya) * (xz - xa)
            step = max(cT, (MAX_BLOCK // max(cells_per_t, 1)) // cT * cT)
            tb = (i0 // cT) * cT
            while tb < i1:
                ta, tz = max(tb, i0), min(tb + step, i1)
                a3 = read_retry(da, {tdim: slice(ta, tz), ydim: slice(ya, yz), xdim: slice(xa, xz)})
                if a3.ndim != 3:
                    sys.exit("unexpected array shape")
                order = [da.dims.index(d) for d in (tdim, ydim, xdim)]
                a3 = np.transpose(a3, order)
                if np.nanmedian(a3) > 150:
                    a3 = a3 - 273.15                              # Kelvin -> Celsius
                d_idx = day[ta - i0:tz - i0]
                if in_c.any():
                    vals = a3[:, cell_y[in_c] - ya, cell_x[in_c] - xa]      # hours x cells
                    np.add.at(wsum, d_idx, vals @ cell_w[in_c])
                for s in np.where(in_s)[0]:
                    np.maximum.at(smax[s], d_idx, a3[:, site_y[s] - ya, site_x[s] - xa])
                tb += step
            log(f"tile {k}/{n_tiles} done ({time.time() - t_start:.0f} s so far)")

    full = hours_per_day == 24
    dates = start + np.arange(ndays).astype("timedelta64[D]")
    nat_new = pd.DataFrame({
        "date": dates[full],
        "t_c": np.round(wsum[full] / (cell_w.sum() * 24), 3),
        "n_points": int(ok.sum())})
    nat_new = nat_new[np.isfinite(nat_new["t_c"])]
    st_new = pd.concat([pd.DataFrame({"site": sites["site"][s], "date": dates[full],
                                      "tmax_c": np.round(smax[s][full], 2)}) for s in range(len(sites))])
    st_new = st_new[np.isfinite(st_new["tmax_c"])]

    # merge into the caches: new values replace the same dates
    os.makedirs(os.path.dirname(NAT), exist_ok=True)
    if os.path.exists(NAT):
        old = pd.read_csv(NAT, parse_dates=["date"])
        nat_new = pd.concat([old[~old["date"].isin(nat_new["date"])], nat_new])
    if os.path.exists(SITES):
        old = pd.read_csv(SITES, parse_dates=["date"])
        key = lambda d: d["site"].astype(str) + "|" + d["date"].astype(str)
        st_new = pd.concat([old[~key(old).isin(set(key(st_new)))], st_new])
    nat_new = nat_new.sort_values("date")
    st_new = st_new.sort_values(["site", "date"])
    nat_new.assign(date=nat_new["date"].dt.strftime("%Y-%m-%d")).to_csv(NAT, index=False)
    with gzip.open(SITES, "wt", newline="") as f:
        st_new.assign(date=st_new["date"].dt.strftime("%Y-%m-%d")).to_csv(f, index=False)
    log(f"saved {NAT} ({len(nat_new)} days, to {nat_new['date'].max():%Y-%m-%d}) and {SITES} "
        f"({len(st_new)} rows) in {time.time() - t_start:.0f} s")


if __name__ == "__main__":
    main()
