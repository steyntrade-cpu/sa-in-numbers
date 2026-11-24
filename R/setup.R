# Run once on a new machine: installs the R packages this project uses.
pkgs <- c("ggplot2", "ggrepel", "dplyr", "readr", "jsonlite", "httr", "scales",
          "rvest", "xml2", "ragg", "knitr", "rmarkdown", "terra")
# ERA5 temperatures are fetched by scripts/era5_arco.py (Python; GitHub runs it weekly)
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
message("All packages installed. Also install Quarto from https://quarto.org/docs/get-started/")
