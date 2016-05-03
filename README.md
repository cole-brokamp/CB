## CB


[![DOI](https://zenodo.org/badge/21831/cole-brokamp/CB.svg)](https://zenodo.org/badge/latestdoi/21831/cole-brokamp/CB)


My personal R package. Install this package within `R` by running `devtools::install_github('cole-brokamp/CB')`.

#### Highlighted functions:

- `CBapply` - wrapper for lapply and returning results as data frame with proper row names; `parallel::mclapply()` support and `plyr::rbind.fill` support
- `getPackages` - takes a package name and returns a list of its dependencies; useful for downloading source files of packages to install on a separate server running R where internet access is blocked
- `LatLongToFIPS` - takes lat/lon coordinate and returns the FIPS code by calling the Census Block Conversion API
- `ORGetter` - returns a nicely formatted table of odds ratios from a logistic regression model
- `save_pdf` - save graphics on current device to a pdf
- `SPapply` - an `sapply` for `sp` spatial objects
- `tableSummary` - make your "Table 1" for your next manuscript
- `tableTest` - make your "Table 2" for your next manuscript

#### Non-function features:

- my custom report and slides R Markdown templates
- an R Studio add in to render a R Markdown document in all three formats with one press of a button



