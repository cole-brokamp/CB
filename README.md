## CB


[![DOI](https://zenodo.org/badge/21831/cole-brokamp/CB.svg)](https://zenodo.org/badge/latestdoi/21831/cole-brokamp/CB)
[![Travis-CI Build Status](https://travis-ci.org/cole-brokamp/CB.svg?branch=master)](https://travis-ci.org/cole-brokamp/CB)


My personal R package. Install this package within `R` by running `devtools::install_github('cole-brokamp/CB')`.

#### Highlighted functions:

- `cb_apply` - custom wrapper for sapply/lapply that returns an organized and properly named data.frame as a result. Also uses progress bars to update users on computation process, even when using parallel processing! Also includes intelligent error handling and locally caching the results for longer computations
- `geojoin` - join a data.frame to a spatial object
- `SPapply` - an `sapply` for `sp` spatial objects
- `switchv` - a vectorized version of `switch`
- `pretty_p` - pretty printing of p-values
- `percent` - pretty printing of decimals as percents
- `LatLongToFIPS` - takes lat/lon coordinate and returns the FIPS code by calling the Census Block Conversion API
- `ORGetter` - returns a nicely formatted table of odds ratios from a logistic regression model
- `save_pdf` - save graphics on current device to a pdf


#### Non-function features:

- my custom report and slides R Markdown templates
- an R Studio add in to render a R Markdown document in all three formats with one press of a button



