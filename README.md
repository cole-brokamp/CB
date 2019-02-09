## CB


[![DOI](https://zenodo.org/badge/21831/cole-brokamp/CB.svg)](https://zenodo.org/badge/latestdoi/21831/cole-brokamp/CB)
[![Travis-CI Build Status](https://travis-ci.org/cole-brokamp/CB.svg?branch=master)](https://travis-ci.org/cole-brokamp/CB)


My personal R package. Install this package within `R` by running `remotes::install_github('cole-brokamp/CB')`.

#### Highlighted functions:

- `mappp` - custom wrapper for `purrr::map()` that includes extras like parallel computation, progress bars, error handling, and local/remote caching
- `theme_cb` - custom theme for ggplot2 (plus `theme_map`, which adds more theme changes designed for `ggplot2` + `sf` maps)
- `round_df` - round all numeric columns in a data.frame
- `cchmc_colors` - access the Cincinnati Children's Hospital Medical Center color palette
- `geojoin` - join a data.frame to a spatial object
- `pretty_p` - pretty printing of p-values
- `percent` - pretty printing of decimals as percents
- `save_pdf` - save graphics on current device to a pdf, also can save 300 DPI jpeg if imagemagick is installed on system
- `htable` - quickly interactively view a data.frame object
- `hlist` - quickly interactively view nested list objects


#### Non-function features:

- my custom report and slides R Markdown templates
- an R Studio add in to render a R Markdown document as HTML, Word, and PDF file at the same time



