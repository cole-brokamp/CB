.onAttach <- function(...) {
  if (!interactive()) return()
  packageStartupMessage('setting CB::theme_bw() to ggplot default via ggplot2::set_theme()')
  theme_set(CB::theme_cb())
}

