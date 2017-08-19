.onAttach <- function(...) {
  if (!interactive()) packageStartupMessage('setting CB::theme_bw() to ggplot default via ggplot2::set_theme()')
  ggplot2::theme_set(CB::theme_cb())
}

