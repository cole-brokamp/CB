#' Custom CB ggplot2 theme
#'
#' @param base_size base font size
#' @param ... other arguments passed to \code{ggplot2::theme_*()}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#' geom_point() +
#'  labs(title = "A Plot",
#'       subtitle = "Look, a subtitle!") +
#'  theme_cb()
#'
theme_cb <- function(base_size = 11, ...) {
  out <- ggplot2::theme_minimal(base_family = "RobotoCondensed-Regular",
                                base_size = base_size, ...)
  out$plot.title <- ggplot2::element_text(family="Roboto-Bold",
                                          size = ggplot2::rel(1.2),
                                          hjust = 0, vjust = 1,
                                          margin = ggplot2::margin(b = base_size / 2 * 1.2)
  )
  out
}
