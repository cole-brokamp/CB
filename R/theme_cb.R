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
#` geom_point() +
#`   labs(title = "A Plot",
#`        subtitle = "Look, a subtitle!") +
#`   theme_cb()
#`
#` ggplot(mtcars, aes(wt, mpg)) +
#`   geom_point() +
#`   labs(title = "A Plot",
#`        subtitle = "Look, a subtitle!") +
#`   facet_grid(~vs) +
#`   theme_cb()
#`
#` ggplot(mtcars, aes(wt, mpg, color=factor(vs))) +
#`   geom_point() +
#`   labs(title = "A Plot",
#`        subtitle = "Look, a subtitle!") +
#`   theme_cb()
theme_cb <- function (base_size = 11, ...){
  out <- ggplot2::theme_light(base_family = "Arial", base_size = base_size)
  out <- out + ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                              panel.border = ggplot2::element_rect(fill = NA, colour = "grey20"),
                              panel.grid.major = ggplot2::element_line(colour = "grey92"),
                              panel.grid.minor = ggplot2::element_blank(),
                              strip.background = ggplot2::element_rect(fill = "grey92", colour = "grey20"),
                              strip.text = ggplot2::element_text(color='grey20'),
                              legend.key = ggplot2::element_rect(fill = "white", colour = NA),
                              complete = TRUE, ...)
  out$plot.title <- ggplot2::element_text(family = "Arial",
                                          face='bold',
                                          size = ggplot2::rel(1.2), hjust = 0, vjust = 1,
                                          margin = ggplot2::margin(b = base_size/2 * 1.2))
  out
}



