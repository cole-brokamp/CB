#' Vectors of colors for figures
#'
#' Creates different vectors of related colors that may be useful for figures.
#'
#' @param set Character string indicating a set of colors.
#' @return Vector of character strings representing the chosen set of colors, in RGB.
#' @export
#' @importFrom grDevices rgb2hsv
#' @keywords utilities
#' @examples
#' plot(1:4,rep(1,4),col=cb_colors('cchmc'),pch=19,cex=5)
#' plot(1:16,rep(1,16),col=cb_colors('web'),pch=19,cex=5)
cb_colors <- function(set=c('web','cchmc')) {

  web <- c(navy="#001f3f",  # from http://clrs.cc
           blue="#0074d9",
           aqua="#7fdbff",
           teal="#39cccc",
           olive="#3d9970",
           green="#2ecc40",
           lime="#01ff70",
           yellow="#ffdc00",
           orange="#ff851b",
           red="#ff4136",
           maroon="#85144b",
           fuchsia="#f012be",
           purple="#b10dc9",
           black="#111111",
           gray="#aaaaaa",
           silver="#dddddd")

  cchmc <- c(purple='#9E4679',
             green='#76BC44',
             blue='#02AAC8',
             grey='#55575A')

  switch(match.arg(set),
         cchmc=cchmc,
         web=web)
}



