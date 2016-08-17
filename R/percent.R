#' paste a decimal as a pretty percent
#'
#' @param x decimal
#' @param digits number of digits to show after the decimal place
#'
#' @export
#'
#' @examples
#' percent(0.6770471)

percent <- function(x,digits=1) {
  paste0(formatC(100 * x,format='f',digits=digits),"%")
}


