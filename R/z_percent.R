#' paste a decimal as a pretty percent
#'
#' @param x decimal
#' @param digits number of digits to show after the decimal place
#' @param paren logical; print with surrounding parentheses?
#'
#' @export
#'
#' @examples
#' \dontrun{
#' percent(0.6770471)
#' percent(0.6770471, digits=0, paren=TRUE)
#' }

percent <- function(x, digits=1, paren=FALSE) {
    .Deprecated("please use `scales::percent()` instead of `CB::percent()`")
  out <- paste0(formatC(100 * x,format='f',digits=digits),"%")
  if (paren) out <- paste0('(', out, ')')
  return(out)
}


