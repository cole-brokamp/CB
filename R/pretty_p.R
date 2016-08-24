#' pretty print a p-value
#'
#' @param x p-value
#' @param digits number of digits to show after the decimal place
#'
#' @export
#'
#' @examples
#' pretty_p(0.6770471)
#' pretty_p(0.00034)
#' pretty_p(0.00034,digits=4)

pretty_p <- function(x,digits=3) {
  p.rounded <- round(x,digits)
  if (p.rounded < 10^-digits) return(paste0('< 0.',paste(rep('0',digits-1),collapse=''),'1'))
  formatC(x,format='f',digits=digits)
}



