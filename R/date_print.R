#' Print the current date in a pretty format
#'
#' @return string
#' @export
#'
#' @examples
#' date_print()

date_print <- function() {
  d <- date()
  month <- substr(d,5,7)
  day <- substr(d,9,10)
  year <- substr(d,21,24)
  paste(day,month,year)
}
