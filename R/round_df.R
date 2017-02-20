#' round all numeric columns in a data.frame
#'
#' @param x data.frame
#' @param digits number of digits after the decimal place
#'
#' @return rounded data.frame
#' @export
round_df <- function(x, digits) {
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
