#' hlist
#' Interactive view of list structures with listviewer::jsonedit()
#'
#' @param l list to display
#' @param ... options sent through to listviewer::jsonedit()
#'
#' @export
#'
#' @examples
#' htable(mtcars)
ltable <- function(l,...) {
  listviewer::jsonedit(l,...)
}
