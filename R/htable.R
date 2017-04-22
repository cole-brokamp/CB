#' htable
#' Create a quick html table using my favorites options for DT::datatable()
#'
#' @param df data.frame to display
#' @param ... options sent through to DT::datatable
#'
#' @export
#'
#' @examples
#' htable(mtcars)
htable <- function(df,...) {
  DT::datatable(df,
                caption = as.character(deparse(substitute(df))),
                rownames = FALSE,
                filter='top',
                class = 'cell-border stripe',
                options=list(paging = TRUE,
                             searchHighlight = TRUE,
                             pageLength = 25,
                             searchDelay = 500,
                             orderClasses = TRUE,
                             buttons = c('copy','csv','colvis'),
                             dom = 'i l f r t B',
                             colReorder = TRUE,
                             scroller = TRUE,
                             scrollX = TRUE,
                             fixedColumns = TRUE),
                extensions=c('Buttons','Scroller','ColReorder'),
                fillContainer = TRUE,
                autoHideNavigation = TRUE,
                ...)
}
