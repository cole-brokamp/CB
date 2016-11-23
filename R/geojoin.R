#' left join a data.frame to a spatial data frame
#'
#' modified from tigris::geo_join to not check data.frame names or coerce strings to factors
#'
#' @param spatial_data a spatial data frame
#' @param data_frame a data frame
#' @param by_sp column id for merge
#' @param by_df column id for merge
#'
#' @return spatial data frame
#' @export
geojoin <- function(spatial_data,data_frame,by_sp,by_df) {
  spatial_data@data <- data.frame(spatial_data@data,
                                  data_frame[match(spatial_data@data[[by_sp]],
                                                   data_frame[[by_df]]), ],
                                  check.names=FALSE,stringsAsFactors=FALSE)
  # remove duplicated column IDs if joined on same name variable
  if (by_sp == by_df) spatial_data@data[, !duplicated(names(spatial_data@data), fromLast = TRUE)]
  return(spatial_data)
}
