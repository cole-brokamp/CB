#' SPapply
#'
#' *apply function for spatial point objects
#' @param sp.object a SpatialPoints or SpatialPointsDataFrame object
#' @param FUN. function to be applied; must take sp.object as first argument and must return a data.frame
#' @param ... additional arguments passed to function
#' @param id.row.names if \code{TRUE}, set row.names of output data.frame from \code{data$id} of \code{sp.object}
#'
#' @return data.frame of all results from function applied to sp.object
#' @export
#'
SPapply <- function(sp.object,FUN.,...,id.row.names=FALSE) {
  # require(pbapply)
  stopifnot(class(sp.object) %in% c('SpatialPoints','SpatialPointsDataFrame'))
  N. <- nrow(sp.object@coords)
  FUN <- match.fun(FUN.)
  pbapply::pb <- startpb(0,N.)
  out <- vector('list',N.)
  for (j in 1:N.) {
    out[[j]] <- FUN(sp.object[j, ],...)
    pbapply::setpb(pb,j)
  }
  pbapply::close(pb)
  out <- do.call(rbind,out)
  if(id.row.names) row.names(out) <- as.character(sp.object@data$id)
  return(out)
}
