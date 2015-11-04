#' CB's Custom Apply Function
#'
#' This function is a wrapper for sapply with simplify=FALSE and USE.NAMES=TRUE. It then rbinds via do.call to return data.frame. In order for the names to work properly, a function that returns a data.frame must be used (see example).
#' @param X List of objects to apply over
#' @param FUN Function to apply
#' @param output Output type. Defaults to 'data.frame', but can also be set to 'list' to suppress rbinding of the list.
#' @param num.cores Defaults to 1 and the base 'sapply' is used. If set to greater than one, then it is the number of cores used in parallel::mclapply().
#' @param ... Additional arguments to the function
#' @keywords CBapply
#' @export
#' @examples X <- as.data.frame(matrix(runif(100),ncol=10))
#' names(X) <- LETTERS[1:10]
#' # CBapply(X,mean) # <- will return error
#' # function must return a data.frame with named columns for column names to work
#' CBapply(X,function(x) data.frame('mean'=mean(x)))

CBapply <- function(X,FUN,output='data.frame',num.cores=1,...) {
  # library(parallel)
  if (! output %in% c('data.frame','list')) stop('output must be specified as "data.frame" or "list"')
  if (num.cores == 1) tmp <- sapply(X,FUN,simplify=FALSE,USE.NAMES=TRUE,...)
  if (! num.cores == 1) tmp <- parallel::mclapply(X,FUN,mc.cores=num.cores,...)
  if (output=='data.frame') rtn <- do.call(plyr::rbind.fill,tmp)
  if (output=='list') rtn <- tmp
  return(rtn)
}
