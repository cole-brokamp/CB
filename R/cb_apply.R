#' custom *apply function
#'
#' Function designed to handle anything that lapply can but can specify parallel
#' processing, output naming, progress bars, output format and more.
#'
#' Ideally, a function that returns a data.frame should be supplied. This gives
#' the user the advantage of specifying the names of the columns in the
#' resulting data.frame.  If the function does not return a data.frame, then
#' column names will be automatically generated ('V1','V2',...).
#'
#' Three options exist for the names parameter: (1) 'row.names' will attempt to
#' assign the names of X to the row names of the resulting data.frame. If the
#' names are not unique, then they will instead be assigned to a column in the
#' resulting data.frame called "names". (2) any other character string (i.e.
#' "names" or "id") will assign the names of X to a new column in the resulting
#' data.frame with that character string. If X does not have a names attribute,
#' then names will be automatically generated ('V1','V2',...). Lastly, (3) a
#' vector of character strings will assign this vector as the row.names; it must
#' have as many elements as the number of rows in the resulting data frame.
#'
#' A progress bar can be shown in the terminal using an interactive R session or in an
#' .Rout file, if using R CMD BATCH and submitting R scripts for non-interactive completion.
#' Although R Studio supports the progress bar for single process workers,
#' it has a problem showing the progress bar if using parallel processing
#' (see the discussion at http://stackoverflow.com/questions/27314011/mcfork-in-rstudio).
#' In this specific case (R Studio + parallel processing),
#' text updates will be printed to the file `.process`. Use a shell and
#' `tail -f .progress` to see the updates.
#'
#' @param X List of objects to apply over
#' @param FUN. Function to apply
#' @param output Output type. Defaults to 'data.frame', but can also be set to
#'   'list' to suppress rbinding of the list.
#' @param num.cores The number of cores used for parallel processing.  Can be
#'   specified as an integer, or it will guess the number of cores available
#'   with detectCores(). If parallel is FALSE, the input here will be set to 1.
#' @param fill (defaults to FALSE) use plyr::rbind.fill to fill in missing
#'   columns when binding together results
#' @param ... Additional arguments to the function
#' @param names how to record the names of X in the resulting list or
#'   data.frame. see details
#' @export
#' @examples
#' X <- as.data.frame(matrix(runif(100),ncol=10))
#'
#' fun. <- function(x) {
#'    Sys.sleep(0.5)
#'    mean(x)
#' }
#'
#' cb_apply(X,fun.,output='data.frame',parallel=F,names='row.names',pb=F)
#'
#' cb_apply(X,fun.,output='data.frame',parallel=F,names='id',pb=F)
#'
#' fun. <- function(x) {
#'   Sys.sleep(0.5)
#'   data.frame('mean'=mean(x),'median'=median(x))
#' }
#'
#' cb_apply(X,fun.,output='data.frame',parallel=F,names='row.names',pb=F)
#'
#' cb_apply(X,fun.,output='data.frame',parallel=F,names='id',pb=F)
#'
#' names(X) <- LETTERS[1:10]
#'
#' cb_apply(X,fun.,output='data.frame',parallel=F,names='row.names',pb=T)
#'
#' cb_apply(X,fun.,output='data.frame',parallel=T,num.cores=2,names='row.names',pb=F)
#'
#' fun. <- function(x) {
#'   Sys.sleep(0.5)
#'   data.frame('summ_stat'=c(mean(x),median(x)))
#' }
#'
#' cb_apply(X,fun.,output='data.frame',parallel=T,num.cores=2,names='row.names',pb=T)
#'
#' cb_apply(X,fun.,output='data.frame',parallel=T,num.cores=2,names=paste(rep(names(X),each=2),c('mean','median'),sep='_'),pb=T)



cb_apply <- function(X,FUN.,output='data.frame',fill=TRUE,
                     # names='row.names',
                     pb=TRUE,
                     parallel=FALSE,num.cores=NULL,...){

  stopifnot(output %in% c('data.frame','list'),
            num.cores > 0 | is.null(num.cores))
            # is.character(names))

  # figure out way to name output column in data.frame if already doesn't return a data.frame
  FUN <- function(x,...) {
    outtt <- FUN.(x,...)
    if (is.data.frame(outtt)) return(outtt)
    if (!is.data.frame(outtt)) outtt <- as.data.frame(outtt)
    names(outtt) <- paste0('V',1:ncol(outtt))
    return(outtt)
  }

  n <- length(X)
  if (!is.vector(X) || is.object(X)) X <- as.list(X)

  # set number of cores
  if (parallel) {
    num.cores <- ifelse(is.null(num.cores),
                        parallel::detectCores(TRUE),
                        num.cores)
    if (is.na(num.cores)) num.cores <- 1
  }
  if(!parallel) num.cores <- 1

   # non parallel progress bar
  if (pb & (num.cores==1)) {
    tmp <- vector('list', n)
    pbb <- progress::progress_bar$new(total=100,
                                      format='...  :what (:percent)   [ ETA: :eta | Elapsed: :elapsed ]',
                                     clear=FALSE,force=TRUE,show_after=0)
    pbb$tick(0)
    for (i in 1:n) {
      pbb$tick(len=100/n,tokens = list(what = paste0('processing ',i,' of ',n)))
      tmp[[i]] <- FUN(X[[i]],...)
    }
  }

  # parallel with progress bar
  if (pb & (num.cores > 1)) {
    if (Sys.getenv("RSTUDIO") == "1") {
      message("progress bar doesn't work in RStudio!\n... follow the file \".progress\" instead")
      wrapFUN <- function(i,...) {
        out <- FUN(X[[i]],...)
        cat(paste0('   ... processing ',i,' of ',n,'\n'),file='.progress',append=FALSE)
        return(out)
      }
      tmp <- mclapply(1:n,wrapFUN,...,mc.cores=num.cores)
    } else {
      tmp <- mclapply_pb(X,FUN,mccores=num.cores,...)
    }
  }

  # no progress bar
  if ((!pb) & (num.cores > 1)) tmp <- parallel::mclapply(X,FUN,mc.cores=num.cores,...)
  if ((!pb) & (num.cores == 1)) tmp <- lapply(X,FUN,...)

  # fill function
  if (output=='data.frame') {
    fillFUN <- ifelse(fill,plyr::rbind.fill,rbind)
    tmp <- do.call(fillFUN,tmp)
  }

  # if (length(names) == 1) {
  #   if(is.null(names(X))) apply_names <- paste0('n',1:length(tmp))
  #   if(names(X)[1] == 'V1') apply_names <- paste0('n',1:length(tmp))
  #   if(!is.null(names(X))) apply_names <- names(X)
  # }
  # if (length(names) > 1) {
  #   apply_names <- names
  #   names <- 'names'
  # }
  #
  # if (('row.names' %in% names) & (output == 'data.frame')) {
  #   if( ! all(sapply(tmp,nrow) ==  1)) {
  #     warning('output from function is not data.frame with 1 row; putting names in "names" column')
  #     new_apply_names <- unlist(lapply(1:length(tmp),function(r) rep(apply_names[r],each=sapply(tmp,nrow)[r])))
  #     tmp <- do.call(fillFUN,tmp)
  #     tmp[ ,'names'] <- new_apply_names
  #   } else {
  #     tmp <- do.call(fillFUN,tmp)
  #     row.names(tmp) <- apply_names
  #   }
  # }
  # if ( (length(names) == 1) & (!'row.names' %in% names) & (output == 'data.frame') ){
  #   tmp <- do.call(fillFUN,tmp)
  #   tmp[ ,names] <- apply_names
  #
  # }
  # if (output == 'list') names(tmp) <- apply_names

  return(tmp)
}




