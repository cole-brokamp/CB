#' a wrapper around purrr::map()
#'
#' This function is a wrapper around purrr::map() with some extras on top,
#' including parallel computation, progress bar, error handling, and result caching.
#'
#' It is designed for long computations and as such it always uses a progress bar,
#' and always returns a list. Long computations shouldn't worry about being type
#' strict; instead, extract results in the right type from the results list.
#'
#' A progress bar will be shown in the terminal using an interactive R session or
#' in an .Rout file, if using R CMD BATCH and submitting R scripts for
#' non-interactive completion. Although R Studio supports the progress bar for
#' single process workers, it has a problem showing the progress bar if using
#' parallel processing (see the discussion at
#' http://stackoverflow.com/questions/27314011/mcfork-in-rstudio). In this
#' specific case (R Studio + parallel processing), text updates will be printed
#' to the file `.process`. Use a shell and `tail -f .progress` to see the
#' updates.
#'
#' @param X List of objects to apply over
#' @param FUN. Function to apply; allows for compact anonymous functions (see
#'   ?purrr::as_mapper() for details
#' @param parallel logical; use parallel processing?
#' @param num.cores The number of cores used for parallel processing.  Can be
#'   specified as an integer, or it will guess the number of cores available
#'   with detectCores(). If parallel is FALSE, the input here will be set to 1.
#' @param cache defaults to FALSE, which means no cache used. If TRUE, cache the results locally in a folder named according to \code{cache.name} using the memoise package
#' @param cache.name a character string to use a custom cache folder name (e.g. "my_cache"); defaults to "cache"
#' @param error.value (defaults to NA) use purrr::possibly to replace errors with this value instead of interrupting the process; set to NULL to not use error handling and instead interrupt the calculation
#' @export
#' @examples
#' X <- list('x' = 100, 'y' = 'a', 'z' = 200)
#' slow_log <- function(.x) {Sys.sleep(0.5); log(.x)}
#' # by default returns NA on error
#' mappp(X, slow_log)
#' # when not using error, entire calculation will fail
#' mappp(X, slow_log, error.value=NULL)
#' # showing error messages when they occur rather than afterwards can be useful
#' # but will cause problems with error bar displays
#' mappp(X, slow_log, quiet=FALSE)

mappp <- function(X, FUN.,
                  parallel=FALSE,
                  cache=FALSE, cache.name='cache',
                  error.value=NA,
                  quiet=TRUE,
                  num.cores=NULL){

  FUN <- purrr::as_mapper(FUN.)

  if (cache) {
    # library(memoise)
    fc <- memoise::cache_filesystem(cache.name)
    FUN <- memoise::memoise(FUN,cache=fc)
  }

  if (!is.null(error.value)) FUN <- purrr::possibly(FUN, otherwise=error.value, quiet=quiet)

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
  if ((num.cores==1)) {
    tmp <- vector('list', n)
    pbb <- progress::progress_bar$new(total=100,
                                      format='...  :what (:percent)   [ ETA: :eta | Elapsed: :elapsed ]',
                                     clear=FALSE,force=TRUE,show_after=0)
    pbb$tick(0)
    for (i in 1:n) {
      pbb$tick(len=100/n,tokens = list(what = paste0('processing ',i,' of ',n)))
      tmp[[i]] <- FUN(X[[i]])
    }
  }

  # parallel with progress bar
  if (num.cores > 1) {
    if (Sys.getenv("RSTUDIO") == "1") {
      message("progress bar doesn't work in RStudio!\n... follow the file \".progress\" instead")
      wrapFUN <- function(i) {
        out <- FUN(X[[i]])
        out.percentage <- round(i/n*100,digits=0)
        cat(paste0('   ... processing ',i,' of ',n,' (',out.percentage,'%)','\n'),
            file='.progress',append=FALSE)
        return(out)
      }
      tmp <- parallel::mclapply(1:n,wrapFUN,mc.cores=num.cores)
    } else {
      tmp <- mclapply_pb(X,FUN,mc.cores=num.cores)
    }
  }

  return(tmp)
}




