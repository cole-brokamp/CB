##------------------------------------------------------------------------------
##' Wrapper around mclapply to track progress
##'
##' Doesn't work in RStudio!
##' Based on http://stackoverflow.com/questions/10984556
##'
##' @param X         a vector (atomic or list) or an expressions vector. Other
##'                  objects (including classed objects) will be coerced by
##'                  ‘as.list’
##' @param FUN       the function to be applied to
##' @param ...       optional arguments to ‘FUN’
##' @param mc.preschedule see mclapply
##' @param mc.set.seed see mclapply
##' @param mc.silent see mclapply
##' @param mc.cores see mclapply
##' @param mc.cleanup see mclapply
##' @param mc.allow.recursive see mclapply
##' @param mc.progress track progress?
##'
##------------------------------------------------------------------------------
mclapply_pb <- function(X, FUN, ...,
                        mc.preschedule = TRUE, mc.set.seed = TRUE,
                        mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
                        mc.cleanup = TRUE, mc.allow.recursive = TRUE,
                        mc.progress=TRUE)
{
  if (Sys.getenv("RSTUDIO") == "1") message("progress bar doesn't work in RStudio!")
  library(parallel)
  if (!is.vector(X) || is.object(X)) X <- as.list(X)

  if (mc.progress) {
    f <- fifo(tempfile(), open="w+b", blocking=T)
    p <- parallel:::mcfork()
    pb <- utils::txtProgressBar(0, length(X), style=3)
    utils::setTxtProgressBar(pb, 0)
    progress <- 0
    if (inherits(p, "masterProcess")) {
      while (progress < length(X)) {
        readBin(f, "double")
        progress <- progress + 1
        utils::setTxtProgressBar(pb, progress)
      }
      cat("\n")
      parallel:::mcexit()
    }
  }
  tryCatch({
    result <- parallel::mclapply(X, function(.x,...) {
      res <- FUN(.x,...)
      if (mc.progress) writeBin(1, f)
      res
    },...,
    mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed,
    mc.silent = mc.silent, mc.cores = mc.cores,
    mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive
    )

  }, finally = {
    if (mc.progress) close(f)
  })
  result
}


