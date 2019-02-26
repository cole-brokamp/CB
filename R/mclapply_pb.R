##' Wrapper around mclapply to track progress
##'
##' Doesn't work in RStudio!
##' Based on http://stackoverflow.com/questions/10984556
##'
##' @param X a vector (atomic or list) or an expressions vector. Other
##'   objects (including classed objects) will be coerced by ‘as.list’
##' @param FUN the function to be applied to
##' @param mc.cores number of cores to use
mclapply_pb <- function(X, FUN, mc.cores = getOption("mc.cores", 2L)){
    if (Sys.getenv("RSTUDIO") == "1") message("progress bar doesn't work in RStudio!")
    if (!is.vector(X) || is.object(X)) X <- as.list(X)
    n <- length(X)
    f <- fifo(tempfile(), open="w+b", blocking=T)
    on.exit(close(f))
    p <- parallel:::mcfork()
    pbb <- pbmcapply::progressBar(0, n, style='ETA', width=60)
    utils::setTxtProgressBar(pbb, 0)
    progress <- 0
    if (inherits(p, "masterProcess")) {
        while (progress < n) {
            readBin(f, "double")
            progress <- progress + 1
            utils::setTxtProgressBar(pbb, progress)
        }
        cat("\n")
        parallel:::mcexit()
    }
    result <- parallel::mclapply(X, function(.x) {
                                     res <- FUN(.x)
                                     writeBin(1, f)
                                     res
},
mc.cores = mc.cores)
    result
}



