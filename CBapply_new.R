#' @param X List of objects to apply over
#' @param FUN Function to apply
#' @param output Output type. Defaults to 'data.frame', but can also be set to 'list' to suppress rbinding of the list.
#' @param num.cores Defaults to 1 and the base 'sapply' is used. If set to greater than one, then it is the number of cores used in parallel::mclapply().
#' @param fill (defaults to FALSE) use plry::rbind.fill to fill in missing columns
#' @param ... Additional arguments to the function
#' @examples X <- as.data.frame(matrix(runif(100),ncol=10))
#' names(X) <- LETTERS[1:10]
#' # CBapply(X,mean) # <- will name column as "FUN.(x, ...)"
#' # function must return a data.frame with named columns for column names to work
#' CBapply(X,function(x) data.frame('mean'=mean(x)))
#' CBapply(X,function(x) data.frame('mean'=mean(x),'median'=median(x)))

# example
X <- as.data.frame(matrix(runif(100),ncol=10))
names(X) <- LETTERS[1:10]

CBapply(X,function(x) data.frame('mean'=mean(x)),
        output='data.frame',fill=F,parallel=F,names='row.names',pb=F)
CBapply(X,mean,
        output='data.frame',fill=F,parallel=T,names='row.names',pb=T,na.rm=T)



CBapply <- function(X,FUN.,output='data.frame',fill=FALSE,parallel=FALSE,
                    num.cores=NULL,...,names='row.names',pb=TRUE) {

  stopifnot(output %in% c('data.frame','list'),
            num.cores > 0 | is.null(num.cores),
            names %in% c('row.names','names'))

  FUN <- function(x,...) as.data.frame(FUN.(x,...))

  n <- length(X)
  if (!is.vector(X) || is.object(X)) X <- as.list(X)


  if (parallel) {
    n.cores <- ifelse(is.null(num.cores),
                      parallel::detectCores(TRUE),
                      num.cores)
    if (is.na(n.cores)) n.cores <- 1
  }

    if (pb & (!parallel)) {
      tmp <- vector('list', n)
      pb <- progress::progress_bar$new(total=100,
                                       format='   ...  :what (:percent)   [ ETA: :eta | Elapsed: :elapsed ]',
                                       clear=FALSE,force=FALSE)
      for (i in 1:n) {
        pb$tick(len=100/n,tokens = list(what = paste0('processing ',i,' of ',n)))
        tmp[[i]] <- FUN(X[[i]],...)
      }
    }

    if (pb & parallel) {
      wrapFUN <- function(i) {
        message(paste0('   ... processing ',i,' of ',n))
        out <- FUN(X[[i]],...)
        return(out)
      }
      wrapFUN(1)
      tmp <- parallel::mclapply(1:n,wrapFUN,mc.cores=n.cores,mc.silent=F,mc.cleanup=T)
    }


    if ((!pb) & parallel) tmp <- parallel::mclapply(X,FUN,mc.cores=num.cores,...)
    if ((!pb) & (!parallel)) tmp <- lapply(X,FUN,...)

    if (output=='data.frame') {
      fillFUN <- ifelse(fill,plyr::rbind.fill,rbind)
      tmp <- do.call(fillFUN,tmp)
    }

    if (is.null(names(X))) {
      apply_names <- as.character(X)
    } else {
      apply_names <- names(X)
    }

    if ((names == 'row.names') & (output == 'data.frame')) {
      if( ! all(sapply(tmp,nrow) ==  1)) {
        warning('output from function is not data.frame with 1 row\n ... putting names in "names" column')
        tmp$names <- rep(apply_names,each=sapply(tmp,nrow))
      } else {
        row.names(tmp) <- apply_names
      }
      row.names(tmp) <- apply_names
    }
  if (names == 'names' & (output == 'data.frame')) tmp$names <- rep(apply_names,each=sapply(tmp,nrow))
  if (output == 'list') names(tmp) <- apply_names

  return(tmp)
}
