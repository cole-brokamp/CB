#' @param X List of objects to apply over
#' @param FUN Function to apply
#' @param output Output type. Defaults to 'data.frame', but can also be set to 'list' to suppress rbinding of the list.
#' @param num.cores Defaults to 1 and the base 'sapply' is used. If set to greater than one, then it is the number of cores used in parallel::mclapply().
#' @param fill (defaults to FALSE) use plry::rbind.fill to fill in missing columns
#' @param ... Additional arguments to the function
#' @examples X <- as.data.frame(matrix(runif(100),ncol=10))
#' names(X) <- LETTERS[1:10]
#' # CBapply(X,mean) # <- will return error
#' # function must return a data.frame with named columns for column names to work
#' CBapply(X,function(x) data.frame('mean'=mean(x)))

# ideas
# pb = c('graphical','text','none')
    # auto for graphical selected if in console or R Studio
    # auto for text selected if not in console
# fill = logical
# names = c('row.names','custom_name')
    # implement check to see if names is unique, if not, use applied names
# output = c('data.frame','list')
# num_cores = integer 
    # if 1, then normal
    # if > 1, then use parallel::mclapply
# ... extra arguments supplied to function
# use AnyBar from Github to put status of function in the MacOSX menubar (change colors when its done)

# example
X <- as.data.frame(matrix(runif(100),ncol=10))
names(X) <- LETTERS[1:10]
CBapply(X,function(x) data.frame('mean'=mean(x)))

CBapply <- function(X,FUN,output=c('data.frame','list'),fill=FALSE,
                    num.cores=2,...,names=c('row.names','names'),
                    pb=c('graphical','text','none')) {
  stopifnot(output %in% c('data.frame','list'), 
            num.cores > 0,
            names %in% c('row.names','names'))
  fillFUN <- ifelse(fill,plyr::rbind.fill,rbind)
  n <- length(X)
  
  wrapperFUN <- function(x,...) {
    if (pb == 'graphical') {
      pb <- progress::progress_bar$new(total=n)
      pb$tick(1/n)
    }
    FUN(x,...)
  }
  
  
  if (is.null(names(X))) {
    apply_names <- as.character(X)
  } else {
    apply_names <- names(X)
  }
  
  
  
  tmp <- parallel::mclapply(X,wrapperFUN,mc.cores=num.cores)
  
  if( ! all(sapply(tmp,nrow) ==  1)) {
    warning('output from function is not data.frame with 1 row\n ... putting names in "names" column')
    tmp$names <- rep(apply_names,each=sapply(tmp,nrow))
  } else {
    row.names(tmp) <- apply_names
  }
  
  
  
  if (names == 'row.names') row.names(tmp) <- apply_names
  if (names == 'names') tmp$names <- apply_names
  

    # test if any data.frames in list of output has more than one row; if so, make new row.names
    
CBapply <- function(X,FUN,output='data.frame',fill=FALSE,num.cores=1,...) {
  if (! output %in% c('data.frame','list')) stop('output must be specified as "data.frame" or "list"')
  if (num.cores == 1) tmp <- sapply(X,FUN,simplify=FALSE,USE.NAMES=TRUE,...)
  if (! num.cores == 1) tmp <- parallel::mclapply(X,FUN,mc.cores=num.cores,...)
  if (output=='data.frame') {
    if (fill) {
      tmp <- lapply(tmp,as.data.frame)
      rtn <- do.call(plyr::rbind.fill,tmp)
      try({
        row.names(rtn) <- names(tmp)
        },silent=TRUE)
    }
    if (!fill) {
      rtn <- do.call(rbind,tmp)
      try({
        row.names(rtn) <- names(tmp)
      },silent=TRUE)
    }
  }
  if (output=='list') rtn <- tmp
  return(rtn)
}
