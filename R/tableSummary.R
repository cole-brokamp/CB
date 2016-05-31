#' Summary Table
#'
#' This function summarizes numerical and dichotomous variables only.  The summary number is either the mean of a numeric variable for the number and percentage of values that are the second of the two factors in a dichotomous variable.  Missing values are removed before the summary statistic is calculated and the numer of missing observations is also presented in the table.
#' @param x Vector of data which to summarize.  Should be used for numerical and dichotomous variables only.
#' @param digits.mean The mean is rounded and displayed using this many digits.
#' @param digits.percentage The percentage is rounded and displayed using this many digits.
#' @keywords CB summary table
#' @export
#' @examples
#' X <- data.frame('some.continuous'=runif(300),'some.factor'=factor(rbinom(300,1,0.3)))
#' tableSummary(X$some.continuous)
# 'tableSummary(X$some.factor)
#' # use CBapply to create a table
#' CBapply(X,tableSummary)
#' # specify the digits differently to change the display of the table
#' CBapply(X,tableSummary,digits.mean=3,digits.percentage=2)


tableSummary <- function(x,digits.mean=2,digits.percentage=0){
  stopifnot(class(x) %in% c("factor","numeric","integer",'ordered'))
  tmp <- stats::na.omit(x)
  number.missing <- length(x) - length(tmp)
  if (class(x) %in% c('numeric','integer')){
    out <- paste0(round(mean(tmp),digits=digits.mean))
    out.round <- round(mean(tmp),digits=digits.mean)
    out <- paste0(sprintf(paste0('%.',digits.mean,'f'),out.round))
  }
  if(class(x) %in% c('factor','ordered')){
    total <- length(tmp)
    n <- summary(tmp)[[2]]
    perc.round <- round(n/total*100,digits=digits.percentage)
    out <- paste0(n,' (',sprintf(paste0('%.',digits.percentage,'f'),perc.round),'%)')
  }
  return(data.frame('summary'=out,'number NA'=number.missing,check.names=FALSE))
}
