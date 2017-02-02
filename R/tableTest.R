#' Table Test
#'
#' This function summarizes and tests the differences of numerical and dichotomous variables only across some factor.  The summary number is either the mean of a numeric variable for the number and percentage of values that are the second of the two factors in a dichotomous variable.  Missing values are removed before the summary statistic, but the number missing is not reported. Furthermore, a p-value is reported testing the differences of the means or counts across the groups factor.  The p-value is derived from an ANOVA for continuous variables or from a chi-squared test via monte-carlo simulation using 100,000 bootstrap replicates.
#' @param x Vector of data which to summarize.  Should be used for numerical and dichotomous variables only.
#' @param group The factor for which to test the x variable across.
#' @param digits.mean The mean is rounded and displayed using this many digits.
#' @param digits.percentage The percentage is rounded and displayed using this many digits.
#' @keywords CB test table
#' @export
#' @examples X <- data.frame('some.continuous'=runif(300),'some.factor'=factor(rbinom(300,1,0.3)))
#' X$some.other.factor <- factor(rbinom(300,1,0.5))
#' tableTest(x=X$some.continuous,group=X$some.other.factor)
#' tableTest(x=X$some.factor,group=X$some.other.factor)
#' CBapply(X[ ,c('some.continuous','some.factor')],tableTest,group=X$some.other.factor)


tableTest <- function(x,group,digits.mean=2,digits.percentage=0){
  .Deprecated(msg='tableSummary is deprecated but is maintained here for backwards compatability.\nConsider using tidyverse instead.')
  stopifnot(class(x) %in% c("factor","numeric","integer",'ordered'))
  stopifnot(class(group) %in% c('factor','ordered'))
  tmp <- data.frame(x,group)
  tmp <- stats::na.omit(tmp)
  if (nrow(tmp) < length(x)) warning('NA values have been omitted')
  if (class(x) %in% c('numeric','integer')){
    p <- round(stats::anova(stats::lm(x ~ group,data=tmp))["Pr(>F)"][[1]][1],digits=3)
    p.text <- ifelse(p<0.001,"< 0.001",paste(p))
    means <- round(tapply(tmp$x,tmp$group,mean,na.rm=TRUE),digits=digits.mean)
    out <- paste0(sprintf(paste0('%.',digits.mean,'f'),means))
  }
  if(class(x) %in% c('factor','ordered')){
    if (!nlevels(tmp$x) == 2) warning('The outcome factor does not have 2 levels, the output of the table is wrong!')
    tbl <- table(tmp$x,tmp$group)
    n <- tbl[2, ]
    p <- round(stats::chisq.test(tbl,simulate.p.value=TRUE,B=100000)$p.value,digits=3)
    p.text <- ifelse(p<0.001,"< 0.001",paste(p))
    perc <- prop.table(tbl,margin=2)[2, ] * 100
    perc.round <- paste0(sprintf(paste0('%.',digits.percentage,'f'),perc))
    out <- paste0(n,' (',perc.round,'%)')
  }
  rtn <- data.frame(t(out),p.text)
  names(rtn) <- c(levels(group),'p-value')
  return(rtn)
}
