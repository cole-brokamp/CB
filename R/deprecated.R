#' Retreive Odds Ratio Table from Logistic GLM Objects
#'
#' This function returns a data.frame of the odds ratios and their 95\% confidence intervals.
#' @param logistic.glm A logistic GLM R object.  If not an object of 'glm' and 'lm', it will stop with an error.
#' @param sig.star Will return an extra column with a star if the confidence interval does not contain 1. Defaults to TRUE.
#' @param show.intercept Will show the intercept and its confidence interval only if set to TRUE. Defaults to FALSE.
#' @param digits Number of digits to round table
#' @export
#' @keywords CB OddsRatio
#' @examples
#' \dontrun{ x1 <- rnorm(100)
#' x2 <- rnorm(100)
#' y <- rbinom(100,1,prob=0.3)
#' logistic.model <- glm(y ~ x1 + x2,family='binomial')
#' ORGetter(logistic.model)
#' }

ORGetter <- function(logistic.glm,digits=2,sig.star=TRUE,show.intercept=FALSE){
  .Deprecated(msg='tableSummary is deprecated but is maintained here for backwards compatability.\nConsider using tidyverse instead.')
  stopifnot(class(logistic.glm)==c('glm','lm'))
  OR <- stats::coef(logistic.glm)
  CI <- suppressMessages(stats::confint(logistic.glm))
  tmp <- exp(data.frame(OR,CI))
  names(tmp) <- c('odds ratio','lower.CI','upper.CI')
  tmp <- round(tmp,digits=digits)
  if (!show.intercept) tmp <- tmp[-1, ]
  tmp$sig <- ifelse((tmp$lower.CI > 1) & (tmp$upper.CI > 1)
                    | (tmp$lower.CI < 1) & (tmp$upper.CI < 1)
                    ,' * ','   ')
  if (!sig.star) tmp$sig <- NULL
  return(tmp)
}

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
  .Deprecated(msg='tableSummary is deprecated but is maintained here for backwards compatability.\nConsider using tidyverse instead.')
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
