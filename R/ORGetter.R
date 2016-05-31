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
