% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/z_deprecated.R
\name{ORGetter}
\alias{ORGetter}
\title{Retreive Odds Ratio Table from Logistic GLM Objects}
\usage{
ORGetter(logistic.glm, digits = 2, sig.star = TRUE,
  show.intercept = FALSE)
}
\arguments{
\item{logistic.glm}{A logistic GLM R object.  If not an object of 'glm' and 'lm', it will stop with an error.}

\item{digits}{Number of digits to round table}

\item{sig.star}{Will return an extra column with a star if the confidence interval does not contain 1. Defaults to TRUE.}

\item{show.intercept}{Will show the intercept and its confidence interval only if set to TRUE. Defaults to FALSE.}
}
\description{
This function returns a data.frame of the odds ratios and their 95\% confidence intervals.
}
\examples{
\dontrun{ x1 <- rnorm(100)
x2 <- rnorm(100)
y <- rbinom(100,1,prob=0.3)
logistic.model <- glm(y ~ x1 + x2,family='binomial')
ORGetter(logistic.model)
}
}
\keyword{CB}
\keyword{OddsRatio}
