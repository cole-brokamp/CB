% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/z_deprecated.R
\name{getPackages}
\alias{getPackages}
\title{getPackages}
\usage{
getPackages(packs)
}
\arguments{
\item{packs}{a quoted package name or list of package names}
}
\description{
This function takes a package and returns a list of its dependencies.  Good for downloading source files of packages to install on a R server where internet access is blocked.
}
\examples{
\dontrun{
# use this to get specifically named packages and their dependencies:
packages <- getPackages('pbapply')
# use this to get all packages installed on local machine and their dependencies:
# packages <- getPackages(row.names(installed.packages()))
# then download the packages:
download.packages(packages, destdir='.',type='source')
}
}
\keyword{CBapply}
