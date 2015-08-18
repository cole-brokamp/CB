#' getPackages
#'
#' This function takes a package and returns a list of its dependencies.  Good for downloading source files of packages to install on a R server where internet access is blocked.
#' @param packs a quoted package name or list of package names
#' @keywords CBapply
#' @examples
#' # use this to get specifically named packages and their dependencies:
#' packages <- getPackages('pbapply')
#' # use this to get all packages installed on local machine and their dependencies:
#' # packages <- getPackages(row.names(installed.packages()))
#' # then download the packages:
#' download.packages(packages, destdir='.',type='source')

getPackages <- function(packs){
  packages <- unlist(
    tools::package_dependencies(packs, available.packages(),
                         which=c("Depends", "Imports", "LinkingTo"), recursive=TRUE)
  )
  packages <- union(packs, packages)
  packages
}






