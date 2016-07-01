#'Access CCHMC color palette
#'
#' @param n which cchmc color (1:purple, 2:green, 3:blue, 4:grey)
#' @param alpha create semi-transparent colors (0 < alpha < 1)
#'
#' @return a named character string or vector of named character strings containing RGB colors in hexadecimal
#' @export
#'
#' @examples
#' cchmc_color(1)
#' cchmc_color(2)
#' cchmc_color(3)
#' cchmc_color(4)
#' cchmc_color(1:4)
#'
#' plot(iris$Sepal.Length,iris$Sepal.Width,pch=19,
#'      col=cchmc_color(sapply(iris$Species,function(x) switch(as.character(x),
#'                                                             'setosa' = 1,
#'                                                             'versicolor' = 2,
#'                                                             'virginica' = 3))))
cchmc_color <- function(n,alpha) {
  stopifnot(n > 0, n < 5)
  colors <- c(grDevices::rgb(158,70,121,alpha=alpha,maxColorValue=255,names='cchmc_purple'),
              grDevices::rgb(118,188,68,alpha=alpha,maxColorValue=255,names='cchmc_green'),
              grDevices::rgb(2,170,200,alpha=alpha,maxColorValue=255,names='cchmc_blue'),
              grDevices::rgb(85,87,90,alpha=alpha,maxColorValue=255,names='cchmc_grey'))

  return(colors[n])
}
