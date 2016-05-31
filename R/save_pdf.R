#' save_pdf
#'
#' Copies the graphics contents of current device to PDF (a wrapper for \code{dev2.pdf}). Default size is 8.5 x 11 in landscape mode.
#' @param file filename to save the image
#' @param width width of pdf image
#' @param height height of pdf image
#' @export

save_pdf <- function(file,width=11,height=8.5) invisible(grDevices::dev.copy2pdf(file=file,width=width,height=height))
