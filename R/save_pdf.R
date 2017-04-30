#' save_pdf
#'
#' Copies the graphics contents of current device to PDF (a wrapper for \code{dev2.pdf}). Default size is 8.5 x 11 in landscape mode.
#' @param file filename to save the image
#' @param width width of pdf image
#' @param height height of pdf image
#' @param jpg convert to 300 dpi .jpg file after pdf save?
#' @export

save_pdf <- function(file,width=11,height=8.5,jpg=FALSE) {
  invisible(grDevices::dev.copy2pdf(file=file,width=width,height=height))
  if (jpg) {
    system2('convert', c('-quality 100',
                         '-density 300',
                         file,
                         gsub('.pdf', '.jpg', file, fixed=TRUE)))
  }
}
