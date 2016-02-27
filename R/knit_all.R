knit_all <- function() rmarkdown::render(rstudioapi::getActiveDocumentContext()$path,
                                         output_format=c('html_document','pdf_document','word_document'))
