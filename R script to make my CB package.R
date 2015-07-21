## code to create initial package

# library(devtools)
# library(roxygen2)
#
# setwd('~/Documents/Biostatistics/_ CB/R')
# create('CB2')

## run this code after updating any of the functions or documentation to rebuild package and install
library(devtools)
library(roxygen2)

setwd('~/Documents/Biostatistics/_CB/R')

document(pkg='CB')
install('CB')


## examples to make sure they work right
library(CB)
example(CBapply)
example(ORGetter)
example(tableSummary)
example(tableTest)


