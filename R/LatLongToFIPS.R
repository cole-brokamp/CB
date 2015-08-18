#' Converting Lat/Long Coords into FIPS code
#'
#' This function takes a latitude and longitude input numbers and returns the FIPS code by calling the Census Block Conversion API at the FCC.gov website.  (See more details here: http://www.fcc.gov/developers/census-block-conversions-api)
#' @param latitude Latitude coordinate
#' @param longitude Longitude coordinate
#' @param census.year Defaults to '2010'. Not tested on other years; shouldn't need to change as FIPS locations rarely change.
#' @param showall Set to 'false' as defualt.  Has to do with the FCC API; shouldn't need to change
#' @keywords CB FIPS
#' @export
#' @examples LatLongToFIPS(latitude=39.135398,longitude=-84.519902)

LatLongToFIPS <- function(latitude,longitude,census.year='2010',showall='false'){
  #require(XML)
  #require(RCurl)
  http.call <- paste0('http://data.fcc.gov/api/block/',
                      'census.year'=census.year,
                      '/find?',
                      'latitude=',latitude,
                      '&longitude=',longitude,
                      '&showall=',showall)
  xData <- RCurl::getURL(http.call)
  xml.doc <- XML::xmlRoot(XML::xmlTreeParse(xData))
  FIPS.code <- grep('[[:digit:]]',xml.doc['Block'][[1]],value=TRUE)
  state <- substr(FIPS.code,1,2)
  county <- substr(FIPS.code,3,5)
  tract <- substr(FIPS.code,6,11)
  block <- substr(FIPS.code,12,15)
  return(data.frame('FIPS.code'=FIPS.code,
                    'FIPS.state'=state,
                    'FIPS.county'=county,
                    'FIPS.tract'=tract,
                    'FIPS.block'=block,
                    'latitude.call'=latitude,
                    'longitude.call'=longitude,
                    row.names=NULL))
}
