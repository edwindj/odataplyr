#' Connect to an OData data source
#'
#' Connect to an OData data source
#' @export
#' @param url of data source
#' @examples
#' src_odata("http://services.odata.org/TripPinRESTierService")
src_odata <- function(url){
  odata_src  <- list( url = url
#                    , metadata = get_metadata(url)
                    )
  jsonlite::fromJSON(url)
  dplyr::src("src_odata", odata_src)
}

url <- "http://services.odata.org/TripPinRESTierService"
src_odata("http://services.odata.org/TripPinRESTierService")
