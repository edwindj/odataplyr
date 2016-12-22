#' Connect to an OData data source
#'
#' Connect to an OData data source
#' @export
#' @param url of data source
src_odata <- function(url){
  odata_src  <- list()
  dplyr::src("src_odata", odata_src)
}


