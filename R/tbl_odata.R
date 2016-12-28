tbl_odata <- function(url, name, ...){
  tbl <- list()
  structure(tbl, class=c("tbl_odata", "tbl"))
}

get_odata_query <- function(x, ...){
  stop("Not implemented")
}
