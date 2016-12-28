get_entityset <- function(url){
  es <- jsonlite::fromJSON("http://services.odata.org/TripPinRESTierService/?$format=json")
}

get_entity <- function(node){
  props <- xml2::xml_find_all(node, "d1:Property")

  nullable <-
    xml2::xml_attr(props, "Nullable") %>%
    toupper() %>%
    as.logical()

  nullable[is.na(nullable)] <- TRUE

  dplyr::data_frame(
    name = xml2::xml_attr(props, "Name"),
    type = xml2::xml_attr(props, "Type"),
    nullable = nullable
  )

}

get_metadata <- function(base_url){
  url <- file.path(base_url, "$metadata", fsep = "/")
  md <- xml2::read_xml(url)

  entity_type <-
    xml2::xml_find_all(md, './/d1:EntityType')

  base_types <-
    entity_type %>%
    xml2::xml_attr("BaseType") %>%
    gsub("\\w+\\.", "", .)   # remove all namespaces from base types (DIRTY HACK)

  entity_type <-
    entity_type %>%
    lapply(get_entity) %>%
    setNames(xml2::xml_attr(entity_type, "Name"))


  for (i in seq_along(base_types)){ # assumes that base classes are defined before derived classes
    bt <- base_types[i]
    if (is.na(bt)){
      next
    }
    entity_type[[i]] <- bind_rows(entity_type[[bt]], entity_type[[i]])
  }

  EntitySet <-
    xml2::xml_find_all(md, "//d1:EntitySet")

  entity_set <-
    EntitySet %>%
    xml2::xml_attr("EntityType") %>%
    gsub("\\w+\\.", "", .) %>%    # remove all namespaces from base types (DIRTY HACK)
    entity_type[.]

  names(entity_set) <-
    EntitySet %>%
    xml2::xml_attr("Name")


  structure( list( entity_type = entity_type
                 , xml = md
                 , url = url
                 , entity_set = entity_set
                 )
           , class = "odata_metadata"
           )
}


#get_metadata("http://services.odata.org/TripPinRESTierService/(S(0x4aa13tgruywjuejj40csc2))")
#get_metadata("http://opendata.cbs.nl/ODataApi/odata/83583NED")
#base_url <-  "http://opendata.cbs.nl/ODataApi/odata/83583NED"
