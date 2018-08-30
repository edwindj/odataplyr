library(xml2)

doc <- xml2::read_xml("https://acc-ccb.cbs.nl/$metadata")

ns <- xml_ns_rename(xml_ns(doc), d1 = "edm")

type_schema <- xml_find_all(doc, "//edm:Schema/edm:EntityType/..", ns=ns)
Namespace <- xml_attr(type_schema, "Namespace")

type_nodes <- xml_find_all(type_schema, "edm:EntityType", ns=ns)
types <- lapply(type_nodes, get_type_info)
names(types) <- paste(Namespace, xml_attr(type_nodes, "Name"), sep=".")


get_type_info <- function(n, ...){
  props <- xml_find_all(n, "edm:Property", ns=ns)
  data.frame( name = xml_attr(props, "Name")
            , type = xml_attr(props, "Type")
            , stringsAsFactors = FALSE
            )
}

get_EntitySets <- function(doc, types, ...){
  es_nodes <- xml_find_all(doc, "//edm:EntitySet", ns=ns)
  es <- data.frame( name = xml_attr(es_nodes, "Name")
              , type = xml_attr(es_nodes, "EntityType")
              , stringsAsFactors = FALSE
  )
  es_list <- lapply(es$type, function(t){types[[t]]})
  names(es_list) <- es$name
  es_list
}
