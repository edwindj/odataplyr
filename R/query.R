op <- function(e){
  if (length(e) >  1){
    e[[1]]
  } else {
    e
  }
}

lhs <- function(e){
  if (length(e) > 1){
    e[[2]]
  } else {
    NULL
  }
}

rhs <- function(e){
  if (length(e) > 2){
    e[[3]]
  } else {
    NULL
  }
}


op_s <- function(e){
  as.character(op(e))
}

query <- function(format = "json"){
  list(format = "json")
}

add_filter  <- function(x, expr){
  e <- lazyeval::lazy(expr)
  # substitute constants if possible
  e <- lazyeval::interp(e, .values = parent.frame())
  e
}

OPERATORS <- c( "<" = "lt"
             , ">"  = "gt"
             , ">=" = "ge"
             , "<=" = "le"
             , "==" = "eq"
             , "!=" = "ne"
             , "!"  = "not"
             , "|"  = "or"
             , "&"  = "and"
             , "+"  = "add"
             , "-"  = "sub"
             )

FUNCTIONS <- c( "startswith" = "startswith"
              , "endswith"   = "endswith"
              , "contains"   = "contains"
              , "tolower"    = "tolower"
              , "toupper"    = "toupper"
              , "length"     = "length"
              )


to_query_s <- function(e){
  if (length(e) == 1){
    if (is.character(e)){
      return(paste0("'", e,"'"))
    } else {
      return(as.character(e))
    }
  }

  o <- op_s(e)
  l <- lhs(e)
  r <- rhs(e)
  switch( o
        , "("  = glue::glue("({to_query_s(l)})")
        , "|"  = glue::glue("{to_query_s(l)} or {to_query_s(r)}")
        , "&"  = glue::glue("{to_query_s(l)} and {to_query_s(r)}")
        , ">"  = glue::glue("{to_query_s(l)} gt {to_query_s(r)}")
        , "==" = glue::glue("{to_query_s(l)} eq {to_query_s(r)}")
        , "!=" = glue::glue("{to_query_s(l)} ne {to_query_s(r)}")
        , ">=" = glue::glue("{to_query_s(l)} ge {to_query_s(r)}")
        , "<"  = glue::glue("{to_query_s(l)} lt {to_query_s(r)}")
        , "<=" = glue::glue("{to_query_s(l)} le {to_query_s(r)}")
        , "+"  = glue::glue("({to_query_s(l)} add {to_query_s(r)})")
        , "-"  = glue::glue("({to_query_s(l)} sub {to_query_s(r)})")
        , if (o %in% FUNCTIONS){
            if (is.null(r)){
              glue::glue("{o}({to_query_s(l)})")
            } else {
              glue::glue("{o}({to_query_s(l)}, {to_query_s(r)})")
            }
        } else {
          stop("Function/operator: '", o, "' not supported in translation to odata.")
        }
        )
}

A <- 3
e <- add_filter(NULL, x > A | y > 3 & (x - 2 < 1) & tolower(woonplaats) == 'Arnhem')
to_query_s(e$expr)

