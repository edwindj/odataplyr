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
             )

to_query_s <- function(e){
  paste(lhs(e), OPERATORS[op_s(e)], rhs(e))
}

# A <- 3
# e <- add_filter(NULL, x > A)
# lhs(e$expr)
# rhs(e$expr)
#
# to_query_s(e$expr)
