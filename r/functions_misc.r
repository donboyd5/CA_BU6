

get_stname <- function(stabbr){
  sts <- c("US", "DC", state.abb)
  allnames <- c("United States", "District of Columbia", state.name)
  allnames[match(stabbr, sts)]
}

namedList  <-  function(...){
  out  <-  list(...)
  for(i in seq(length(out)))
    names(out)[i]  <-  as.character(sys.call()[[i+1]])
  out
}