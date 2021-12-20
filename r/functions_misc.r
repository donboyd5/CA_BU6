

get_stname <- function(stabbr){
  sts <- c("US", "DC", state.abb)
  allnames <- c("United States", "District of Columbia", state.name)
  allnames[match(stabbr, sts)]
}

