
library(ggplot2)

#******************************************************************
# Split text box input into a vector of strings
#******************************************************************
parse_strings <- function(str){
  return( stringr::str_trim(unlist(strsplit(str, "[\n]"))) )
}

#******************************************************************
# Convert vector of numeric strings into actual numerics
#******************************************************************
parse_numerics <- function(str){
  return( suppressWarnings(as.numeric(parse_strings(str))) )
}

is_valid_rate_part <- function(rate_part){
  tryCatch({
    rate_part[[names(rate_part)]]
    return(TRUE)
  }, error = function(err){
    return(FALSE)
  })
}

stopif <- function(bool, message){
  if(bool){
    stop(message, call.=FALSE)
  }
}

is_rate_type <- function(rate_part){
  name <- names(rate_part)
  if(rate_part[[name]] %in% c("Budget", "Tiered")){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

get_long_part_name <- function(name){
  long_names <- list("sewer_charge"="Sewer Charge")
  ln <- long_names[[name]]
  # browser()
  if(is.character(ln)){ return(ln) }
  else{ return(name) }
}


is_map <- function(rate_part){
  parts <- names(rate_part)
  has_depends <- "depends_on" %in% parts
  has_values <- "values" %in% parts
  
  if(has_depends && has_values){
    return(TRUE)
  } else if (has_depends || has_values){
    stop("Each 'depends_on' clause must have a corresponding 'values' clause, and vice versa.")
  } else{
    return(FALSE)
  }
}

getmode <- function(v) {
  #fn to get mode of a vector
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


