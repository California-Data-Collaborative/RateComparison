
library(ggplot2)
library(lubridate)

check_last_month <- function(data){
  max_date <- max(data$usage_date)
  
  previous_count <- length(unique(dplyr::filter(data, usage_date == max_date %m-% months(1))$cust_id))
  last_count <- length(unique(dplyr::filter(data, usage_date == max_date)$cust_id))
  
  if(last_count < 0.95*previous_count){
    warning(paste("Last month of data has more than 5% fewer accounts than the second to last month. ",
                  "This could be a data error or the result of bimonthly billing."))
  }
  else if(last_count > 1.05*previous_count){
    warning(paste("Last month of data has more than 5% more accounts than the second to last month. ",
                  "This could be a data error or the result of bimonthly billing."))
  }
}

check_first_month <- function(data){
  min_date <- min(data$usage_date)
  
  second_count <- length(unique(dplyr::filter(data, usage_date == min_date %m+% months(1))$cust_id))
  first_count <- length(unique(dplyr::filter(data, usage_date == min_date)$cust_id))

  if(first_count < 0.95*second_count){
    warning(paste("First month of data has more than 5% fewer accounts than the second month. ",
                  "This could be a data error or the result of bimonthly billing."))
  }
  else if(first_count > 1.05*second_count){
    warning(paste("First month of data has more than 5% more accounts than the second month. ",
                  "This could be a data error or the result of bimonthly billing."))
  }
}



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
  long_names <- list("service_charge"="Service Charge")
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


