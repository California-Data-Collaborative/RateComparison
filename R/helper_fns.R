
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
# calculate mean monthly usage for each account. If an account has no meter reads
# in a given month, then the monthly mean for that account's rate code is used instead
#******************************************************************
get_monthly_usage_by_account <- function(data){
  # All possible combinations of usage_month & cust_id, 
  # even those that aren't present in the data
  months <- tidyr::expand(data, usage_month, cust_id)
  
  cust_id_with_rate_code <- data %>% distinct(cust_id, rate_code) %>% select(cust_id, rate_code)
  
  df_out <- data %>% 
    group_by(cust_id,usage_month) %>% 
    summarise(usage_ccf = mean(usage_ccf,na.rm=TRUE))
  
  rate_code_means <- data %>% 
    group_by(rate_code, usage_month) %>% 
    summarise(rate_code_usage = mean(usage_ccf,na.rm=TRUE))
  
  
  df_out <- months %>% left_join(cust_id_with_rate_code, by="cust_id")  %>% 
    left_join(df_out, by=c("usage_month", "cust_id")) %>%
    left_join(rate_code_means, by=c("usage_month", "rate_code")) %>%
    mutate(usage_ccf = ifelse(is.na(usage_ccf), rate_code_usage, usage_ccf)) %>%
    dplyr::select(usage_month, cust_id, usage_ccf) %>%
    distinct(cust_id, usage_month, .keep_all=TRUE)
  
  df_out
}

add_date_cols <- function(data, the_date){
  data[, "usage_date"] <- the_date
  data[, "usage_month"] <- month(the_date)
  data[, "usage_year"] <- year(the_date)
  return(data)
}

grow_or_remove_accounts <- function(data, class_proportions, month_idx){
  for(j in 1:nrow(class_proportions)){
    current_class <- class_proportions$Var1[j]
    class_growth_amt <- class_proportions$Freq[j]
    
    if(class_proportions$Freq[j] >= 0){
      new_rows_idx <- (nrow(data)+1):(nrow(data)+(class_growth_amt*month_idx))
      data[new_rows_idx, "cust_class"] <- current_class
      
    }else{
      to_del_accounts <- sample_n(filter(data, cust_class == current_class), size = abs(class_growth_amt))
      data <- data %>% filter(!sort_index %in% to_del_accounts$sort_index)
    }
  }
  return(data)
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


