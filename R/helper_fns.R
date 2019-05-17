
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
# Make sure that each rate code is mapped to at most 1 customer class.
# Throws an error if this assumption is violated.
#******************************************************************
check_duplicated_rate_codes <- function(data){
  tmp <- data %>% group_by(rate_code, cust_class) %>% summarise(num=1) %>% 
    group_by(rate_code) %>% summarise(is_duped = length(cust_class) > 1)  
  
  stopif(any(tmp$is_duped), paste("A rate_code is mapped to more that one cust_class.",
                                  "Please ensure consistent mapping to customer classes."))
}

#******************************************************************
# Make sure that every class present in the data is also defined in the OWRS file.
#******************************************************************
check_missing_classes <- function(owrs_classes, data_classes, owrs_file_name){
  classes_not_in_OWRS <- data_classes[!(data_classes %in% owrs_classes)]
  stopif(length(classes_not_in_OWRS) > 0,
         paste("Customer class(es)",  paste(classes_not_in_OWRS, collapse=", "),
               "are present in the data but no rate information is defined in OWRS file",
               owrs_file_name))
}

#******************************************************************
# Create the list of customer classes to display
#******************************************************************
get_cust_class_list <- function(data, base_rates, owrs_file){
  # list of unique customer classes in the data
  cust_class_list <- names(base_rates$rate_structure)
  #in case there are any classes in the data that are not defined in the ORWS file
  cust_class_list_from_data <- unique(data$cust_class)
  
  # error checking
  check_missing_classes(cust_class_list, cust_class_list_from_data, owrs_file)
  
  cust_class_list <- cust_class_list[cust_class_list %in% cust_class_list_from_data]
  cust_class_list
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
  
  customer_means <- data %>%
    group_by(cust_id) %>% 
    summarise(customer_usage = mean(usage_ccf,na.rm=TRUE))
  
  rate_code_means <- data %>% 
    group_by(rate_code, usage_month) %>% 
    summarise(rate_code_usage = mean(usage_ccf,na.rm=TRUE))
  
  df_out <- months %>% left_join(cust_id_with_rate_code, by="cust_id")  %>% 
    left_join(df_out, by=c("usage_month", "cust_id")) %>%
    left_join(rate_code_means, by=c("usage_month", "rate_code")) %>%
    left_join(customer_means, by=c("cust_id")) %>%
    mutate(usage_ccf = ifelse(is.na(usage_ccf), customer_usage, usage_ccf)) %>%
    mutate(usage_ccf = ifelse(is.na(usage_ccf), rate_code_usage, usage_ccf)) %>%
    dplyr::select(usage_month, cust_id, usage_ccf) %>%
    distinct(cust_id, usage_month, .keep_all=TRUE)
  
  df_out
}

get_monthly_budget_by_account <- function(data){
  # All possible combinations of usage_month & cust_id, 
  # even those that aren't present in the data
  months <- tidyr::expand(data, usage_month, cust_id)
  
  cust_id_with_rate_code <- data %>% distinct(cust_id, rate_code) %>% select(cust_id, rate_code)
  
  df_out <- data %>% 
    group_by(cust_id,usage_month) %>% 
    summarise(budget = mean(budget,na.rm=TRUE))
  
  customer_means <- data %>%
    group_by(cust_id) %>% 
    summarise(customer_usage = mean(budget,na.rm=TRUE))
  
  rate_code_means <- data %>% 
    group_by(rate_code, usage_month) %>% 
    summarise(rate_code_usage = mean(budget,na.rm=TRUE))
  
  df_out <- months %>% left_join(cust_id_with_rate_code, by="cust_id")  %>% 
    left_join(df_out, by=c("usage_month", "cust_id")) %>%
    left_join(rate_code_means, by=c("usage_month", "rate_code")) %>%
    left_join(customer_means, by=c("cust_id")) %>%
    mutate(budget = ifelse(is.na(budget), customer_usage, budget)) %>%
    mutate(budget = ifelse(is.na(budget), rate_code_usage, budget)) %>%
    dplyr::select(usage_month, cust_id, budget) %>%
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


