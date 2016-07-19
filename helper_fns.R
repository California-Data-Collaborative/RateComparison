library(dplyr)
library(ggplot2)
# library(reshape2)
# library(DT)


#---------------------------------- FUNCTIONS ----------------------------------------------

#******************************************************************
# Read in the data and map the columns to application columns
#******************************************************************
read_data <- function(filename, usage_col, month_col, year_col, et_col, hhsize_col, irr_area_col, 
                      rate_code_col){
  print("Reading data...")
  data <- tbl_df(read.csv(filename)) %>% 
          dplyr::rename_(.dots=setNames(list(usage_col), "usage_ccf")) %>%
    dplyr::rename_(.dots=setNames(list(month_col), "usage_month")) %>%
    dplyr::rename_(.dots=setNames(list(year_col), "usage_year")) %>%
    dplyr::rename_(.dots=setNames(list(et_col), "et_amount")) %>%
    dplyr::rename_(.dots=setNames(list(hhsize_col), "hhsize")) %>%
    dplyr::rename_(.dots=setNames(list(irr_area_col), "irr_area")) %>%
    dplyr::rename_(.dots=setNames(list(rate_code_col), "rate_code"))
  
  print("...loaded.")
  return(data)
}

#******************************************************************
# Calculate the variable portion of a bill
#******************************************************************
calculate_variable_bill <- function(data, rate_type, tier_start_str=NULL, tier_price_str,
                                    gpcd=NULL, plant_factor=NULL){
  print("Calculating Variable bill.")
  
  tier_prices <- parse_numerics(tier_price_str)
  
  #call correct bill calculator function
  if(rate_type == "Flat"){
    bills <- calculate_flat_charge(data, tier_prices[1])
  }
  else if(rate_type == "Tiered"){
    tier_starts <- parse_numerics(tier_price_str)
    #check that prices are same length as tiers
    stopifnot(length(tier_starts)==length(tier_prices))
    bills <- calculate_tiered_charge(data, tier_starts, tier_prices)
  }
  else if(rate_type == "Budget"){
    tier_starts <- get_budget_tiers(data, parse_strings(tier_price_str), gpcd, plant_factor)
    #check that prices are same length as tiers
    stopifnot(ncol(tier_starts)==length(tier_prices))
    bills <- calculate_tiered_charge(data, tier_starts, tier_prices, budget_based=TRUE)
  }
  
  return(bills)
}

#******************************************************************
# Calculate a flat rate usage charge
#******************************************************************
calculate_flat_charge <- function(data, price){
  return(data$usage_ccf*price)
}

#******************************************************************
# Calculate a tiered usage charge
#******************************************************************
calculate_tiered_charge <- function(data, tier_starts, tier_prices, budget_based=FALSE){
  usage_in_tiers <- get_usage_in_tiers(data, tier_starts, budget_based=budget_based)
  return(usage_in_tiers%*%tier_prices)
}

#******************************************************************
# Get the amount of water usage in each tier.
#
# Assumes a 1-d vector of tier starts for a normal IBR, and a 
# matrix for budget-based rates (tier start depends on budget)
#******************************************************************
get_usage_in_tiers <- function(data, tier_starts, budget_based=FALSE){
  # tier_stars is a matrix if budget, else is a vector
  if(budget_based){
    num_tiers <- ncol(tier_starts)
  }
  else{
    num_tiers <- length(tier_starts)
  }
  
  # Assumes tier structure starts at 0
  usage_in_tiers <- matrix(0, nrow(data), num_tiers)
  
  for(i in 1:(num_tiers-1) ){
    # tier_stars is a matrix if budget, else is a vector
    if(budget_based){
      t <- tier_starts[,i+1]
    }
    else{
      t <- tier_starts[i+1]
    }
    
    if(i==1){
      # Usage in first tier
      usage_in_tiers[,i] <- pmax(pmin(data$usage_ccf, t), 0)
    }
    else{
      # Usage in middle tiers
      lower_tier_use <- rowSums(usage_in_tiers)
      usage_in_tiers[,i] <- pmax(pmin(data$usage_ccf-lower_tier_use, t-lower_tier_use), 0)
    }
  }
  # Usage in final tier
  lower_tier_use <- rowSums(usage_in_tiers)
  usage_in_tiers[,num_tiers] <- pmax(data$usage_ccf-lower_tier_use, 0)
  
  return(usage_in_tiers)
}

#******************************************************************
# d
#******************************************************************
parse_strings <- function(str){
  return( unlist(strsplit(str, "[\n| ]+")) )
}

parse_numerics <- function(str){
  return( as.numeric(parse_strings(str)) )
}

get_indoor_tier <- function(data, gpcd){
  return(data$hhsize*gpcd*30.4/748)
}

get_outdoor_tier <- function(data, plant_factor){
  return(data$irr_area*data$et_amount*plant_factor*0.62/748)
}

get_budget_tiers <- function(data, tier_start_strs, gpcd, plant_factor){
  indoor <- get_indoor_tier(data, gpcd)
  outdoor <- get_outdoor_tier(data, plant_factor)
  budget <- indoor+outdoor
  budget_tiers <- matrix(0, nrow(data), length(tier_start_strs))
  
  for(i in 1:length(tier_start_strs)){
    t <- tier_start_strs[i]
    
    # if t is numeric
    if( !is.na(as.numeric(t)) ){
      budget_tiers[,i] <- as.numeric(t)
    }
    else if(tolower(t) == "indoor"){
      budget_tiers[,i] <- indoor
    }
    else if(tolower(t) == "outdoor"){
      budget_tiers[,i] <- outdoor
    }
    else if( grepl("%", t) ){
      percent <- as.numeric( gsub("[^0-9\\.]", "", t, "") )
      stopifnot(is.finite(percent))
      
      budget_tiers[,i] <- (percent/100)*budget
    }
  }
  
  return(budget_tiers)
}








