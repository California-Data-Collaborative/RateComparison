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
# Calculate total bill, given data and dashboard inputs
#******************************************************************
calculate_total_bill <- function(data, rate_type, tier_starts, tier_prices){
  bills <-  switch(rate_type,
                  Flat = calculate_flat_bill(data, tier_starts[1], tier_prices[1]),
                  Tiered = calculate_tiered_bill(data, tier_starts, tier_prices),
                  Budget = calculate_budget_bill(data, tier_starts, tier_prices)
                  )
  return(bills)
}






