context("Df_for_plots")

#sample data frame
sample_df <- data.frame(cust_id = 887765,	usage_month = 11,	usage_year = 2014,	usage_date = '11/1/2014',
                         usage_ccf = 109,	et_amount = 2.4,	hhsize = 0,	irr_area = 1000,
                         cust_class = 'RESIDENTIAL_SINGLE',	rate_code = 'RC1',	
                         meter_size = "2\"",	water_type = "RECYCLED",sort_index = 44444)


#total_bill_info function
total_bill_info_func <- function(df = sample_df, owrs_file = "mnwd.owrs.txt"){
  
  hypothetical_rate_list <- RateParser::read_owrs_file(owrs_file)
  
  bill_info <- RateParser::calculate_bill(df,hypothetical_rate_list) 
  
  bill_info <- bill_info %>% ungroup %>% dplyr::arrange(sort_index)
  
  bill_info <- bill_info %>% dplyr::rename(variable_bill=commodity_charge,
                                           total_bill=bill)
  
  
  bill_info$hypothetical_usage <- bill_info$usage_ccf
  
  # select and return only relevent columns
  mask <- grepl("XR?[0-9]|variable.*|total.*|hypothetical.*", names(bill_info))
  bill_info <- bill_info[mask]
  
  
}




#total_bill_info test
test_that("df plot hypothetical info is OK", {
  total_bill_info <- c("X1","X2","X3","X4","X5","XR1","XR2","XR3","XR4","XR5","variable_bill","total_bill",
                       "hypothetical_usage")
  
  bill_info <- total_bill_info_func()
  expect_equal(colnames(bill_info), total_bill_info)
  
  
  
})

#baseline_bill_info function
baseline_bill_info_func <- function(df = sample_df, owrs_file = "mnwd.owrs.txt"){
  
  base_rate_list <- RateParser::read_owrs_file(owrs_file)
  
  bill_info <- RateParser::calculate_bill(df, baseline_rate_list)
  
  bill_info <- bill_info %>% ungroup %>% dplyr::arrange(sort_index)
  
  #mask for columns representing tier usage
  usage_col_mask <- grepl("X[0-9]", names(bill_info))
  revenue_col_mask <- grepl("XR[0-9]", names(bill_info))
  num_tiers <- sum(usage_col_mask)
  colnames(bill_info)[usage_col_mask] <- c( paste("B", 1:num_tiers, sep=""))
  colnames(bill_info)[revenue_col_mask] <- c( paste("BR", 1:num_tiers, sep=""))
  
  bill_info <- bill_info %>% dplyr::rename(baseline_variable_bill=commodity_charge,
                                           baseline_bill=bill)
  #adding baseline usage
  bill_info$baseline_usage <- bill_info$usage_ccf
  
  return(bill_info)
}

#baseline_bill_info test
test_that("df plot baseline info OK", {
  
  baseline_bill_info <- c("cust_id", "usage_month", "usage_year", "usage_date", "usage_ccf", "et_amount", "hhsize",
                          "irr_area", "cust_class", "rate_code", "meter_size", "water_type", "sort_index", "service_charge",
                          "gpcd", "landscape_factor", "days_in_period", "indoor", "outdoor", "budget", "tier_starts",
                          "tier_prices", "tier_starts.1", "tier_prices.1", "B1", "B2", "B3", "B4","B5", "BR1", "BR2", "BR3",
                          "BR4", "BR5", "baseline_variable_bill", "baseline_bill", "baseline_usage")
    

 bill_info <- baseline_bill_info_func(df = sample_df)
 
 expect_equal(colnames(bill_info), baseline_bill_info)

})

#test_dir("C:/Users/avanjavakam/RateComparison/tests/testthat")
