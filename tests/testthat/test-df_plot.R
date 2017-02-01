context("Df_for_plots")




#total_bill_info
test_that("df plot hypothetical info are OK", {
  total_bill_info <- c("variable_bill","total_bill","X1","X2","X3","X4","XR1","XR2","XR3","XR4","X5","XR5","hypothetical_usage")
  

  #change rates in mnwd owrs txt
  hypothetical_rate_list <- RateParser::read_owrs_file("mnwd.owrs.txt")
  
  bill_info <- RateParser::calculate_bill(df[1:5000, ],hypothetical_rate_list) 
  
  bill_info <- bill_info %>% ungroup %>% dplyr::arrange(sort_index)
  
  bill_info <- bill_info %>% dplyr::rename(variable_bill=commodity_charge,
                                           total_bill=bill)
  
  
  bill_info$hypothetical_usage <- bill_info$usage_ccf
  
  # select and return only relevent columns
  mask <- grepl("XR?[0-9]|variable.*|total.*|hypothetical.*", names(bill_info))
  bill_info <- bill_info[mask]
  
  expect_equal(colnames(bill_info), total_bill_info)
  
  
  
})


#baseline_bill_info 
test_that("df plot baseline info OK", {
  
  baseline_bill_info <- c("cust_id", "usage_month", "usage_year", "usage_date", "usage_ccf", "et_amount", "hhsize", "irr_area", "cust_class", "rate_code",
                          "meter_size", "water_type", "sort_index", "service_charge", "budget", "tier_starts", "tier_prices",
                          "baseline_variable_bill", "baseline_bill", "landscape_factor", "outdoor", "B1", "B2", "B3", "B4","BR1",
                          "BR2", "BR3", "BR4", "gpcd", "days_in_period",  "indoor", "B5", "BR5", "baseline_usage")
    


 base_rate_list <- RateParser::read_owrs_file("mnwd.owrs.txt")
  
 bill_info <- RateParser::calculate_bill(df[1:5000, ], baseline_rate_list)
 
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

 expect_equal(colnames(bill_info), baseline_bill_info)



})

#test_dir("C:/Users/avanjavakam/RateComparison/tests/testthat")
