context("Df_change_for_plots")



test_that("df plot baseline values are OK", {
  
  baseline_bill_info <- data.frame(baseline_variable_bill = numeric(), baseline_bill = numeric(), B1 = numeric(), B2 = numeric(), 
                                   B3 = numeric(), B4 = numeric(), BR1 = numeric(), BR2 = numeric(), BR3 = numeric(), BR4 = numeric(),
                                   B5 = numeric(), BR5 = numeric(), baseline_usage = numeric())
  
  #####calculate bills manually in the above data frame for first 500 rows######
  
  
  ############################################################
  
  base_rate_list <- RateParser::read_owrs_file("mnwd.owrs.txt")
  
  bill_info <- RateParser::calculate_bill(df[1:500, ], baseline_rate_list)
  
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
  
  expect_equal(bill_info$total_bill, total_bill_info$total_bill)
  expect_equal(bill_info$variable_bill, total_bill_info$variable_bill)
  
  
})