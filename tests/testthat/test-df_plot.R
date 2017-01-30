context("Df_for_plots")


#Have to make all reactive functions normal functions for testing
test_that("df_original is OK", {
  #sample test to check all columns are present
  all_fields <- c('cust_id', 'usage_month','usage_year','usage_date','usage_ccf','et_amount','hhsize','irr_area',
                  'cust_class','rate_code','meter_size','water_type','sort_index','variable_bill','total_bill',
                  'X1','X2','X3','X4','XR1','XR2','XR3','XR4','XR5','hypothetical_usage','baseline_variable_bill',
                  'baseline_bill','B1','B2','B3','B4','BR1','BR2','BR3','BR4','BR5','baseline_usage')
  expect_equal(colnames(df_plots, all_fields))
})

#total_bill_info is ok
test_that("df plot hypothetical values are OK", {
  total_bill_info <- data.frame(variable_bill = double(), total_bill = double(), X1 = double(), X2 = double(), 
                                X3 = double(), X4 = double(), XR1 = double(), XR2 = double(), XR3 = double(), XR4 = double(),
                                X5 = double(), XR5 = double(), hypothetical_usage = double())
  
  #have to simulate hypothetical_rate_list
  bill_info <- RateParser::calculate_bill(df[1:500, ],hypothetical_rate_list) 
  
  bill_info <- bill_info %>% ungroup %>% dplyr::arrange(sort_index)
  
  bill_info <- bill_info %>% dplyr::rename(variable_bill=commodity_charge,
                                           total_bill=bill)
  
  #adding baseline usage
  bill_info$hypothetical_usage <- bill_info$usage_ccf
  
  # select and return only relevent columns
  mask <- grepl("XR?[0-9]|variable.*|total.*|hypothetical.*", names(bill_info))
  bill_info <- bill_info[mask]
  expect_equal(colnames(bill_info), colnames(total_bill_info))
  
  
})

