context("Df_for_plots")



test_that("df_original is OK", {
  #sample test to check all columns are present
  all_fields <- c('cust_id', 'usage_month','usage_year','usage_date','usage_ccf','et_amount','hhsize','irr_area',
                  'cust_class','rate_code','meter_size','water_type','sort_index','variable_bill','total_bill',
                  'X1','X2','X3','X4','XR1','XR2','XR3','XR4','XR5','hypothetical_usage','baseline_variable_bill',
                  'baseline_bill','B1','B2','B3','B4','BR1','BR2','BR3','BR4','BR5','baseline_usage')
  expect_equal(colnames(df_plots()), all_fields)
})