

source("helper_fns.R", local=TRUE)

df <- read_data("data/mnwd_test.csv", usage_col="usage_ccf", month_col="usage_month", 
                year_col="usage_year", et_col="usage_et_amount", hhsize_col="cust_loc_hhsize", 
                irr_area_col="cust_loc_irr_area_sf", rate_code_col= "cust_loc_class_from_utility")


shinyServer(function(input, output, clientData, session) {

  

})
