context("Df_change_for_plots")


##############In Progress##############
sample_df_plots <- data.frame(cust_id = c(833857,833858), usage_month = c(1,1),  usage_year = c(2014,2014),
                              usage_date = c(2014-01-01,2014-01-01), usage_ccf =c(5,20), et_amount = c(2.5,2.5),
                              hhsize = c(4,4), irr_area = c(1134,1000), cust_class = c("RESIDENTIAL_SINGLE","RESIDENTIAL_SINGLE"),
                              rate_code = c('R1','R1'), meter_size = c("1\"","1\""), water_type = c("POTABLE","POTABLE"),
                              sort_index = c(14736,14789), variable_bill = c(10,10),total_bill=c(80,100), X1=c(5,5), X2=c(0,0), 
                              X3=c(0,0), X4=c(0,0), XR1=c(7.45,7.45), XR2=c(0,0), XR3=c(0,0), XR4=c(0,0), X5=c(0,0), XR5=c(0,0),
                              hypothetical_usage=c(20,30), baseline_variable_bill=c(7.45,7.45), baseline_bill=c(10,15),
                              B1=c(5,5), B2=c(0,0), B3=c(0,0), B4=c(0,0), BR1=c(7.45,7.45), BR2=c(0,0), BR3=c(0,0),
                              BR4=c(0,0), B5=c(0,0), BR5=c(0,0), baseline_usage=c(4,6))        


#test df_change function
df_change_test_func <- function(df = sample_df_plots, barType = "Absolute", displayType = "Revenue"){

  first_step <- df %>% group_by(cust_id) %>%
    summarise(total_bill=sum(total_bill, na.rm=TRUE),
              baseline_bill=sum(baseline_bill, na.rm=TRUE),
              hypothetical_usage=sum(hypothetical_usage, na.rm=TRUE), #calculating hypothetical and baseline usages
              baseline_usage=sum(baseline_usage, na.rm=TRUE)) %>%
    dplyr::select(total_bill, baseline_bill, hypothetical_usage, baseline_usage)
  
  if(barType == "Absolute"){
    #calucating differences in usage
    second_step <- first_step %>%
      mutate(changes=total_bill-baseline_bill, changes_in_usage=hypothetical_usage-baseline_usage, change_group=1)
  }else{
    #calucating percent differences in usage
    second_step <- first_step %>%
      mutate(changes=((total_bill-baseline_bill)/baseline_bill)*100, changes_in_usage=((hypothetical_usage-baseline_usage)/baseline_usage)*100, change_group=1)
  }

  if (displayType == "Revenue"){
    third_step <- second_step %>% filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE))
  }
  else{
    third_step <- second_step %>% filter(abs(changes_in_usage) < mean(changes_in_usage, na.rm=TRUE) +
                                        2.5*sd(changes_in_usage, na.rm=TRUE))
  }
  test_list <- list(first_step, second_step, third_step)
  return(test_list)
}

test_that("df change is OK", {
  
  df_output <- df_change_test_func(df = sample_df_plots, barType = "Absolute", displayType = "Revenue")
  
  #check whether columns are populated correctly
  expect_equal(colnames(df_output[[1]]), c("total_bill", "baseline_bill", "hypothetical_usage", "baseline_usage"))
  expect_equal(colnames(df_output[[2]]), c("total_bill", "baseline_bill", "hypothetical_usage", "baseline_usage","changes",
                                           "changes_in_usage", "change_group"))
  expect_equal(colnames(df_output[[3]]), c("total_bill", "baseline_bill", "hypothetical_usage", "baseline_usage","changes",
                                           "changes_in_usage", "change_group"))
  
  #check group by and summarise
  expect_equal(df_output[[1]],data.frame(total_bill = c(80,100),baseline_bill = c(10,15),hypothetical_usage = c(20,30),
                                         baseline_usage = c(4,6)))
  
  #check for changes in revenue and usage
  expect_equal(df_output[[2]], data.frame(total_bill = c(80,100),baseline_bill = c(10,15),hypothetical_usage = c(20,30),
                                         baseline_usage = c(4,6), changes = c(70,85), changes_in_usage = c(16,24),
                                         change_group = c(1,1)))
  

  #check for changes only within 2.5 times their SD 
  expect_equal(df_output[[3]], data.frame(total_bill = c(80,100),baseline_bill = c(10,15),hypothetical_usage = c(20,30),
                                          baseline_usage = c(4,6), changes = c(70,85), changes_in_usage = c(16,24),
                                          change_group = c(1,1)))
  
})






#test_dir("C:/Users/avanjavakam/RateComparison/tests/testthat")
#test_dir("C:/Users/anude/Documents/Github/RateComparison/tests/testthat")