context("Df_change_for_plots")

#read 1. sample_df_plots1.csv for changes in total and variable bills
#     2. sample_df_plots2.csv for no changes in bills   

sample_df_plots <- read.csv("sample_df_plots1.csv", stringsAsFactors = FALSE)

df_change_test_func <- function(df = sample_df_plots, barType = "Absolute", displayType = "Revenue"){

  df_change <- df %>% group_by(cust_id) %>%
    summarise(total_bill=sum(total_bill, na.rm=TRUE),
              baseline_bill=sum(baseline_bill, na.rm=TRUE),
              hypothetical_usage=sum(hypothetical_usage, na.rm=TRUE), #calculating hypothetical and baseline usages
              baseline_usage=sum(baseline_usage, na.rm=TRUE)) %>%
    dplyr::select(total_bill, baseline_bill, hypothetical_usage, baseline_usage)

  if(barType == "Absolute"){
    #calucating differences in usage
    df_change <- df_change %>%
      mutate(changes=total_bill-baseline_bill, changes_in_usage=hypothetical_usage-baseline_usage, change_group=1)
  }else{
    #calucating percent differences in usage
    df_change <- df_change %>%
      mutate(changes=((total_bill-baseline_bill)/baseline_bill)*100, changes_in_usage=((hypothetical_usage-baseline_usage)/baseline_usage)*100, change_group=1)
  }

  if (displayType == "Revenue"){
    df_change <- df_change %>% filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE))
  }
  else{
    df_change <- df_change %>% filter(abs(changes_in_usage) < mean(changes_in_usage, na.rm=TRUE) +
                                        2.5*sd(changes_in_usage, na.rm=TRUE))
  }

}

test_that("df change is OK", {
  
  df_output <- df_change_test_func()
  
  expect_equal(nrow(df_change1), 493)
  expect_equal(colnames(df_change1), df_change_fields)
  
})






#test_dir("C:/Users/avanjavakam/RateComparison/tests/testthat")