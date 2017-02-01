context("planned_df")



test_that("customer class generation is OK", {
  
  recent_date <- max(df$usage_date)
  
  recent_month_data <- df %>% filter(usage_date == recent_date)
  
  class_proportions <- as.data.frame(prop.table(table(df$cust_class)), stringsAsFactors = FALSE)
  
  #pass input$Commercial,input$Institutional, input$Irrigation,input$Other,input$ResidentialMulti,
  #input$ResidentialSingle values
  class_proportions$Freq <- c(1, 1, 1, -1, 1, 5)
  
  new_recent_month_data <- recent_month_data
    
    for(j in 1:nrow(class_proportions)){
      if(class_proportions$Freq[j] >= 0){
        new_recent_month_data[(nrow(new_recent_month_data)+1):(nrow(new_recent_month_data)+(class_proportions$Freq[j])), "cust_class"] <- class_proportions$Var1[j]
        
      }else{
        
        to_del_accounts <- sample_n(filter(new_recent_month_data,cust_class == class_proportions$Var1[j]), size = abs(class_proportions$Freq[j]))
        new_recent_month_data <- new_recent_month_data %>% filter(!sort_index %in% to_del_accounts$sort_index)
      
        
      }
    }
  
  #class_proportions$Freq <- c(-1, -1, 1, 1, 1, 5)
  #new_recent_month_data <- recent_month_data
  
  expect_equal(sum(new_recent_month_data$cust_class == 'RESIDENTIAL_SINGLE'), sum(recent_month_data$cust_class == 'RESIDENTIAL_SINGLE')
                                                                              +class_proportions$Freq[6])
  expect_equal(sum(new_recent_month_data$cust_class == 'RESIDENTIAL_MULTI'), sum(recent_month_data$cust_class == 'RESIDENTIAL_MULTI')
                                                                              +class_proportions$Freq[5])
  expect_equal(sum(new_recent_month_data$cust_class == 'OTHER'), sum(recent_month_data$cust_class == 'OTHER')
                                                                              +class_proportions$Freq[4])
  expect_equal(sum(new_recent_month_data$cust_class == 'IRRIGATION'), sum(recent_month_data$cust_class == 'IRRIGATION')
                                                                              +class_proportions$Freq[3])
  expect_equal(sum(new_recent_month_data$cust_class == 'INSTITUTIONAL'), sum(recent_month_data$cust_class == 'INSTITUTIONAL')
                                                                              +class_proportions$Freq[2])
  expect_equal(sum(new_recent_month_data$cust_class == 'COMMERCIAL'), sum(recent_month_data$cust_class == 'COMMERCIAL')
                                                                              +class_proportions$Freq[1])
  
  
})

#test_dir("C:/Users/avanjavakam/RateComparison/tests/testthat")
