context("planned_df")


#run app once before running this unit test so that df gets loaded in the environment
#Customer Class Generation test function
cust_class_generation_func <- function(sample_df = df , cust_class_vec){
 
 recent_date <- max(df$usage_date)

 recent_month_data <- df %>% filter(usage_date == recent_date)

 class_proportions <- as.data.frame(prop.table(table(df$cust_class)), stringsAsFactors = FALSE)

#pass input$Commercial,input$Institutional, input$Irrigation,input$Other,input$ResidentialMulti,
#input$ResidentialSingle values
 class_proportions$Freq <- cust_class_vec

 new_recent_month_data <- recent_month_data

 for(j in 1:nrow(class_proportions)){
  if(class_proportions$Freq[j] >= 0){
    new_recent_month_data[(nrow(new_recent_month_data)+1):(nrow(new_recent_month_data)+(class_proportions$Freq[j])), "cust_class"] <- class_proportions$Var1[j]
    
  }else{
    
    to_del_accounts <- sample_n(filter(new_recent_month_data,cust_class == class_proportions$Var1[j]), size = abs(class_proportions$Freq[j]))
    new_recent_month_data <- new_recent_month_data %>% filter(!sort_index %in% to_del_accounts$sort_index)
    
    
  }
 }
 return(new_recent_month_data)
}


#test for sum of each individual class
test_that("customer class generation is OK", {

  #class_proportions$Freq <- c(-1, -1, 1, 1, 1, 5)
  #new_recent_month_data <- recent_month_data
  new_recent_month_data <- cust_class_generation_func(sample_df = df, cust_class_vec = c(-1,-1,1,1,1,5))
  recent_date <- max(df$usage_date)
  recent_month_data <- df %>% filter(usage_date == recent_date)
  class_proportions <- as.data.frame(prop.table(table(df$cust_class)), stringsAsFactors = FALSE)
  class_proportions$Freq <- c(-1,-1,1,1,1,5)
  
  
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
#test_dir("C:/Users/anude/Documents/Github/RateComparison/tests/testthat")
