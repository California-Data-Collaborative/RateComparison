
# Load functions
source("helper_fns.R", local=TRUE)
source("make_plots.R", local=TRUE)


shinyServer(function(input, output, clientData, session) {

  # Get the indoor tier cutoffs
  indoor <- reactive({
    get_indoor_tier(df, input$galPerCapitaSlider)
  })
  # Get the outdoor tier cutoffs
  outdoor <- reactive({
    get_outdoor_tier(df, input$ETFactorSlider)
  })
  
  #******************************************************************
  # Calculate variable potion of the bill, dependent on rate type
  #******************************************************************
  variable_charge <- reactive({ 
    
    if(input$rateType == "Flat"){
      bill_info <- calculate_variable_bill(data=df, rate_type=input$rateType, 
                                   tier_price_str=as.character(input$flatRate))
    }
    else if(input$rateType == "Tiered"){
      bill_info <- calculate_variable_bill(data=df, rate_type=input$rateType, 
                                   tier_start_str=input$tieredTiers,
                                   tier_price_str=input$tieredPrice)
    }
    else if(input$rateType == "Budget"){
      print(paste("Rate type: ", input$rateType, input$budgetTiers, input$budgetPrice, 
                  input$galPerCapitaSlider, input$ETFactorSlider))
      bill_info <- calculate_variable_bill(data=df, rate_type=input$rateType, 
                                   tier_start_str=input$budgetTiers,
                                   tier_price_str=input$budgetPrice,
                                   indoor_tier=indoor(), 
                                   outdoor_tier=outdoor())
    }
    print( paste("Variable Revenue:",sum(bill_info$variable_bill, na.rm=TRUE)) )
    bill_info
  })
  
  #******************************************************************
  # Calculate total bill
  #******************************************************************
  total_bill_info <- reactive({
    bill_info <- variable_charge() 
    bill_info$total_bill <- bill_info$variable_bill + input$fixedCharge
    return(bill_info)
  })
  
  #******************************************************************
  # Get the filtered dataframe with all billing and tier information
  #******************************************************************
  df_plots <- reactive({
    combined <- dplyr::bind_cols(df, total_bill_info(), baseline_bill_info()) %>%
           filter(rate_code %in% c("RESIDENTIAL_SINGLE"),
                  usage_date >= input$timeSlider[1],
                  usage_date <= input$timeSlider[2])
    combined
  })
  
  #******************************************************************
  # Calculate bills and tiers for the MNWD residential baseline rate
  #******************************************************************
  baseline_bill_info <- reactive({
    switch(utility_code,
           "MNWD"=mnwd_baseline(),
           "LVMWD"=lvmwd_baseline()
    )
  })

  #******************************************************************
  # Line plot of revenue over time
  #******************************************************************
  output$revenue_time_series <- renderPlotly({
    # print(glimpse(df_plots()[1,]))
    p <- plot_revenue_over_time( df_plots() )
    # ggplotly(p)
    p
  }) 
  
  #******************************************************************
  # Bar chart of revenue/usage by tier
  #******************************************************************
  output$barchart_by_tiers <- renderPlotly({
    plot_barchart_by_tiers( df_plots(), input$displayType, input$barType )
  })
  
  #******************************************************************
  # Reactive dataframe of changes to amount paid
  #******************************************************************
  df_change <- reactive({
    start.time <- Sys.time()
    
    df_change <- df_plots() %>% group_by(cust_id) %>% 
      summarise(total_bill=sum(total_bill, na.rm=TRUE), 
                baseline_bill=sum(baseline_bill, na.rm=TRUE)) %>%
      dplyr::select(total_bill, baseline_bill) %>% 
      mutate(changes=total_bill-baseline_bill, change_group=1) %>%
      filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE))
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print("Calcing df_change")
    print(time.taken)
    
    df_change
  })
  
  # Plot histogram
  output$bill_change_histogram <- renderPlotly({
    plot_bill_change_histogram( df_change() )
  })
  
  # Plot boxplot
  output$bill_change_boxplot <- renderPlotly({
    plot_bill_change_boxplot( df_change() )
  })

})



mnwd_baseline <- function(){
  bill_info <- calculate_variable_bill(data=df, rate_type="Budget", 
                                       tier_start_str="0\nIndoor\n101%\n126%\n151%",
                                       tier_price_str="1.49\n1.70\n2.62\n4.38\n9.17",
                                       indoor_tier=get_indoor_tier(df, 60), 
                                       outdoor_tier=get_outdoor_tier(df, 0.7))
  
  num_tiers <- length(parse_strings("1.49\n1.70\n2.62\n4.38\n9.17"))
  colnames(bill_info) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_info$baseline_bill <- bill_info$baseline_variable_bill + 11.39
  
  return(bill_info)
}

lvmwd_baseline <- function(){
  
  #2014 rate
  tmp <- filter(df, usage_year < 2015)
  bill_2014 <- calculate_variable_bill(data=tmp, 
                                       rate_type="Tiered", 
                                       tier_start_str="0\n16\n67\n200",
                                       tier_price_str="2.19\n2.60\n3.56\n5.02")
  num_tiers <- length(parse_strings("0\n16\n67\n200"))
  colnames(bill_2014) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_2014 <- bill_2014 %>% mutate(baseline_bill=baseline_variable_bill + 30.21) 

  
  #2015 before the September switch to monthly billing
  tmp <- filter(df, usage_year >= 2015, usage_year < 2016, usage_month < 9)
  bill_2015_1 <- calculate_variable_bill(data=tmp, 
                                       rate_type="Tiered", 
                                       tier_start_str="0\n16\n67\n200",
                                       tier_price_str="2.23\n2.72\n3.73\n5.26")
  num_tiers <- length(parse_strings("0\n16\n67\n200"))
  colnames(bill_2015_1) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_2015_1 <- bill_2015_1 %>% mutate(baseline_bill=baseline_variable_bill + 31.73) 

  
  #2015 after the September switch to monthly billing
  tmp <- filter(df, usage_date >= as.Date("2015-09-01"), usage_year < 2016)
  bill_2015_2 <- calculate_variable_bill(data=tmp, 
                                       rate_type="Tiered", 
                                       tier_start_str="0\n8\n34\n100",
                                       tier_price_str="2.23\n2.72\n3.73\n5.26")
  num_tiers <- length(parse_strings("0\n16\n67\n200"))
  colnames(bill_2015_2) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_2015_2$baseline_bill <- bill_2015_2$baseline_variable_bill + 15.87

  
  #2016 budgets
  tmp <- filter(df, usage_year >= 2016)
  bill_2016 <- calculate_variable_bill(data=tmp, 
                                   rate_type="Budget", 
                                   tier_start_str="0\nIndoor\n101%\n151%",
                                   tier_price_str="2.36\n3.18\n3.96\n4.98",
                                   indoor_tier=get_indoor_tier(tmp, 55), 
                                   outdoor_tier=get_outdoor_tier(tmp, 0.8))
  num_tiers <- length(parse_strings("2.36\n3.18\n3.96\n4.98"))
  colnames(bill_2016) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_2016$baseline_bill <- bill_2016$baseline_variable_bill + 18.30

  
  return( bind_rows(bill_2014, bill_2015_1, bill_2015_2, bill_2016) )
}
