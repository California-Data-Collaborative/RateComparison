
# Load functions
source("R/helper_fns.R", local=TRUE)
source("R/make_plots.R", local=TRUE)


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
    bill_info <- calculate_variable_bill(data=df, rate_type=input$rateType, 
                                         tier_starts=tier_info()$starts,
                                         tier_prices=tier_info()$prices )
    
    print( paste("Variable Revenue:",sum(bill_info$variable_bill, na.rm=TRUE)) )
    bill_info
  })
  
  tier_info <- reactive({
    tier_info <- list()
    if(input$rateType == "Flat"){
      tier_info$starts <- NULL
      tier_info$prices <- parse_numerics(as.character(input$flatRate))
    }
    else if(input$rateType == "Tiered"){
      tier_info$starts <- parse_numerics(input$tieredTiers)
      tier_info$prices <- parse_numerics(input$tieredPrice)
    }
    else if(input$rateType == "Budget"){
      tier_info$starts <- budget_tier_starts()
      tier_info$prices <- parse_numerics(input$budgetPrice)
    }
    tier_info
  })
  
  budget_tier_starts <- reactive({
    get_budget_tiers(df, parse_strings(input$budgetTiers), indoor(), outdoor())
  })
  
  #******************************************************************
  # Calculate total bill
  #******************************************************************
  total_bill_info <- reactive({
    bill_info <- variable_charge() 
    bill_info$total_bill <- bill_info$variable_bill + input$fixedCharge
    
    bill_info$hypothetical_usage <- bill_info %>% select(matches("[X][0-9]")) %>% rowSums() #adding hypothetical usage
  
    return(bill_info)
    
  })
  
  
  #******************************************************************
  # Get the filtered dataframe with all billing and tier information
  #******************************************************************
  df_plots <- reactive({
    combined <- dplyr::bind_cols(df, total_bill_info(), baseline_bill_info()) %>%
      filter(usage_date >= input$timeSlider[1],
             usage_date <= input$timeSlider[2],
             rate_code %in% input$RateCode) 
    combined 
  })
  
  df_plots1 <- reactive({
    combined <- dplyr::bind_cols(df, total_bill_info(), baseline_bill_info()) %>%
      filter(usage_date >= input$timeSlider[1],
             usage_date <= input$timeSlider[2],
             rate_code %in% input$RateCode1 ) 
    combined 
  })
  
  df_plots2 <- reactive({
    combined <- dplyr::bind_cols(df, total_bill_info(), baseline_bill_info()) %>%
      filter(usage_date >= input$timeSlider[1],
             usage_date <= input$timeSlider[2],
             rate_code %in% input$RateCode2  ) 
    combined 
  })
  
  df_plots3 <- reactive({
    combined <- dplyr::bind_cols(df, total_bill_info(), baseline_bill_info()) %>%
      filter(usage_date >= input$timeSlider[1],
             usage_date <= input$timeSlider[2],
             rate_code %in% input$RateCode3) 
    combined 
  })
  
  df_plots4 <- reactive({
    combined <- dplyr::bind_cols(df, total_bill_info(), baseline_bill_info()) %>%
      filter(usage_date >= input$timeSlider[1],
             usage_date <= input$timeSlider[2],
             rate_code %in% input$RateCode4) 
    combined 
  })
  
  df_plots5 <- reactive({
    combined <- dplyr::bind_cols(df, total_bill_info(), baseline_bill_info()) %>%
      filter(usage_date >= input$timeSlider[1],
             usage_date <= input$timeSlider[2],
             rate_code %in% input$RateCode5) 
    combined 
  })
  
  
  
  #******************************************************************
  # Calculate bills and tiers for the MNWD residential baseline rate
  #******************************************************************
  baseline_bill_info <- reactive({
    switch(utility_code,
           "IRWD"=irwd_baseline(),
           "MNWD"=mnwd_baseline(),
           "LVMWD"=lvmwd_baseline(),
           "SMWD"=smwd_baseline(),
           "SMC"=smc_baseline()
    )
  })
  
  #******************************************************************
  # Line plot of revenue over time
  #******************************************************************
  output$revenue_time_series <- renderPlotly({
    # print(glimpse(df_plots()[1,]))
    p <- plot_revenue_over_time( df_plots(), input$displayType )
    # ggplotly(p)
    p
  }) 
  
  output$revenue_time_series1 <- renderPlotly({
    # print(glimpse(df_plots()[1,]))
    p <- plot_revenue_over_time( df_plots1(), input$displayType )
    # ggplotly(p)
    p
  }) 
  output$revenue_time_series2 <- renderPlotly({
    # print(glimpse(df_plots()[1,]))
    p <- plot_revenue_over_time( df_plots2(), input$displayType )
    # ggplotly(p)
    p
  }) 
  output$revenue_time_series3 <- renderPlotly({
    # print(glimpse(df_plots()[1,]))
    p <- plot_revenue_over_time( df_plots3(), input$displayType )
    # ggplotly(p)
    p
  }) 
  output$revenue_time_series4 <- renderPlotly({
    # print(glimpse(df_plots()[1,]))
    p <- plot_revenue_over_time( df_plots4(), input$displayType )
    # ggplotly(p)
    p
  }) 
  output$revenue_time_series5 <- renderPlotly({
    # print(glimpse(df_plots()[1,]))
    p <- plot_revenue_over_time( df_plots5(), input$displayType )
    # ggplotly(p)
    p
  })
  
  
  #******************************************************************
  # Bar chart of revenue/usage by tier
  #******************************************************************
  output$barchart_by_tiers <- renderPlotly({
    plot_barchart_by_tiers( df_plots(), input$displayType, input$barType )
  })
  output$barchart_by_tiers1 <- renderPlotly({
    plot_barchart_by_tiers( df_plots1(), input$displayType, input$barType )
  })
  output$barchart_by_tiers2 <- renderPlotly({
    plot_barchart_by_tiers( df_plots2(), input$displayType, input$barType )
  })
  output$barchart_by_tiers3 <- renderPlotly({
    plot_barchart_by_tiers( df_plots3(), input$displayType, input$barType )
  })
  output$barchart_by_tiers4 <- renderPlotly({
    plot_barchart_by_tiers( df_plots4(), input$displayType, input$barType )
  })
  output$barchart_by_tiers5 <- renderPlotly({
    plot_barchart_by_tiers( df_plots5(), input$displayType, input$barType )
  })
  
  #******************************************************************
  # Reactive dataframe of changes to amount paid
  #******************************************************************
  df_change <- reactive({
    start.time <- Sys.time()
    
    df_change <- df_plots() %>% group_by(cust_id) %>% 
      summarise(total_bill=sum(total_bill, na.rm=TRUE), 
                baseline_bill=sum(baseline_bill, na.rm=TRUE),
                hypothetical_usage=sum(hypothetical_usage, na.rm=TRUE), #calculating hypothetical and baseline usages
                baseline_usage=sum(baseline_usage, na.rm=TRUE)) %>%
      dplyr::select(total_bill, baseline_bill, hypothetical_usage, baseline_usage) %>% 
                                                                #calucating differences in usage
      mutate(changes=total_bill-baseline_bill, changes_in_usage=hypothetical_usage-baseline_usage, change_group=1) %>%
      filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE),
             abs(changes_in_usage) < mean(changes_in_usage, na.rm=TRUE) + 2.5*sd(changes_in_usage, na.rm=TRUE))
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print("Calcing df_change")
    print(time.taken)
    
    df_change
  })
  
  df_change1 <- reactive({
    start.time <- Sys.time()
    
    df_change1 <- df_plots1() %>% group_by(cust_id) %>% 
      summarise(total_bill=sum(total_bill, na.rm=TRUE), 
                baseline_bill=sum(baseline_bill, na.rm=TRUE),
                hypothetical_usage=sum(hypothetical_usage, na.rm=TRUE), #calculating hypothetical and baseline usages
                baseline_usage=sum(baseline_usage, na.rm=TRUE)) %>%
      dplyr::select(total_bill, baseline_bill, hypothetical_usage, baseline_usage) %>% 
                                                                #calucating differences in usage                                                        
      mutate(changes=total_bill-baseline_bill, changes_in_usage=hypothetical_usage-baseline_usage, change_group=1) %>%
      filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE),
             abs(changes_in_usage) < mean(changes_in_usage, na.rm=TRUE) + 2.5*sd(changes_in_usage, na.rm=TRUE))
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print("Calcing df_change1")
    print(time.taken)
    
    df_change1
  })
  
  df_change2 <- reactive({
    start.time <- Sys.time()
    
    df_change2 <- df_plots2() %>% group_by(cust_id) %>% 
      summarise(total_bill=sum(total_bill, na.rm=TRUE), 
                baseline_bill=sum(baseline_bill, na.rm=TRUE),
                hypothetical_usage=sum(hypothetical_usage, na.rm=TRUE), #calculating hypothetical and baseline usages
                baseline_usage=sum(baseline_usage, na.rm=TRUE)) %>%
      dplyr::select(total_bill, baseline_bill, hypothetical_usage, baseline_usage) %>%
                                                                #calucating differences in usage
      mutate(changes=total_bill-baseline_bill, changes_in_usage=hypothetical_usage-baseline_usage, change_group=1) %>%
      filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE),
             abs(changes_in_usage) < mean(changes_in_usage, na.rm=TRUE) + 2.5*sd(changes_in_usage, na.rm=TRUE))
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print("Calcing df_change")
    print(time.taken)
    
    df_change2
  })
  
  df_change3 <- reactive({
    start.time <- Sys.time()
    
    df_change3 <- df_plots3() %>% group_by(cust_id) %>% 
      summarise(total_bill=sum(total_bill, na.rm=TRUE), 
                baseline_bill=sum(baseline_bill, na.rm=TRUE),
                hypothetical_usage=sum(hypothetical_usage, na.rm=TRUE), #calculating hypothetical and baseline usages
                baseline_usage=sum(baseline_usage, na.rm=TRUE)) %>%
      dplyr::select(total_bill, baseline_bill, hypothetical_usage, baseline_usage) %>% 
                                                                #calucating differences in usage
      mutate(changes=total_bill-baseline_bill, changes_in_usage=hypothetical_usage-baseline_usage, change_group=1) %>%
      filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE),
             abs(changes_in_usage) < mean(changes_in_usage, na.rm=TRUE) + 2.5*sd(changes_in_usage, na.rm=TRUE))
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print("Calcing df_change")
    print(time.taken)
    
    df_change3
  })
  
  
  df_change4 <- reactive({
    start.time <- Sys.time()
    
    df_change4 <- df_plots4() %>% group_by(cust_id) %>% 
      summarise(total_bill=sum(total_bill, na.rm=TRUE), 
                baseline_bill=sum(baseline_bill, na.rm=TRUE),
                hypothetical_usage=sum(hypothetical_usage, na.rm=TRUE), #calculating hypothetical and baseline usages
                baseline_usage=sum(baseline_usage, na.rm=TRUE)) %>%
      dplyr::select(total_bill, baseline_bill, hypothetical_usage, baseline_usage) %>%
                                                                #calucating differences in usage
      mutate(changes=total_bill-baseline_bill, changes_in_usage=hypothetical_usage-baseline_usage, change_group=1) %>%
      filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE),
             abs(changes_in_usage) < mean(changes_in_usage, na.rm=TRUE) + 2.5*sd(changes_in_usage, na.rm=TRUE))
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print("Calcing df_change")
    print(time.taken)
    
    df_change4
  })
  
  df_change5 <- reactive({
    start.time <- Sys.time()
    
    df_change5 <- df_plots5() %>% group_by(cust_id) %>% 
      summarise(total_bill=sum(total_bill, na.rm=TRUE), 
                baseline_bill=sum(baseline_bill, na.rm=TRUE),
                hypothetical_usage=sum(hypothetical_usage, na.rm=TRUE), #calculating hypothetical and baseline usages
                baseline_usage=sum(baseline_usage, na.rm=TRUE)) %>%
      dplyr::select(total_bill, baseline_bill, hypothetical_usage, baseline_usage) %>%
                                                                #calucating differences in usage
      mutate(changes=total_bill-baseline_bill, changes_in_usage=hypothetical_usage-baseline_usage, change_group=1) %>%
      filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE),
             abs(changes_in_usage) < mean(changes_in_usage, na.rm=TRUE) + 2.5*sd(changes_in_usage, na.rm=TRUE))
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print("Calcing df_change")
    print(time.taken)
    
    df_change5
  })
  
  # Plot histogram
  output$bill_change_histogram <- renderPlotly({
    plot_bill_change_histogram( df_change(), input$displayType )
  })
  output$bill_change_histogram1 <- renderPlotly({
    plot_bill_change_histogram( df_change1(), input$displayType )
  })
  output$bill_change_histogram2 <- renderPlotly({
    plot_bill_change_histogram( df_change2(), input$displayType )
  })
  output$bill_change_histogram3 <- renderPlotly({
    plot_bill_change_histogram( df_change3(), input$displayType )
  })
  output$bill_change_histogram4 <- renderPlotly({
    plot_bill_change_histogram( df_change4(), input$displayType )
  })
  output$bill_change_histogram5 <- renderPlotly({
    plot_bill_change_histogram( df_change5(), input$displayType )
  })
  
  # Plot boxplot
  output$bill_change_boxplot <- renderPlotly({
    plot_bill_change_boxplot( df_change(), input$displayType )
  })
  output$bill_change_boxplot1 <- renderPlotly({
    plot_bill_change_boxplot( df_change1(), input$displayType )
  })
  output$bill_change_boxplot2 <- renderPlotly({
    plot_bill_change_boxplot( df_change2(), input$displayType )
  })
  output$bill_change_boxplot3 <- renderPlotly({
    plot_bill_change_boxplot( df_change3(), input$displayType )
  })
  output$bill_change_boxplot4 <- renderPlotly({
    plot_bill_change_boxplot( df_change4(), input$displayType )
  })
  output$bill_change_boxplot5 <- renderPlotly({
    plot_bill_change_boxplot( df_change5(), input$displayType )
  })
  
  output$fixed_revenue_barchart <- renderPlotly({
    plot_fixed_revenue(df_plots(), input$barType)
  })
  output$fixed_revenue_barchart1 <- renderPlotly({
    plot_fixed_revenue(df_plots1(), input$barType)
  })
  output$fixed_revenue_barchart2 <- renderPlotly({
    plot_fixed_revenue(df_plots2(), input$barType)
  })
  output$fixed_revenue_barchart3 <- renderPlotly({
    plot_fixed_revenue(df_plots3(), input$barType)
  })
  output$fixed_revenue_barchart4 <- renderPlotly({
    plot_fixed_revenue(df_plots4(), input$barType)
  })
  output$fixed_revenue_barchart5 <- renderPlotly({
    plot_fixed_revenue(df_plots5(), input$barType)
  })
  
})



mnwd_baseline <- function(){
  tier_starts <- get_budget_tiers(df, parse_strings("0\nIndoor\n101%\n126%\n151%"), 
                                  get_indoor_tier(df, 60), get_outdoor_tier(df, 0.7))
  bill_info <- calculate_variable_bill(data=df, rate_type="Budget", 
                                       tier_starts=tier_starts,
                                       tier_prices=parse_numerics("1.49\n1.70\n2.62\n4.38\n9.17"))
  
  num_tiers <- length(parse_strings("1.49\n1.70\n2.62\n4.38\n9.17"))
  colnames(bill_info) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_info$baseline_bill <- bill_info$baseline_variable_bill + 11.39
  #adding baseline usage
  bill_info$baseline_usage <- bill_info$B1 + bill_info$B2 + bill_info$B3 + bill_info$B4 + bill_info$B5
  return(bill_info)
}

lvmwd_baseline <- function(){
  
  #2014 rate
  tmp <- filter(df, usage_year < 2015)
  bill_2014 <- calculate_variable_bill(data=tmp, 
                                       rate_type="Tiered", 
                                       tier_starts=parse_numerics("0\n16\n67\n200"),
                                       tier_prices=parse_numerics("2.19\n2.60\n3.56\n5.02") )
  num_tiers <- length(parse_strings("0\n16\n67\n200"))
  colnames(bill_2014) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_2014 <- bill_2014 %>% mutate(baseline_bill=baseline_variable_bill + 30.21)
  #adding baseline usage
  bill_2014$baseline_usage <- bill_2014$B1 + bill_2014$B2 + bill_2014$B3 + bill_2014$B4 + bill_2014$B5
  
  #2015 before the September switch to monthly billing
  tmp <- filter(df, usage_year >= 2015, usage_year < 2016, usage_month < 9)
  bill_2015_1 <- calculate_variable_bill(data=tmp, 
                                         rate_type="Tiered", 
                                         tier_starts=parse_numerics("0\n16\n67\n200"),
                                         tier_prices=parse_numerics("2.31\n2.80\n3.81\n5.34") )
  num_tiers <- length(parse_strings("0\n16\n67\n200"))
  colnames(bill_2015_1) <- c( paste("B", 1:num_tiers, sep=""),
                              paste("BR", 1:num_tiers, sep=""),
                              "baseline_variable_bill")
  bill_2015_1 <- bill_2015_1 %>% mutate(baseline_bill=baseline_variable_bill + 31.73)
  #adding baseline usage
  bill_2015_1$baseline_usage <- bill_2015_1$B1 + bill_2015_1$B2 + bill_2015_1$B3 + bill_2015_1$B4
  
  #2015 after the September switch to monthly billing
  tmp <- filter(df, usage_date >= as.Date("2015-09-01"), usage_year < 2016)
  bill_2015_2 <- calculate_variable_bill(data=tmp, 
                                         rate_type="Tiered", 
                                         tier_starts=parse_numerics("0\n8\n34\n100"),
                                         tier_prices=parse_numerics("2.31\n2.80\n3.81\n5.34") )
  num_tiers <- length(parse_strings("0\n16\n67\n200"))
  colnames(bill_2015_2) <- c( paste("B", 1:num_tiers, sep=""),
                              paste("BR", 1:num_tiers, sep=""),
                              "baseline_variable_bill")
  bill_2015_2$baseline_bill <- bill_2015_2$baseline_variable_bill + 15.87
  #adding baseline usage
  bill_2015_2$baseline_usage <- bill_2015_2$B1 + bill_2015_2$B2 + bill_2015_2$B3 + bill_2015_2$B4
  
  #2016 budgets
  tmp <- filter(df, usage_year >= 2016)
  tier_starts <- get_budget_tiers(tmp, parse_strings("0\nIndoor\n101%\n151%"), 
                                  get_indoor_tier(tmp, 55), get_outdoor_tier(tmp, 0.8))
  bill_2016 <- calculate_variable_bill(data=tmp, 
                                       rate_type="Budget", 
                                       tier_starts=tier_starts,
                                       tier_prices=parse_numerics("2.36\n3.18\n3.96\n4.98") )
  num_tiers <- length(parse_strings("2.36\n3.18\n3.96\n4.98"))
  colnames(bill_2016) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_2016$baseline_bill <- bill_2016$baseline_variable_bill + 18.30
  #adding baseline usage
  bill_2016$baseline_usage <- bill_2016$B1 + bill_2016$B2 + bill_2016$B3 + bill_2016$B4
  
  return( bind_rows(bill_2014, bill_2015_1, bill_2015_2, bill_2016) )
}

smwd_baseline <- function(){
  #before March 2015
  tmp <- filter(df, usage_date < as.Date("2015-03-01"))
  bill_2015_1 <- calculate_variable_bill(data=tmp, 
                                         rate_type="Tiered", 
                                         tier_starts=parse_numerics("0\n7\n21\n36\n71"),
                                         tier_prices=parse_numerics("2.23\n2.46\n2.94\n3.45\n4.33") )
  num_tiers <- length(parse_strings("0\n7\n21\n36\n71"))
  colnames(bill_2015_1) <- c( paste("B", 1:num_tiers, sep=""),
                              paste("BR", 1:num_tiers, sep=""),
                              "baseline_variable_bill")
  bill_2015_1 <- bill_2015_1 %>% mutate(baseline_bill=baseline_variable_bill + 6.41)
  #adding baseline usage
  bill_2015_1$baseline_usage <- bill_2015_1$B1 + bill_2015_1$B2 + bill_2015_1$B3 + bill_2015_1$B4 + bill_2015_1$B5
  
  #after March 2015
  tmp <- filter(df, usage_date >= as.Date("2015-03-01"), usage_year < 2016)
  bill_2015_2 <- calculate_variable_bill(data=tmp, 
                                         rate_type="Tiered", 
                                         tier_starts=parse_numerics("0\n7\n21\n36\n71"),
                                         tier_prices=parse_numerics("2.04\n2.29\n2.77\n3.28\n4.50") )
  num_tiers <- length(parse_strings("0\n7\n21\n36\n71"))
  colnames(bill_2015_2) <- c( paste("B", 1:num_tiers, sep=""),
                              paste("BR", 1:num_tiers, sep=""),
                              "baseline_variable_bill")
  bill_2015_2$baseline_bill <- bill_2015_2$baseline_variable_bill + 8.72
  #adding baseline usage
  bill_2015_2$baseline_usage <- bill_2015_2$B1 + bill_2015_2$B2 + bill_2015_2$B3 + bill_2015_2$B4 + bill_2015_2$B5
  
  #   #2016 budgets
  #   tmp <- filter(df, usage_year >= 2016)
  #   tier_starts <- get_budget_tiers(tmp, parse_strings("0\nIndoor\n101%\n151%"), 
  #                                   get_indoor_tier(tmp, 55), get_outdoor_tier(tmp, 0.8))
  #   bill_2016 <- calculate_variable_bill(data=tmp, 
  #                                        rate_type="Budget", 
  #                                        tier_starts=tier_starts,
  #                                        tier_prices=parse_numerics("2.36\n3.18\n3.96\n4.98") )
  #   num_tiers <- length(parse_strings("2.36\n3.18\n3.96\n4.98"))
  #   colnames(bill_2016) <- c( paste("B", 1:num_tiers, sep=""),
  #                             paste("BR", 1:num_tiers, sep=""),
  #                             "baseline_variable_bill")
  #   bill_2016$baseline_bill <- bill_2016$baseline_variable_bill + 14.89
  
  
  return( bind_rows(bill_2015_1, bill_2015_2) )
}


smc_baseline <- function(){
  #before 2016
  tmp <- filter(df, usage_date < as.Date("2016-01-01"))
  bill_2015 <- calculate_variable_bill(data=tmp, 
                                         rate_type="Tiered", 
                                         tier_starts=parse_numerics("0\n15\n41\n149"),
                                         tier_prices=parse_numerics("2.73\n4.09\n6.13\n9.59") )
  num_tiers <- length(parse_strings("0\n15\n41\n149"))
  colnames(bill_2015) <- c( paste("B", 1:num_tiers, sep=""),
                              paste("BR", 1:num_tiers, sep=""),
                              "baseline_variable_bill")
  bill_2015 <- bill_2015 %>% mutate(baseline_bill=baseline_variable_bill + 0)
  #adding baseline usage
  bill_2015$baseline_usage <- bill_2015$B1 + bill_2015$B2 + bill_2015$B3 + bill_2015$B4
  
  #after 2016
  tmp <- filter(df, usage_date >= as.Date("2016-01-01"))
  bill_2016 <- calculate_variable_bill(data=tmp, 
                                         rate_type="Tiered", 
                                         tier_starts=parse_numerics("0\n15\n41\n149"),
                                         tier_prices=parse_numerics("2.87\n4.29\n6.44\n10.07") )
  
  num_tiers <- length(parse_strings("0\n15\n41\n149"))
  colnames(bill_2016) <- c( paste("B", 1:num_tiers, sep=""),
                              paste("BR", 1:num_tiers, sep=""),
                              "baseline_variable_bill")
  
  bill_2016$baseline_bill <- bill_2016$baseline_variable_bill + 0
  #adding baseline usage
  bill_2016$baseline_usage <- bill_2016$B1 + bill_2016$B2 + bill_2016$B3 + bill_2016$B4
  
  return( bind_rows(bill_2015, bill_2016) )
}


irwd_baseline <- function(){
  #FY2014  
  tmp <- filter(df, usage_date < as.Date("2014-07-01"))
  tier_starts <- get_budget_tiers(tmp, parse_strings("0\n41%\n101%\n151%\n201%"), 
                                  get_indoor_tier(tmp, 50), get_outdoor_tier(tmp, 0.8))
  bill_2014 <- calculate_variable_bill(data=tmp, 
                                       rate_type="Budget", 
                                       tier_starts=tier_starts,
                                       tier_prices=parse_numerics("0.91\n1.27\n2.86\n4.80\n9.84") )
  num_tiers <- length(parse_strings("0\n41%\n101%\n151%\n201%"))
  colnames(bill_2014) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_2014 <- bill_2014 %>% mutate(baseline_bill=baseline_variable_bill + 9.85)
  #adding baseline usage
  bill_2014$baseline_usage <- bill_2014$B1 + bill_2014$B2 + bill_2014$B3 + bill_2014$B4 + bill_2014$B5
  
  #FY2015
  tmp <- filter(df, usage_date < as.Date("2015-07-01"), usage_date >= as.Date("2014-07-01"))
  tier_starts <- get_budget_tiers(tmp, parse_strings("0\n41%\n101%\n131%\n161%"), 
                                  get_indoor_tier(tmp, 50), get_outdoor_tier(tmp, 0.8))
  bill_2015 <- calculate_variable_bill(data=tmp, 
                                       rate_type="Budget", 
                                       tier_starts=tier_starts,
                                       tier_prices=parse_numerics("0.88\n1.34\n3.91\n6.22\n12.60") )
  num_tiers <- length(parse_strings("0\n41%\n101%\n151%\n201%"))
  colnames(bill_2015) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_2015 <- bill_2015 %>% mutate(baseline_bill=baseline_variable_bill + 10.50)
  #adding baseline usage
  bill_2015$baseline_usage <- bill_2015$B1 + bill_2015$B2 + bill_2015$B3 + bill_2015$B4 + bill_2015$B5
  #FY2016
  tmp <- filter(df, usage_date < as.Date("2016-07-01"), usage_date >= as.Date("2015-07-01"))
  tier_starts <- get_budget_tiers(tmp, parse_strings("0\n41%\n101%\n131%"), 
                                  get_indoor_tier(tmp, 50), get_outdoor_tier(tmp, 0.8))
  bill_2016 <- calculate_variable_bill(data=tmp, 
                                       rate_type="Budget", 
                                       tier_starts=tier_starts,
                                       tier_prices=parse_numerics("1.11\n1.62\n3.92\n14.53") )
  num_tiers <- length(parse_strings("0\n41%\n101%\n131%"))
  colnames(bill_2016) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_2016 <- bill_2016 %>% mutate(baseline_bill=baseline_variable_bill + 10.30)
  #adding baseline usage
  bill_2016$baseline_usage <- bill_2016$B1 + bill_2016$B2 + bill_2016$B3 + bill_2016$B4
  return( bind_rows(bill_2014, bill_2015, bill_2016) )
}
