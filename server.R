options(shiny.error = NULL)
# Load functions
source("R/helper_fns.R", local=TRUE)
source("R/make_plots.R", local=TRUE)


shinyServer(function(input, output, clientData, session) {
  
 planneddf <- reactive({
   
  if(input$Planning == TRUE){
    set.seed(10000)
    getmode <- function(v) {
      #fn to get mode of a vector
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    
    #for mean usage_ccf
    monthlyusagebyaccount <- df %>% 
                             group_by(cust_id,usage_month) %>% 
                             summarise(usage_ccf = mean(usage_ccf,na.rm=TRUE))
    
 
    month_Vec <- 1:input$Months
    
    increment_Vec <- (1:input$Months)*input$Growth
    
    recent_date <- max(df$usage_date)
    
    recent_month_data <- df %>% filter(usage_date == recent_date)
    
    recent_date_Vec <- c(recent_date %m+% months(1:input$Months))
    
    #for rate code filling
    ratecode_filler <- data.table(r2)
    ratecode_filler <- ratecode_filler[,head(.SD, 1), by=cust_class]
    
    #for generating customer class
    class_proportions <- as.data.frame(prop.table(table(df$cust_class)), stringsAsFactors = FALSE)
 
    planneddflist <- list()
    
     
      if(is_budget){
        if("et_amount" %in% colnames(df)){
        #average et by month
        avg_et_df <-  df%>%  group_by(usage_month) %>% summarise(et_amount = mean(et_amount,na.rm=TRUE))
        }
        
        if("hhsize" %in% colnames(df)){
        #average hhsize by customer class
        mean_hhsize <- df %>% 
                       group_by(cust_class) %>% 
                       summarise(hhsize = round(mean(hhsize,na.rm=TRUE)))
        }
        
        if("irr_area" %in% colnames(df)){
        #average irr_area by customer class
        #irrarea <- mean(df$irr_area[!df$irr_area %in% boxplot.stats(df$irr_area)$out]) #removing outliers
        mean_irr_area <- df %>% 
                         group_by(cust_class) %>% 
                         summarise(irr_area = round(mean(irr_area,na.rm=TRUE)))
        }
        
        
         for(i in month_Vec){
          
          new_recent_month_data <- recent_month_data
          
          new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]), "cust_id"] <- 1:increment_Vec[i]
          
          new_recent_month_data[, "usage_date"] <- rep(recent_date_Vec[i], nrow(recent_month_data)+increment_Vec[i])
          
          new_recent_month_data[, "usage_month"] <- rep(month(recent_date_Vec[i]), nrow(recent_month_data)+increment_Vec[i])
          
          new_recent_month_data[, "usage_year"] <- rep(year(recent_date_Vec[i]), nrow(recent_month_data)+increment_Vec[i])
        
          new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]), "cust_class"] <- sample(class_proportions$Var1, replace = TRUE, 
                                                                                                                                prob = class_proportions$Freq,
                                                                                                                                size = increment_Vec[i])
          
          #for filling hhsize to new accounts
          tmp <- new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]),]
          
          tmp <- left_join(tmp, mean_hhsize, by = c("cust_class")) %>% arrange(cust_id)
          
          new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]), "hhsize"] <- tmp$hhsize.y
          
          
          
          #for filling irr_area to new accounts
          tmp <- new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]),]
          
          tmp <- left_join(tmp, mean_irr_area, by = c("cust_class")) %>% arrange(cust_id)
          
          new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]), "irr_area"] <- tmp$irr_area.y
          
         
          #fill in average et by month
          tmp <- left_join(new_recent_month_data, avg_et_df, by = 'usage_month')
          
          new_recent_month_data$et_amount <- tmp$et_amount.y
          
          #fill in average usage by account and month
          tmp <- left_join(new_recent_month_data, monthlyusagebyaccount, by = c('cust_id','usage_month'))
          
          new_recent_month_data$usage_ccf[1:nrow(recent_month_data)] <- tmp$usage_ccf.y
          
          #fill in the usage for new accounts with the estimated usage input
          new_recent_month_data[(nrow(new_recent_month_data)-increment_Vec[i]+1):nrow(new_recent_month_data), "usage_ccf"] <- input$EstUsagePerAccount
          
         
          #fill in meter size for new accounts
          if("cust_loc_meter_size" %in% colnames(df)){
          new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]),"cust_loc_meter_size"] <- rep(getmode(df$cust_loc_meter_size),
                                                                                                       length.out = increment_Vec[i])
          }
          
          #fill in water type for new accounts
          if("cust_loc_water_type" %in% colnames(df)){
          new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]),"cust_loc_water_type"] <- rep(getmode(df$cust_loc_water_type),
                                                                                                       length.out = increment_Vec[i])
          }
          
          #fill in rate code for new accounts
          tmp <- new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]),]
          
          tmp <- left_join(tmp, ratecode_filler, by = c("cust_class"))%>% arrange(cust_id)
          
          new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]), "rate_code"] <- tmp$rate_code.y
          
         
          planneddflist[[i]] <- new_recent_month_data
          
        }
        
        

       }else{
         
         for(i in month_Vec){
           
           new_recent_month_data <- recent_month_data
           
           new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]), "cust_id"] <- 1:increment_Vec[i]
          
           new_recent_month_data[, "usage_date"] <- rep(recent_date_Vec[i], nrow(recent_month_data)+increment_Vec[i])
           
           new_recent_month_data[, "usage_month"] <- rep(month(recent_date_Vec[i]), nrow(recent_month_data)+increment_Vec[i])
           
           new_recent_month_data[, "usage_year"] <- rep(year(recent_date_Vec[i]), nrow(recent_month_data)+increment_Vec[i])
           
           new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]), "cust_class"] <- sample(class_proportions$Var1, replace = TRUE, 
                                                                                                                                 prob = class_proportions$Freq,
                                                                                                                                 size = increment_Vec[i])
      
           
           #fill in average usage by account and month
           tmp <- left_join(new_recent_month_data, monthlyusagebyaccount, by = c('cust_id','usage_month'))
           
           new_recent_month_data$usage_ccf[1:nrow(recent_month_data)] <- tmp$usage_ccf.y
           
           #fill in the usage for new accounts with the estimated usage input
           new_recent_month_data[(nrow(recent_month_data)-increment_Vec[i]+1):nrow(recent_month_data), "usage_ccf"] <- input$EstUsagePerAccount
           
           if("cust_loc_meter_size" %in% colnames(df)){
           #fill in meter size for new accounts
           new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]),"cust_loc_meter_size"] <- rep(getmode(df$cust_loc_meter_size),
                                                                                                                                      length.out = increment_Vec[i])
           }
           if("cust_loc_water_type" %in% colnames(df)){
           #fill in water type for new accounts
           new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]),"cust_loc_water_type"] <- rep(getmode(df$cust_loc_water_type),
                                                                                                                                      length.out = increment_Vec[i])
           
           }
           #fill in rate code for new accounts
           tmp <- new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]),]
           
           tmp <- left_join(tmp, ratecode_filler, by = c("cust_class"))%>% arrange(cust_id)
           
           new_recent_month_data[(nrow(recent_month_data)+1):(nrow(recent_month_data)+increment_Vec[i]), "rate_code"] <- tmp$rate_code.y
           
           
           planneddflist[[i]] <- new_recent_month_data
           
         }
         
         
       }
  
  planneddf = do.call(rbind, planneddflist)
  planneddf <- rbind(df, planneddf)

  }

})  
 

  
 observe({

  updateSliderInput(session, "timeSlider", label = "Time Range", min = min(planneddf()$usage_date), 
  max = max(planneddf()$usage_date), value = c(min(planneddf()$usage_date), max(planneddf()$usage_date)))
  
 })
 
  # Get the indoor tier cutoffs
  indoor <- reactive({
    if(input$Planning == TRUE){
    get_indoor_tier(planneddf(), input$galPerCapitaSlider)
    }
    else{
    get_indoor_tier(df, input$galPerCapitaSlider)
    }
  })
  # Get the outdoor tier cutoffs
  outdoor <- reactive({
    if(input$Planning == TRUE){
      get_indoor_tier(planneddf(), input$galPerCapitaSlider)
    }
    else{
    get_outdoor_tier(df, input$ETFactorSlider)
    }
  })
  
  #******************************************************************
  # Calculate variable potion of the bill, dependent on rate type
  #******************************************************************
  variable_charge <- reactive({
    if(input$Planning == TRUE){
    bill_info <- calculate_variable_bill(data=planneddf(), rate_type=input$rateType, 
                                         tier_starts=tier_info()$starts,
                                         tier_prices=tier_info()$prices )
    }
    else{
      bill_info <- calculate_variable_bill(data=df, rate_type=input$rateType, 
                                           tier_starts=tier_info()$starts,
                                           tier_prices=tier_info()$prices )
    }
    
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
    if(input$Planning == TRUE){
      get_budget_tiers(planneddf(), parse_strings(input$budgetTiers), indoor(), outdoor())
    }
    else{
      get_budget_tiers(df, parse_strings(input$budgetTiers), indoor(), outdoor())
    }
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
    if(input$Planning == TRUE){
      combined <- dplyr::bind_cols(planneddf(), total_bill_info(), baseline_bill_info())%>%
        filter(usage_date >= input$timeSlider[1],
              usage_date <= input$timeSlider[2],
              rate_code %in% input$RateCode)
         
    }
    else{
      combined <- dplyr::bind_cols(df, total_bill_info(), baseline_bill_info()) %>%
        filter(usage_date >= input$timeSlider[1],
                usage_date <= input$timeSlider[2],
                rate_code %in% input$RateCode)
    }
    
    combined
  })
  
  df_plots1 <- reactive({
    if(input$Planning == TRUE){
      combined <- dplyr::bind_cols(planneddf(), total_bill_info(), baseline_bill_info()) %>%
        filter(usage_date >= input$timeSlider[1],
               usage_date <= input$timeSlider[2],
               rate_code %in% input$RateCode1)
      
    }
    else{
      combined <- dplyr::bind_cols(df, total_bill_info(), baseline_bill_info()) %>%
        filter(usage_date >= input$timeSlider[1],
               usage_date <= input$timeSlider[2],
               rate_code %in% input$RateCode1)
    }
    combined
  })
  
  df_plots2 <- reactive({
    if(input$Planning == TRUE){
      combined <- dplyr::bind_cols(planneddf(), total_bill_info(), baseline_bill_info()) %>%
        filter(usage_date >= input$timeSlider[1],
               usage_date <= input$timeSlider[2],
               rate_code %in% input$RateCode2)
      
    }
    else{
      combined <- dplyr::bind_cols(df, total_bill_info(), baseline_bill_info()) %>%
        filter(usage_date >= input$timeSlider[1],
               usage_date <= input$timeSlider[2],
               rate_code %in% input$RateCode2)
    }
    combined
  })
  
  df_plots3 <- reactive({
    if(input$Planning == TRUE){
      combined <- dplyr::bind_cols(planneddf(), total_bill_info(), baseline_bill_info()) %>%
        filter(usage_date >= input$timeSlider[1],
               usage_date <= input$timeSlider[2],
               rate_code %in% input$RateCode3)
      
    }
    else{
      combined <- dplyr::bind_cols(df, total_bill_info(), baseline_bill_info()) %>%
        filter(usage_date >= input$timeSlider[1],
               usage_date <= input$timeSlider[2],
               rate_code %in% input$RateCode3)
    }
    combined
  })
  
  df_plots4 <- reactive({
    if(input$Planning == TRUE){
      combined <- dplyr::bind_cols(planneddf(), total_bill_info(), baseline_bill_info()) %>%
        filter(usage_date >= input$timeSlider[1],
               usage_date <= input$timeSlider[2],
               rate_code %in% input$RateCode4)
      
    }
    else{
      combined <- dplyr::bind_cols(df, total_bill_info(), baseline_bill_info()) %>%
        filter(usage_date >= input$timeSlider[1],
               usage_date <= input$timeSlider[2],
               rate_code %in% input$RateCode4)
    }
    combined
  })
  
  df_plots5 <- reactive({
    if(input$Planning == TRUE){
      combined <- dplyr::bind_cols(planneddf(), total_bill_info(), baseline_bill_info()) %>%
         filter(usage_date >= input$timeSlider[1],
                usage_date <= input$timeSlider[2],
                rate_code %in% input$RateCode5)
      
    }
    else{
      combined <- dplyr::bind_cols(df, total_bill_info(), baseline_bill_info()) %>%
        filter(usage_date >= input$timeSlider[1],
               usage_date <= input$timeSlider[2],
               rate_code %in% input$RateCode5)
    }
    combined
  })
  
  
  
  #******************************************************************
  # Calculate bills and tiers for the MNWD residential baseline rate
  #******************************************************************
  baseline_bill_info <- reactive({
    if(input$Planning == TRUE){
    switch(utility_code,
           "IRWD"=irwd_baseline(basedata=planneddf()),
           "MNWD"=mnwd_baseline(basedata=planneddf()),
           "LVMWD"=lvmwd_baseline(basedata=planneddf()),
           "SMWD"=smwd_baseline(basedata=planneddf()),
           "SMC"=smc_baseline(basedata=planneddf())
      )
    }
    else{
      switch(utility_code,
             "IRWD"=irwd_baseline(basedata=df),
             "MNWD"=mnwd_baseline(basedata=df),
             "LVMWD"=lvmwd_baseline(basedata=df),
             "SMWD"=smwd_baseline(basedata=df),
             "SMC"=smc_baseline(basedata=df)
      )
    }
  })
  
  #******************************************************************
  # Line plot of revenue over time
  #******************************************************************
  
  output$revenue_time_series <- renderPlotly({
    # print(glimpse(df_plots()[1,]))
    if(input$Planning == TRUE){
      p <- plot_revenue_over_time( df_plots(), input$displayType ) + 
           geom_vline(xintercept=as.numeric(max(df$usage_date)),color='red3',linetype=2) +
           geom_text(data=data.table(date=max(df$usage_date),extracol=0),aes(date,extracol),label="forecast",color='red3',angle=45,vjust=-0.5,hjust=-0.5)  
    }else{
      p <- plot_revenue_over_time( df_plots(), input$displayType )  
    }
     ggplotly(p) %>% config(displayModeBar = FALSE)
    
  }) 
  
  output$revenue_time_series1 <- renderPlotly({
    # print(glimpse(df_plots()[1,]))
    if(input$Planning == TRUE){
      p <- plot_revenue_over_time( df_plots(), input$displayType ) + 
        geom_vline(xintercept=as.numeric(max(df$usage_date)),color='red3',linetype=2) +
        geom_text(data=data.table(date=max(df$usage_date),extracol=0),aes(date,extracol),label="forecast",color='red3',angle=45,vjust=-0.5,hjust=-0.5)  
    }else{
      p <- plot_revenue_over_time( df_plots(), input$displayType )  
    }
    ggplotly(p) %>% config(displayModeBar = FALSE)
  }) 
  output$revenue_time_series2 <- renderPlotly({
    # print(glimpse(df_plots()[1,]))
    if(input$Planning == TRUE){
      p <- plot_revenue_over_time( df_plots(), input$displayType ) + 
        geom_vline(xintercept=as.numeric(max(df$usage_date)),color='red3',linetype=2) +
        geom_text(data=data.table(date=max(df$usage_date),extracol=0),aes(date,extracol),label="forecast",color='red3',angle=45,vjust=-0.5,hjust=-0.5)  
    }else{
      p <- plot_revenue_over_time( df_plots(), input$displayType )  
    }
    ggplotly(p) %>% config(displayModeBar = FALSE)
  }) 
  output$revenue_time_series3 <- renderPlotly({
    if(input$Planning == TRUE){
      p <- plot_revenue_over_time( df_plots(), input$displayType ) + 
        geom_vline(xintercept=as.numeric(max(df$usage_date)),color='red3',linetype=2) +
        geom_text(data=data.table(date=max(df$usage_date),extracol=0),aes(date,extracol),label="forecast",color='red3',angle=45,vjust=-0.5,hjust=-0.5)  
    }else{
      p <- plot_revenue_over_time( df_plots(), input$displayType )  
    }
    ggplotly(p) %>% config(displayModeBar = FALSE)
  }) 
  output$revenue_time_series4 <- renderPlotly({
    # print(glimpse(df_plots()[1,]))
    if(input$Planning == TRUE){
      p <- plot_revenue_over_time( df_plots(), input$displayType ) + 
        geom_vline(xintercept=as.numeric(max(df$usage_date)),color='red3',linetype=2) +
        geom_text(data=data.table(date=max(df$usage_date),extracol=0),aes(date,extracol),label="forecast",color='red3',angle=45,vjust=-0.5,hjust=-0.5)  
    }else{
      p <- plot_revenue_over_time( df_plots(), input$displayType )  
    }
    ggplotly(p) %>% config(displayModeBar = FALSE)
  }) 
  output$revenue_time_series5 <- renderPlotly({
    # print(glimpse(df_plots()[1,]))
    if(input$Planning == TRUE){
      p <- plot_revenue_over_time( df_plots(), input$displayType ) + 
        geom_vline(xintercept=as.numeric(max(df$usage_date)),color='red3',linetype=2) +
        geom_text(data=data.table(date=max(df$usage_date),extracol=0),aes(date,extracol),label="forecast",color='red3',angle=45,vjust=-0.5,hjust=-0.5)  
    }else{
      p <- plot_revenue_over_time( df_plots(), input$displayType )  
    }
    ggplotly(p) %>% config(displayModeBar = FALSE)
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
      mutate(changes=total_bill-baseline_bill, changes_in_usage=hypothetical_usage-baseline_usage, change_group=1)
    
      if (input$displayType == "Revenue"){
        
        df_change <- df_change %>% filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE))
      }
      
      else{
        df_change <- df_change %>% filter(abs(changes_in_usage) < mean(changes_in_usage, na.rm=TRUE) + 
                               2.5*sd(changes_in_usage, na.rm=TRUE))
      }
    
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
      mutate(changes=total_bill-baseline_bill, changes_in_usage=hypothetical_usage-baseline_usage, change_group=1)
    
      if (input$displayType == "Revenue"){
        
        df_change1 <- df_change1 %>% filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE))
      }
    
      else{
        df_change1 <- df_change1 %>% filter(abs(changes_in_usage) < mean(changes_in_usage, na.rm=TRUE) + 
                                          2.5*sd(changes_in_usage, na.rm=TRUE))
    }
    
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
      mutate(changes=total_bill-baseline_bill, changes_in_usage=hypothetical_usage-baseline_usage, change_group=1)
    
    if (input$displayType == "Revenue"){
      
      df_change2 <- df_change2 %>% filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE))
    }
    
    else{
      df_change2 <- df_change2 %>% filter(abs(changes_in_usage) < mean(changes_in_usage, na.rm=TRUE) + 
                                            2.5*sd(changes_in_usage, na.rm=TRUE))
    }
    
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
      mutate(changes=total_bill-baseline_bill, changes_in_usage=hypothetical_usage-baseline_usage, change_group=1)
    
    if (input$displayType == "Revenue"){
      
      df_change3 <- df_change3 %>% filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE))
    }
    
    else{
      df_change3 <- df_change3 %>% filter(abs(changes_in_usage) < mean(changes_in_usage, na.rm=TRUE) + 
                                            2.5*sd(changes_in_usage, na.rm=TRUE))
    }
    
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
      mutate(changes=total_bill-baseline_bill, changes_in_usage=hypothetical_usage-baseline_usage, change_group=1)
    
    if (input$displayType == "Revenue"){
      
      df_change4 <- df_change4 %>% filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE))
    }
    
    else{
      df_change4 <- df_change4 %>% filter(abs(changes_in_usage) < mean(changes_in_usage, na.rm=TRUE) + 
                                            2.5*sd(changes_in_usage, na.rm=TRUE))
    }
    
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
      mutate(changes=total_bill-baseline_bill, changes_in_usage=hypothetical_usage-baseline_usage, change_group=1)
    
    if (input$displayType == "Revenue"){
      
      df_change5 <- df_change5 %>% filter(abs(changes) < mean(changes, na.rm=TRUE) + 2.5*sd(changes, na.rm=TRUE))
    }
    
    else{
      df_change5 <- df_change5 %>% filter(abs(changes_in_usage) < mean(changes_in_usage, na.rm=TRUE) + 
                                            2.5*sd(changes_in_usage, na.rm=TRUE))
    }
    
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



mnwd_baseline <- function(basedata){
  
  
    tier_starts <- get_budget_tiers(basedata, parse_strings("0\nIndoor\n101%\n126%\n151%"), 
                                  get_indoor_tier(basedata, 60), get_outdoor_tier(basedata, 0.7))
    bill_info <- calculate_variable_bill(basedata, rate_type="Budget", 
                                       tier_starts=tier_starts,
                                       tier_prices=parse_numerics("1.49\n1.70\n2.62\n4.38\n9.17"))
  
  
  num_tiers <- length(parse_strings("1.49\n1.70\n2.62\n4.38\n9.17"))
  colnames(bill_info) <- c( paste("B", 1:num_tiers, sep=""),
                            paste("BR", 1:num_tiers, sep=""),
                            "baseline_variable_bill")
  bill_info$baseline_bill <- bill_info$baseline_variable_bill + 11.39
  #adding baseline usage
  bill_info$baseline_usage <- bill_info %>% select(matches("[B][0-9]")) %>% rowSums()
  
  return(bill_info)
}

lvmwd_baseline <- function(basedata){
  
  #2014 rate
  tmp <- filter(basedata, usage_year < 2015)
  
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
  bill_2014$baseline_usage <- bill_2014 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  
  
  #2015 before the September switch to monthly billing
  tmp <- filter(basedata, usage_year >= 2015, usage_year < 2016, usage_month < 9)
  
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
  bill_2015_1$baseline_usage <- bill_2015_1 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  #2015 after the September switch to monthly billing
  tmp <- filter(basedata, usage_date >= as.Date("2015-09-01"), usage_year < 2016)
  
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
  bill_2015_2$baseline_usage <- bill_2015_2 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  #2016 budgets
  tmp <- filter(basedata, usage_year >= 2016)
  
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
  bill_2016$baseline_usage <- bill_2016 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  return( bind_rows(bill_2014, bill_2015_1, bill_2015_2, bill_2016) )
}

smwd_baseline <- function(basedata){
  
  
  #before March 2015
  tmp <- filter(basedata, usage_date < as.Date("2015-03-01"))
  
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
  bill_2015_1$baseline_usage <- bill_2015_1 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  
  #after March 2015
  tmp <- filter(planneddf(), usage_date >= as.Date("2015-03-01"), usage_year < 2016)
  
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
  bill_2015_2$baseline_usage <- bill_2015_2 %>% select(matches("[B][0-9]")) %>% rowSums()
  
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


smc_baseline <- function(basedata){
  
  #before 2016
  tmp <- filter(basedata, usage_date < as.Date("2016-01-01"))
  
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
  bill_2015$baseline_usage <- bill_2015 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  #after 2016
  tmp <- filter(basedata, usage_date >= as.Date("2016-01-01"))
  
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
  bill_2016$baseline_usage <- bill_2016 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  return( bind_rows(bill_2015, bill_2016) )
}


irwd_baseline <- function(basedata){
  
  #FY2014  
  tmp <- filter(basedata, usage_date < as.Date("2014-07-01"))
  
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
  bill_2014$baseline_usage <- bill_2014 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  #FY2015
  
  tmp <- filter(basedata, usage_date < as.Date("2015-07-01"), usage_date >= as.Date("2014-07-01"))
  
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
  bill_2015$baseline_usage <- bill_2015 %>% select(matches("[B][0-9]")) %>% rowSums()
  
  #FY2016
  tmp <- filter(basedata, usage_date < as.Date("2016-07-01"), usage_date >= as.Date("2015-07-01"))
  
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
  bill_2016$baseline_usage <- bill_2016 %>% select(matches("[B][0-9]")) %>% rowSums()
  return( bind_rows(bill_2014, bill_2015, bill_2016) )
}


