

source("helper_fns.R", local=TRUE)
source("make_plots.R", local=TRUE)

df <- read_data("data/mnwd_test.csv", cust_col="cust_loc_id", usage_col="usage_ccf", month_col="usage_month", 
                year_col="usage_year", et_col="usage_et_amount", hhsize_col="cust_loc_hhsize", 
                irr_area_col="cust_loc_irr_area_sf", rate_code_col= "cust_loc_class")


shinyServer(function(input, output, clientData, session) {
  
  min_date <- min(df$usage_date)
  max_date <- max(df$usage_date)
  print(min_date, max_date)
  updateSliderInput(session, "timeSlider", label = "Time Range", min = min_date, 
                    max = max_date, value = c(min_date, max_date))
#   sliderMonth <- reactiveValues()
#   observe({
#     full.date <- as.POSIXct(input$timeSlider, tz="GMT")
#     sliderMonth$Month <- as.character(monthStart(full.date))
#   })
#   
  
  indoor <- reactive({
    get_indoor_tier(df, input$galPerCapitaSlider)
  })
  
  outdoor <- reactive({
    get_outdoor_tier(df, input$plantFactorSlider)
  })
  
  #******************************************************************
  # 
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
                  input$galPerCapitaSlider, input$plantFactorSlider))
      bill_info <- calculate_variable_bill(data=df, rate_type=input$rateType, 
                                   tier_start_str=input$budgetTiers,
                                   tier_price_str=input$budgetPrice,
                                   indoor_tier=indoor(), 
                                   outdoor_tier=outdoor())
    }
    print( paste("Variable Revenue:",sum(bill_info$variable_bill, na.rm=TRUE)) )
    bill_info
  })
  
  total_bill_info <- reactive({
    bill_info <- variable_charge() 
    bill_info$total_bill <- bill_info$variable_bill + input$fixedCharge
    return(bill_info)
  })
  
  df_plots <- reactive({
    combined <- dplyr::bind_cols(df, total_bill_info(), baseline_bill_info()) %>%
           filter(rate_code %in% c("RESIDENTIAL_SINGLE", "RESIDENTIAL_MULTI"),
                  usage_date >= input$timeSlider[1],
                  usage_date <= input$timeSlider[2])
    combined
  })
  
  baseline_bill_info <- reactive({
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
  })

  output$revenue_time_series <- renderPlotly({
    # print(glimpse(df_plots()[1,]))
    p <- plot_revenue_over_time( df_plots() )
    # ggplotly(p)
    p
  }) 
  
  output$barchart_by_tiers <- renderPlotly({
    plot_barchart_by_tiers( df_plots(), input$displayType, input$barType )
  })
  
  
  # Changes to bills
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
  
  output$bill_change_histogram <- renderPlotly({
    plot_bill_change_histogram( df_change() )
  })
  
  output$bill_change_boxplot <- renderPlotly({
    plot_bill_change_boxplot( df_change() )
  })

})
