

source("helper_fns.R", local=TRUE)

df <- read_data("data/mnwd_test.csv", usage_col="usage_ccf", month_col="usage_month", 
                year_col="usage_year", et_col="usage_et_amount", hhsize_col="cust_loc_hhsize", 
                irr_area_col="cust_loc_irr_area_sf", rate_code_col= "cust_loc_class_from_utility")


shinyServer(function(input, output, clientData, session) {
  
  variable_charge <- reactive({ 
    
    if(input$rateType == "Flat"){
      v <- calculate_variable_bill(data=df, rate_type=input$rateType, 
                                   tier_price_str=as.character(input$flatRate))
    }
    else if(input$rateType == "Tiered"){
      v <- calculate_variable_bill(data=df, rate_type=input$rateType, 
                                   tier_start_str=input$tieredTiers,
                                   tier_price_str=input$tieredPrice)
    }
    else if(input$rateType == "Budget"){
      v <- calculate_variable_bill(data=df, rate_type=input$rateType, 
                                   tier_start_str=input$tieredTiers,
                                   tier_price_str=input$tieredPrice,
                                   gpcd=input$galPerCapitaSlider, 
                                   plant_factor=input$plantFactorSlider)
    }
    
    return(v)
  })
  
  total_bill <- reactive({
    return(variable_charge + input$fixedCharge)
  })

  output$revenue_time_series <- renderPlotly({
    p <- plot_revenue_over_time(df, total_bill)
    return(ggplotly(p))
  }) 

})
