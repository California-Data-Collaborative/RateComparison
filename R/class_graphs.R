



classGraphOutput <- function(id, rate_codes){
  ns <- NS(id)
  
  tagList(
    checkboxGroupInput(ns("RateCode"), label = "Rate Code", inline=TRUE,
                       choices = rate_codes, selected = rate_codes[1]),
    
    fluidRow(
      column(12, plotlyOutput(ns("revenue_time_series"), height=250) )
    ),
    fluidRow(
      column(4,
             plotlyOutput(ns("barchart_by_tiers"), height=350),
             radioButtons(ns("barType"), label = "",  selected = "Absolute", inline=TRUE,
                          choices = list("Absolute" = "Absolute", "Percent" = "Percent"))
      ),
      column(3,
             plotlyOutput(ns("fixed_revenue_barchart"), height=350)
      ),
      column(5, 
             plotlyOutput(ns("bill_change_boxplot"), height=100),
             plotlyOutput(ns("bill_change_histogram"), height=250) )
      
    ),
    
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }" )
  )
}


classGraph <- function(input, output, session, df_original, df_total_bill, df_baseline_bill, timeslider, displayType){
  
  df_plots <- reactive({
    combined <- dplyr::bind_cols(df_original(), df_total_bill(), df_baseline_bill()) %>%
      # filter(usage_date >= timeSlider()[1],
      #    usage_date <= timeSlider()[2])
      filter(rate_code %in% input$RateCode)

    combined
  })
  
  #******************************************************************
  # Line plot of revenue over time
  #******************************************************************
  output$revenue_time_series <- renderPlotly({
    # print(glimpse(df_plots()[1,]))
    p <- plot_revenue_over_time( df_plots(), displayType() )
    # ggplotly(p)
    p
  })
  
  #******************************************************************
  # Bar chart of revenue/usage by tier
  #******************************************************************
  output$barchart_by_tiers <- renderPlotly({
    plot_barchart_by_tiers( df_plots(), displayType(), input$barType )
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
    
    if (displayType() == "Revenue"){
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
  
  # Plot histogram
  output$bill_change_histogram <- renderPlotly({
    plot_bill_change_histogram( df_change(), displayType() )
  })
  
  # Plot boxplot
  output$bill_change_boxplot <- renderPlotly({
    plot_bill_change_boxplot( df_change(), displayType() )
  })
  
  output$fixed_revenue_barchart <- renderPlotly({
    plot_fixed_revenue(df_plots(), input$barType)
  })
  
}