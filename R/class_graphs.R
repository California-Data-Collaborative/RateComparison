



classGraphOutput <- function(id, rate_codes){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(4, 
         wellPanel(
           tabsetPanel(type = 'tabs', 
                       tabPanel("Current Rate Comparision",
                                
              fluidRow(
                column(5, 
                       radioButtons(ns("rateType"), label = "Rate Type",
                                    choices = list("Flat" = "Flat", "Tiered" = "Tiered", "Budget" = "Budget"), 
                                    selected = "Flat")
                ),
                column(7, 
                       radioButtons(ns("displayType"), label = "Display", selected = "Revenue", inline=TRUE,
                                    choices = list("Revenue" = "Revenue", "Usage" = "Usage"))
                )
              ),#end row
              
              fluidRow(
                column(1),
                column(10,
                       sliderInput(ns("timeSlider"), label = "Time Range", min = min_date, 
                                   max = max_date, value = c(min_date, max_date), timeFormat="%Y-%m")
                ),
                column(1)
              ),
              
              fluidRow(
                column(12,
                       ratePartInput(ns("service_charge"))
                )
              ),
              
              # FLAT RATES
              conditionalPanel(
                condition = sprintf("input['%s'] == 'Flat'", ns("rateType")),
                fluidRow(
                  column(12,
                         ratePartInput(ns("flatRate"))
                         # numericInput("flatRate", label = "Price per CCF ($)", value = 2.7)
                  )
                )#end row
              ),#end conditionalPanel
              
              # TIERED RATES
              conditionalPanel(
                condition = sprintf("input['%s'] == 'Tiered'", ns("rateType")),
                fluidRow(
                  column(6,
                         fluidRow(
                           strong("Tier start (CCF)")
                         ),
                         fluidRow(
                           HTML(default_tiered_tiers_html)
                         )
                  ),
                  column(6,
                         fluidRow(
                           strong("Tier prices")
                         ),
                         fluidRow(
                           HTML(default_tiered_prices_html)
                         )
                  )
                )#end row
              ),#end conditionalPanel
              
              # BUDGET BASED
              conditionalPanel(
                condition = sprintf("input['%s'] == 'Budget'", ns("rateType")),
                fluidRow(
                  column(6, 
                         sliderInput("galPerCapitaSlider", label = "GPCD", min = 25, 
                                     max = 75, value = default_gpcd, step=5)
                  ),
                  column(6, 
                         sliderInput("ETFactorSlider", label = "ET Factor", min = 0, 
                                     max = 1, value = default_et_factor, step=0.05)
                  )
                ),#end row
                fluidRow(
                  column(6,
                         fluidRow(
                           strong("Tier start")
                         ),
                         fluidRow(
                           HTML(default_budget_tiers_html)
                         )
                  ),
                  column(6,
                         fluidRow(
                           strong("Tier prices ($)")
                         ),
                         fluidRow(
                           HTML(default_budget_prices_html)
                         )
                  )
                ),#end row
                fluidRow(paste("Enter the starting value for each tier either as a CCF value, ",
                               " or as a percent of budget (water budget assumed as Indoor + Outdoor). ",
                               "Where:")
                ),
                fluidRow(br(),
                         em("Indoor = GPCD * HHSize * (365/12/748)"),
                         br(),
                         em("Outdoor = ET_Factor * ET * LA  * (0.62/748)")
                )
              )
                    
                    
           )
           
                       
           )#end tabsetPanel
           
           #                conditionalPanel(
           #                  condition = "input.rateType == 'Tiered' || input.rateType == 'Budget'",
           #                  fluidRow(
           #                    column(12,
           #                           actionButton("updateTiers", "Update Tiers")
           #                    )
           #                  )#end row
           #                )
           
         )#end wellPanel
      ),# end column
      column(8,
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
      ) # end column
    )# end row
  )#end tagList
}


classGraph <- function(input, output, session, df_original, df_total_bill, df_baseline_bill, timeslider){
  
  ratePartInputs <- callModule(ratePart, "service_charge", part_name="service_charge", 
                               part_name_long="Service Charge",depends_col_list=dropdown_cols)
  
  ratePartInput2 <- callModule(ratePart, "flatRate", part_name="flatRate", 
                               part_name_long="Price per CCF",depends_col_list=dropdown_cols)
  
  
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
    p <- plot_revenue_over_time( df_plots(), input$displayType )
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
  
  # Plot histogram
  output$bill_change_histogram <- renderPlotly({
    plot_bill_change_histogram( df_change(), input$displayType )
  })
  
  # Plot boxplot
  output$bill_change_boxplot <- renderPlotly({
    plot_bill_change_boxplot( df_change(), input$displayType )
  })
  
  output$fixed_revenue_barchart <- renderPlotly({
    plot_fixed_revenue(df_plots(), input$barType)
  })
  
}