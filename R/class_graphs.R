



classGraphOutput <- function(id, rate_codes){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(4, 
         wellPanel(
#            tabsetPanel(type = 'tabs', 
#                        tabPanel("Current Rate Comparision",
                                
              fluidRow(
                column(8, 
                       radioButtons(ns("rateType"), label = "Rate Type", inline=TRUE,
                                    choices = list("Flat" = "Flat", "Tiered" = "Tiered", "Budget" = "Budget"), 
                                    selected = "Flat")
                ),
                column(4, 
                       radioButtons(ns("displayType"), label = "Display", selected = "Revenue", inline=FALSE,
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
                         ratePartInput(ns("flat_rate"))
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
                           tierBoxInput(ns("tiered_prices"))
                         )
                  )
                )#end row
              ),#end conditionalPanel
              
              # BUDGET BASED
              conditionalPanel(
                condition = sprintf("input['%s'] == 'Budget'", ns("rateType")),
                fluidRow(
                  column(12,
                         ratePartInput(ns("gpcd"))
                  )
                ),
                
                fluidRow(
                  column(12,
                         ratePartInput(ns("landscape_factor"))
                  )
                ),
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
                           tierBoxInput(ns("budget_prices"))
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
                    
#                     
#            )
#            
#            )#end tabsetPanel
           
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


classGraph <- function(input, output, session, cust_class, df_original, df_total_bill, 
                       df_baseline_bill, active_class, rate_list){
  ns <- session$ns
  
  input_list <- list()
  input_list$other_inputs <- input
  
  rate_part_name <- "service_charge"
  input_list[[rate_part_name]] <- callModule(ratePart, rate_part_name, 
             part_name=rate_part_name, part_name_long="Service Charge",
             depends_col_list=dropdown_cols,
             rate_part = rate_list()$rate_structure[[active_class()]][[rate_part_name]])
  # browser()
  rate_part_name2 <- "flat_rate"
  input_list[[rate_part_name2]] <- callModule(ratePart, rate_part_name2, 
             part_name=rate_part_name2, part_name_long="Charge per CCF",
             depends_col_list=dropdown_cols, 
             value_map = rate_list()$rate_structure[[active_class()]][[rate_part_name2]]$values)   
  
  rate_part_name3 <- "gpcd"
  input_list[[rate_part_name3]] <- callModule(ratePart, rate_part_name3, 
            part_name=rate_part_name3, part_name_long="GPCD",
            depends_col_list=dropdown_cols, 
            rate_part = rate_list()$rate_structure[[active_class()]][[rate_part_name3]]
            )   

  rate_part_name4 <- "landscape_factor"
  input_list[[rate_part_name4]] <- callModule(ratePart, rate_part_name4, 
            part_name=rate_part_name4, part_name_long="Landscape Factor",
            depends_col_list=dropdown_cols, 
            rate_part = rate_list()$rate_structure[[active_class()]][[rate_part_name4]]
  )  
  
  rate_part_name5 <- "tiered_prices"
  input_list[[rate_part_name5]] <- callModule(tierBox, rate_part_name5, 
            part_name=rate_part_name5, part_name_long="Tier Prices ($)", 
            rate_type=rate_list()$rate_structure[[active_class()]][["commodity_charge"]], 
            rate_type_provided="Tiered", 
            rate_part=rate_list()$rate_structure[[active_class()]][["tier_prices"]]
  )
  
  rate_part_name6 <- "budget_prices"
  input_list[[rate_part_name6]] <- callModule(tierBox, rate_part_name6, 
            part_name=rate_part_name6, part_name_long="Tier Prices ($)", 
            rate_type=rate_list()$rate_structure[[active_class()]][["commodity_charge"]], 
            rate_type_provided="Budget", 
            rate_part=rate_list()$rate_structure[[active_class()]][["tier_prices"]]
  )
  
  #TODO: add other tier boxes, and link the boxes up with the hypothetical_rate_list
  
  df_plots <- reactive({
    
    combined <- dplyr::bind_cols(df_original(), df_total_bill(), df_baseline_bill()) %>%
      # filter(usage_date >= input$timeSlider[1],
         # usage_date <= input$timeSlider[2])
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
  
  return(input_list)
}




get_rate_part_value <- function(class_rate, rate_part_name){
  for(i in 1:length(class_rate)){
    rate_part <- class_rate[[i]]
    name <- names(rate_part)
    
    if(name==rate_part_name){
      
      if(is_map(rate_part[[name]])){
        return(rate_part[[name]]$value)
      }else{
        return(rate_part[[name]])
      }
    }
  }
  return(0)
}

get_rate_part_depends_col <- function(class_rate, rate_part_name){
  for(i in 1:length(class_rate)){
    rate_part <- class_rate[[i]]
    name <- names(rate_part)
    
    if(name==rate_part_name){
      if(is_map(rate_part[[name]])){
        return(rate_part[[name]]$depends_on)
      }
    }
  }
  return(NULL)
}

# extract_depends_values()

# rate_part_is_map <- function(class_rate, rate_part_name){
#   for(i in 1:length(class_rate)){
#     rate_part <- class_rate[[i]]
#     name <- names(rate_part)
#     
#     if(name==rate_part_name){
#       if(is_map(rate_part[[name]])){
#         return(TRUE)
#       }
#     }
#   }
#   return(FALSE)
# }
