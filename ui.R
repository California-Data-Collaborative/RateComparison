
# automatic reading rate codes, renaming the tabs, consistent naming convention
shinyUI(navbarPage(
  #color #2c3e50
  HTML(paste('<img src="CaDC_logo_bluegrey.png" ',
             'href="http://californiadatacollaborative.com/" ', 'height="60" ',
             'style="position: relative; top:-20px; right:-15px">')),
  theme = "bootstrap.css",
  
  tabPanel(
    "Rate Comparison",
    fluidRow(
      #----------------------- Input widget panel -----------------------    
      column(4,
             wellPanel(
              tabsetPanel(type = 'tabs', 
               tabPanel("Current Rate Comparision",
                fluidRow(
                 column(5, 
                        radioButtons("rateType", label = "Rate Type",
                                     choices = list("Flat" = "Flat", "Tiered" = "Tiered", "Budget" = "Budget"), 
                                     selected = "Flat")
                        
                        
                        
                 ),
                 column(7, 
                        numericInput("fixedCharge", label = "Fixed Charge ($)", value = default_fixed_charge),
                        radioButtons("displayType", label = "Display", selected = "Revenue", inline=TRUE,
                                     choices = list("Revenue" = "Revenue", "Usage" = "Usage"))
                        
                        
                 )
               ),#end row
               
               fluidRow(
                 column(1),
                 column(10,
                        sliderInput("timeSlider", label = "Time Range", min = min_date, 
                                    max = max_date, value = c(min_date, max_date), timeFormat="%Y-%m")
                 ),
                 column(1)
               ),
               
               
               # FLATE RATES
               conditionalPanel(
                 condition = "input.rateType == 'Flat'",
                 fluidRow(
                   column(12,
                          numericInput("flatRate", label = "Price per CCF ($)", value = 2.7)
                   )
                 )#end row
               ),#end conditionalPanel
               
               # TIERED RATES
               conditionalPanel(
                 condition = "input.rateType == 'Tiered'",
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
                 condition = "input.rateType == 'Budget'",
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
                
                         
                ),
               tabPanel("Scenario Planning",
                  
                  checkboxInput("Planning", "Enable Scenario Planning", value = FALSE, 
                                width = NULL),
                  
                  numericInput("Months", "Number of months to Forecast", 
                               3, min = 1, max = 24, step = 1,
                               width = NULL),
                  numericInput("Growth", "Expected Monthly Growth of number of accounts in the forecast period",
                               5, min = 0, max = 100, step = 1,
                               width = NULL),
                  numericInput("EstUsagePerAccount", "Estimated Usage per New Account(ccf)",
                               10, min = 0, max = 1000, step = NA,
                               width = NULL)
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
      ),#end column
      
      #----------------------- Output panels ------------------
      column( 8, #"main panel",
              tabsetPanel(type="tabs",
                          tabPanel("RESIDENTIAL_SINGLE",
                                   classGraphOutput("graphs_RESIDENTIAL_SINGLE", rate_codes)
                          ),
                          tabPanel("RESIDENTIAL_MULTI",
                                   classGraphOutput("graphs_RESIDENTIAL_MULTI", rate_codes1)
                          ),
                          tabPanel("COMMERCIAL",
                                   classGraphOutput("graphs_COMMERCIAL", rate_codes2)
                          ) ,
                          tabPanel("IRRIGATION",
                                   classGraphOutput("graphs_IRRIGATION", rate_codes3)
                          ), 
                          tabPanel("INSTITUTIONAL",
                                   classGraphOutput("graphs_INSTITUTIONAL", rate_codes4)
                          ),
                          tabPanel("OTHER",
                                   classGraphOutput("graphs_OTHER", rate_codes5)
                          )
                          
              )#end tabsetpanel
      ) #end column
    )#end row
  )#end tabpanel for navar
))
