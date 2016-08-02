

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
               fluidRow(
                 column(5, 
                        radioButtons("rateType", label = "Rate Type",
                                     choices = list("Flat" = "Flat", "Tiered" = "Tiered", "Budget" = "Budget"), 
                                     selected = "Flat")
                 ),
                 column(7, 
                        numericInput("fixedCharge", label = "Fixed Charge ($)", value = 18.30),
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
                            HTML('<textarea id="tieredTiers" rows="6" cols="15" style="resize: none;">0\n16\n67\n200</textarea>')
                          )
                   ),
                   column(6,
                          fluidRow(
                            strong("Tier prices")
                          ),
                          fluidRow(
                            HTML('<textarea id="tieredPrice" rows="6" cols="15" style="resize: none;">2.31\n2.80\n3.81\n5.34</textarea>')
                          )
                   )
                 )#end row
               ),#end conditionalPanel
               
               # BUDGET BASED
               conditionalPanel(
                 condition = "input.rateType == 'Budget'",
                 fluidRow(
                   column(6, 
                          sliderInput("galPerCapitaSlider", label = "GPCD", min = 0, 
                                      max = 75, value = 60, step=5)
                   ),
                   column(6, 
                          sliderInput("ETFactorSlider", label = "ET Factor", min = 0, 
                                      max = 1, value = 0.7, step=0.05)
                   )
                 ),#end row
                 fluidRow(
                   column(6,
                          fluidRow(
                            strong("Tier start")
                          ),
                          fluidRow(
                            # HTML('<textarea id="budgetTiers" rows="6" cols="15" style="resize: none;">0\nIndoor\n101%\n126%\n151%</textarea>')
                            HTML('<textarea id="budgetTiers" rows="6" cols="15" style="resize: none;">0\nIndoor\n101%\n151%</textarea>')
                          )
                   ),
                   column(6,
                          fluidRow(
                            strong("Tier prices ($)")
                          ),
                          fluidRow(
                            # HTML('<textarea id="budgetPrice" rows="6" cols="15" style="resize: none;">1.49\n1.70\n2.62\n4.38\n9.17</textarea>')
                            HTML('<textarea id="budgetPrice" rows="6" cols="15" style="resize: none;">2.36\n3.18\n3.96\n4.98</textarea>')
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
                          em("Outdoor = ET_Factor * ET * LA  * (1/748)")
                 )
                 
               )#end conditionalPanel
               
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
                          tabPanel("Single-Family",
                                   fluidRow(
                                     column(12, plotlyOutput("revenue_time_series", height=250) )
                                   ),
                                   fluidRow(
                                     column(4,
                                            plotlyOutput("barchart_by_tiers", height=350),
                                            radioButtons("barType", label = "",  selected = "Absolute", inline=TRUE,
                                                         choices = list("Absolute" = "Absolute", "Percent" = "Percent"))
                                     ),
                                     column(3,
                                            plotlyOutput("fixed_revenue_barchart", height=350)
                                     ),
                                     column(5, 
                                            plotlyOutput("bill_change_boxplot", height=100),
                                            plotlyOutput("bill_change_histogram", height=250) )
                                     
                                     )
                                   
                          ),
                          
                          tabPanel("OtherTab",
                                   fluidRow(
                                   ),
                                   fluidRow(
                                     br()
                                   )
                          )
              )#end tabsetpanel
      ) #end column
    )#end row
  )#end tabpanel for navar
))
