

shinyUI(fluidPage(
  titlePanel("Rate Comparison"),
  
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
                 numericInput("fixedCharge", label = "Fixed Charge ($)", value = 30.5)
          )
        ),#end row
        
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
                     HTML('<textarea id="tieredTiers" rows="6" cols="15" style="resize: none;">0\n12\n19\n32</textarea>')
                   )
            ),
            column(6,
                   fluidRow(
                     strong("Tier prices")
                   ),
                   fluidRow(
                     HTML('<textarea id="tieredPrice" rows="6" cols="15" style="resize: none;">1.85\n2.33\n4.68\n6.77</textarea>')
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
                               max = 75, value = 55, step=5)
            ),
            column(6, 
                   sliderInput("plantFactorSlider", label = "Plant Factor", min = 0, 
                               max = 1, value = 0.8, step=0.05)
            )
          ),#end row
          fluidRow(
            column(6,
                   fluidRow(
                     strong("Tier start")
                   ),
                   fluidRow(
                     HTML('<textarea id="budgetTiers" rows="6" cols="15" style="resize: none;">0\nIndoor\n100%\n125%</textarea>')
                   )
            ),
            column(6,
                   fluidRow(
                     strong("Tier prices ($)")
                   ),
                   fluidRow(
                     HTML('<textarea id="budgetPrice" rows="6" cols="15" style="resize: none;">1.85\n2.33\n4.68\n6.77</textarea>')
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
                   em("Outdoor = PlantFactor * ET * LA  * (1/748)")
          )
          
        )#end conditionalPanel
        
  
      )#end wellPanel
    ),#end column
    
#----------------------- Output panels ------------------
    column( 8, #"main panel",
        tabsetPanel(type="tabs",
                    tabPanel("Residential",
                             fluidRow(
                               plotlyOutput("revenue_time_series", height=350)
                             ),
                             fluidRow(
                               # column(12, plotlyOutput("savings_boxplot", height=350) )
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
))
