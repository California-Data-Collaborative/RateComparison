

shinyUI(fluidPage(
  titlePanel("Rate Comparison"),
  
  fluidRow(
    column(4,
      wellPanel( 
        fluidRow(
          column(6, 
                 radioButtons("rateType", label = "Rate Type",
                              choices = list("Flat" = "Flat", "Tiered" = "Tiered", "Budget" = "Budget"), 
                              selected = "Flat")
          ),
          column(6, 
                 numericInput("fixedCharge", label = "Fixed Charge", value = 30.5)
          )
        ),#end row
        
        conditionalPanel(
          condition = "input.rateType == 'Flat'",
          fluidRow(
              column(12,
                     numericInput("flatRate", label = "Price per CCF ($)", value = 2.7)
              )
          )#end row
        ),#end conditionalPanel
        
        conditionalPanel(
          condition = "input.rateType == 'Tiered'",
          fluidRow(
            column(12,
                   numericInput("numTiers", label = "Number of Tiers", value = 3)
            )
          )#end row
        ),#end conditionalPanel
        
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
          )#end row
        )#end conditionalPanel
        
  
      )#end wellPanel
    ),#end column
    
    column( 8, #"main panel",
        tabsetPanel(type="tabs",
                    tabPanel("Overall",
                             fluidRow(
                               # column(5, htmlOutput("result_text")),
                               # column(7, plotlyOutput("savings_histogram", height=350) )
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
