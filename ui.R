

shinyUI(fluidPage(
  titlePanel("Rate Comparison"),
  
  fluidRow(
    column(4,
      wellPanel( 
        radioButtons("rateType", label = "Rate Type",
                     choices = list("Flat" = "Flat", "Tiered" = "Tiered", "Budget" = "Budget"), 
                     selected = "Flat"),
        
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
          )
        )
        
  
  
                  )
      ),#end column,
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
  )
)
