
# automatic reading rate codes, renaming the tabs, consistent naming convention
shinyUI(navbarPage(
  #color #2c3e50
  title = HTML(paste('<img src="CaDC_logo_bluegrey.png" ',
             'href="http://californiadatacollaborative.com/" ', 'height="60" ',
             'style="position: relative; top:-20px; right:-15px">')),
  theme = "bootstrap.css",
  windowTitle = "RateComparison",
  # tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.ico")),
  tabPanel(
    "Rate Comparison",
    fluidRow(
      tags$head(tags$link(rel="icon", type="image/x-icon", href="favicon.ico")),


      column( 12, #"main panel",
              uiOutput('classTabs')
      ) #end column
    )#end row
  ),#end tabpanel for navar
  tabPanel("Scenario Planning",
     
     checkboxInput("Planning", "Enable Scenario Planning", value = TRUE, 
                   width = NULL),
     
     numericInput("Months", "Number of months to Forecast", 
                  3, min = 1, max = 24, step = 1,
                  width = NULL),
     numericInput("Growth", "Expected Monthly Growth of number of accounts in the forecast period",
                  10, min = 0, max = 100, step = 1,
                  width = NULL),
     numericInput("EstUsagePerAccount", "Estimated Usage per New Account(ccf)",
                  20, min = 0, max = 1000, step = NA,
                  width = NULL)
  )

))
