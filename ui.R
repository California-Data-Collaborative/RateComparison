
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
     # numericInput("Growth", "Expected Monthly Growth/Decline of number of accounts (Enter Negative value for reduction in accounts)",
     #              -10, min = -1000, max = 1000, step = 1,
     #              width = NULL),
     numericInput("ResidentialSingle", "Residential Single accounts growth/decline per Month", 
                  0, min = -1000, max = 1000, step = 1,
                  width = NULL),
     numericInput("ResidentialMulti", "Residential Multi accounts growth/decline per Month", 
                  -1, min = -1000, max = 1000, step = 1,
                  width = NULL),
     numericInput("Irrigation", "Irrigation accounts growth/decline per Month", 
                  -1, min = -1000, max = 1000, step = 1,
                  width = NULL),
     numericInput("Commercial", "Commercial accounts growth/decline per Month", 
                  -1, min = -1000, max = 1000, step = 1,
                  width = NULL),
     numericInput("Other", "Other accounts growth/decline per Month", 
                  -1, min = -1000, max = 1000, step = 1,
                  width = NULL),
     numericInput("Institutional", "Institutional accounts growth/decline per Month", 
                  -1, min = -1000, max = 1000, step = 1,
                  width = NULL),
     
     
     numericInput("EstUsagePerAccount", "Estimated Usage per New Account(ccf)",
                  10, min = 0, max = 1000, step = NA,
                  width = NULL)
  )

))
