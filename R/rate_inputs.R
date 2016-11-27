

ratePartInput <- function(id){
  ns <- NS(id)
  uiOutput(ns("inputBox"))
}

ratePart <- function(input, output, session, part_name, part_name_long="", depends_col_list, 
                     current_selected=NULL, labels=FALSE, simple_value=0){
  
  output$inputBox <- renderUI({
    ns <- session$ns
    
    
    
    tagList(
      fluidRow(
         column(1, checkboxInput(ns("expanded"), label=NULL, value = FALSE)),
         conditionalPanel(condition = sprintf("input['%s'] == false", ns("expanded")),
           column(5, strong( paste0(part_name_long, " ($):" ) )),
           column(5, numericInput(ns("simpleValue"), label=NULL, value=simple_value) )
         ),
         conditionalPanel(condition = sprintf("input['%s'] == true", ns("expanded")),
           column(5, strong( paste0(part_name_long, ":\n(depends on...)") ) ),
           column(5, selectInput(ns("depend_col"), label=NULL, choices=depends_col_list, 
                       selected=current_selected, multiple=TRUE)
           )
         )
      ),
      # Display text entry boxes if the values depends on another
      conditionalPanel(condition = sprintf("input['%s'] == true", ns("expanded")),
         fluidRow(
           column(4, textAreaInput(ns("depend_values"), label="Values", value=simple_value,
                                   height=50)),
           column(4, textAreaInput(ns("depend_charges"), label="Charges ($)", value=simple_value,
                                   height=50))
         )
      ),
      
      # Make the dropdown the proper height to match other input boxes
      tags$style(
        ".selectize-dropdown, .selectize-input, .selectize-input { 
        line-height: 29px; 
        }"
      )
    )
    
  })

  # return(input)
  
}