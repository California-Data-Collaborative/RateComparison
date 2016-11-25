

ratePartInput <- function(id){
  ns <- NS(id)
  uiOutput(ns("inputBox"))
}

ratePart <- function(input, output, session, part_name="", depends_col_list, 
                     current_selected=NULL, labels=FALSE, field_or_formula=""){
  
  output$inputBox <- renderUI({
    ns <- session$ns
    
    if(labels){
      labelList = c("Name:", "Depends on:", "Value:")
    }
    else{
      labelList = c(NULL, NULL, NULL)
    }
    
    tagList(
      fluidRow(
        column(4, textInput(ns("part_name"), label=labelList[1], value=part_name) ),
        column(4, selectInput(ns("depends_col"), label=labelList[2], choices=depends_col_list, 
                              selected=current_selected, multiple=TRUE) ),
        column(4, textAreaInput(ns("field_or_formula"), label=labelList[3], value=field_or_formula,
                                height=50))
      ),
      #     ),
      #     
      #     fluidRow(
      #       conditionalPanel(
      #         condition = "input.depends_col == null",
      #         column(12, textInput(ns("field_or_formula"), label=NULL, value=field_or_formula ) )
      #       ),
      #       conditionalPanel(
      #         condition = "input.depends_col != null",
      #         column(12, textInput(ns("field_or_formula"), label=NULL, value="DLKJLJK" ) )
      #       )
      #         
      #     )
      
      tags$style(
        ".selectize-dropdown, .selectize-input, .selectize-input { 
        line-height: 29px; 
        }"
      )
    )
    
  })
  
}