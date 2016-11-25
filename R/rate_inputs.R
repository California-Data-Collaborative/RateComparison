

ratePartInput <- function(id, part_name="", field_or_formula="", depends_col_list, current_selected=NULL){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(4, textInput(ns("part_name"), label=NULL, value=part_name ) ),
      column(4, selectInput(ns("depends_col"), label=NULL, choices=depends_col_list, 
                            selected=current_selected, multiple=TRUE) ),
      column(4, textInput(ns("field_or_formula"), label=NULL, value=field_or_formula ) )
    )
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
  )
  
}

ratePart <- function(input, output, session){
  
}