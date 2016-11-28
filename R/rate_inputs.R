

ratePartInput <- function(id){
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("inputRow")),
    uiOutput(ns("inputDropdown"))
  )
}

ratePart <- function(input, output, session, part_name, part_name_long="", depends_col_list, 
                     current_selected=NULL, labels=FALSE, simple_value=0){
  
  output$inputRow <- renderUI({
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
           column(5, selectInput(ns("depend_cols"), label=NULL, choices=depends_col_list, 
                       selected=current_selected, multiple=TRUE)
           )
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
  
  output$inputDropdown <- renderUI({
    ns <- session$ns
    
    tagList(
      # Display text entry boxes if the values depends on another
      conditionalPanel(condition = sprintf("input['%s'] == true", ns("expanded")),
         fluidRow(
           column(8, textAreaInput(ns("depend_values"), label="Values", value=unique_values(input$depend_cols),
                                   height=text_height(input$depend_cols) )),
           column(4, textAreaInput(ns("depend_charges"), label="Charges ($)", value=simple_value,
                                   height=text_height(input$depend_cols)))
         )
      )
    )
    
  })

  # return(input)
}


unique_value_list <- function(colList){
  
#   uniqueList <- list()
#   
#   for(i in 1:length(colList)){
#     col <- colList[i]
#   }
#   
#   expand.grid(unique(df$meter_size), unique(df$meter_size), stringsAsFactors=FALSE)
  
  if(is.null(colList)){
    retVal <- ""
  }else{
    sorted <- df %>%group_by_(colList[1]) %>% summarise_(count=sprintf("length(%s)", colList[1]) ) %>% 
                arrange(desc(count))
    retVal <- sorted[[colList[1]]]
  }
  
  return(retVal)
}

unique_values <- function(colList){
  ls <- unique_value_list(colList)
  return(paste0(ls, collapse="\n"))
}

num_uniques <- function(colList){
  ls <- unique_value_list(colList)
  return(length(ls))
}

text_height <- function(colList){
  return(26+21*num_uniques(colList))
}




