#****************************************************************************************** 
#rate_inputs.R
#
# This file defines two modules (tierBox and ratePart) corresponding to two different types of inputs found on the
# input panel of the classGraph module.
#
# tierBox is a module that defines a text input box for displaying tier starts and prices.
# 
# ratePart is a module that defines a more complex, multi-input for specifying either 
# simple values like landscape factors and flat rate charges, or more complex values that
# depend on other columns in the source data. For example it is possible to define a fixed
# service charge that is dependent on the meter size of a customer.
#
#******************************************************************************************


#********************************************
# tierBox
#********************************************
tierBoxInput <- function(id){
  ns <- NS(id)
  tagList(uiOutput(ns("tierInfoBox")))
}

tierBox <- function(input, output, session, part_name, part_name_long, 
                    rate_type, rate_type_provided, rate_part){
  
  output$tierInfoBox <- renderUI({
    ns <- session$ns

    # should use defaults?
    if(is.null(rate_part) || rate_type != rate_type_provided){
      box_info <- defaults[[rate_type]][[part_name]]
    }else{
      box_info <- paste0(rate_part, collapse="\n")
      print(paste0("BOX INFO: ", box_info))
    }
    
    tagList(
      textAreaInput(ns("tier_box"), label=part_name_long, value=box_info,
                    height=150)#text_height(input$depend_cols))
    )
  })
  
  return(input)
} 



#********************************************
# ratePart
#********************************************
ratePartInput <- function(id){
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("inputRow")),
    uiOutput(ns("inputDropdown"))
  )
}

ratePart <- function(input, output, session, part_name, part_name_long="", depends_col_list, 
                     current_selected=NULL, simple_value_provided=NULL, is_expanded=FALSE, value_map=list(),
                     rate_part=NULL){
  
  
  output$inputRow <- renderUI({
    ns <- session$ns
    
    if(!is.null(rate_part)){
      if(is_map(rate_part)){
        print(paste0(part_name, " is a map!"))
        current_selected <- c(rate_part$depends_on)
        value_map <- rate_part$values
        is_expanded <- TRUE
      }else{
        simple_value_provided <- rate_part
      }
    }
    
    tagList(
      fluidRow(
         column(1, checkboxInput(ns("expanded"), label=NULL, value = is_expanded)),
         conditionalPanel(condition = sprintf("input['%s'] == false", ns("expanded")),
           column(5, strong( paste0(part_name_long, " ($):" ) )),
           column(5, numericInput(ns("simpleValue"), label=NULL, value=simple_value(simple_value_provided, part_name)) )
         ),
         conditionalPanel(condition = sprintf("input['%s'] == true", ns("expanded")),
           column(5, strong( paste0(part_name_long, ":\n(depends on...)") ) ),
           column(5, selectInput(ns("depend_cols"), label=NULL, choices=depends_col_list, 
                       selected=c(current_selected), multiple=TRUE)
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
    
    if(!is.null(rate_part)){
      if(is_map(rate_part)){
        print(paste0(part_name, " is a map!"))
        current_selected <- c(rate_part$depends_on)
        value_map <- rate_part$values
        is_expanded <- TRUE
      }else{
        simple_value_provided <- rate_part
      }
    }
    
    tagList(
      # Display text entry boxes if the values depends on another
      conditionalPanel(condition = sprintf("input['%s'] == true", ns("expanded")),
         fluidRow(
           column(7, textAreaInput(ns("depend_values"), label="Values", 
                                   value=unique_values(input$depend_cols),
                                   height=text_height(input$depend_cols) )),
           column(5, textAreaInput(ns("depend_charges"), label="Charges ($)", 
                                   value=eval_uniques(input$depend_cols, value_map),
                                   height=text_height(input$depend_cols)))
         )
      )
    )
    
  })

  return(input)
}


simple_value <- function(simple_value_provided, part_name){
  if(is.null(simple_value_provided)){
    ls <- list("flat_rate"=3, "gpcd"=55, "landscape_factor"=0.7)
    value <- ls[[part_name]]
  }else{
    value <- simple_value_provided
  }
  
  value
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

eval_uniques <- function(colList, value_map){
  ls <- unique_value_list(colList)
  retVal <- c()
  
  for(v in ls){
    print(v)
    if(v != ""){
      value <- value_map[[v]]
      
      if(!is.null(value)){
        retVal <- c(retVal, value)
      }else{
        retVal <- c(retVal, 0.0)
      }
      
    }else{
      retVal <- c(retVal, 0.0)
    }
  }
  return( paste0(retVal, collapse="\n") )
}


text_height <- function(colList){
  return(26+21*num_uniques(colList))
}




