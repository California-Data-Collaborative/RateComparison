
library(plotly)
library(shiny)

#--------------------------- Function Definitions ------------------------

#******************************************************************
# Pull all analyzed rebate households from the DB
#******************************************************************
get_rebate_households <- function(connection){
  query <- build_sql("SELECT * FROM cust_loc_join_rebates_with_dupes")
  treatments <- tbl(connection, query)
  tmp <- collect(treatments)
  tmp$cust_loc_id <- as.character(tmp$cust_loc_id)
  return(tmp)
}