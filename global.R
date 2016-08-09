
library(plotly)
library(shiny)
library(dplyr)


utility_code <- "SMWD"


#******************************************************************
# Read in the data and map the columns to application columns
#******************************************************************
read_data <- function(filename, cust_col, usage_col, month_col, year_col, et_col, hhsize_col, irr_area_col, 
                      rate_code_col){
  print("Reading data...")
  start.time <- Sys.time()
  
  data <- tbl_df(read.csv(filename)) %>% 
    dplyr::rename_(.dots=setNames(list(cust_col), "cust_id")) %>%
    dplyr::rename_(.dots=setNames(list(usage_col), "usage_ccf")) %>%
    dplyr::rename_(.dots=setNames(list(month_col), "usage_month")) %>%
    dplyr::rename_(.dots=setNames(list(year_col), "usage_year")) %>%
    dplyr::rename_(.dots=setNames(list(et_col), "et_amount")) %>%
    dplyr::rename_(.dots=setNames(list(hhsize_col), "hhsize")) %>%
    dplyr::rename_(.dots=setNames(list(irr_area_col), "irr_area")) %>%
    dplyr::rename_(.dots=setNames(list(rate_code_col), "rate_code")) %>%
    dplyr::mutate(usage_date = as.Date(usage_date)) %>%
    dplyr::arrange(usage_date) %>%
    filter(usage_date < as.Date("2016-01-01"))
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  print("...loaded.")
  return(data)
}


test_file <- switch(utility_code,
                    "MNWD"="data/mnwd_test.csv",
                    "LVMWD"="data/lvmwd_test.csv",
                    "SMWD"="data/smwd_test.csv")

# Read data from file and rename the columns to be compatable with internal calls
df <- read_data(test_file, cust_col="cust_loc_id", usage_col="usage_ccf", month_col="usage_month", 
                year_col="usage_year", et_col="usage_et_amount", hhsize_col="cust_loc_hhsize", 
                irr_area_col="cust_loc_irr_area_sf", rate_code_col= "cust_loc_class")


# Update the time slider with the actual date values in the data
min_date <- min(df$usage_date)
max_date <- max(df$usage_date)
print(min_date, max_date)

