library(dplyr)
library(zoo)
library(ggplot2)
library(scales)


plot_revenue_over_time <- function(data, cust_class){
  monthly_revenue <- filter(data, rate_code==cust_class) %>%  group_by(usage_date) %>% 
                      summarise(revenue=sum(total_bill, na.rm=TRUE)) %>% na.omit()
  
  ggplot(monthly_revenue, aes(usage_date, revenue, group=1)) + geom_line() +
    xlab("Date") + ylab("$") +
    scale_x_date(labels = date_format("%m-%y"), date_breaks="2 months")
}