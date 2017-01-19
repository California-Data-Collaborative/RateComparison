library(dplyr)
library(zoo)
library(ggplot2)
library(scales)
library(reshape2)
library(stringi)

#' Line chart of revenue over time.
#'
#' \code{plot_revenue_over_time} returns a line chart showing revenue over 
#' time for both baseline and hypothetical rate structures. The baseline
#' rate is shown in black and the hypthotical in blue.
#'
#' @param data The dataframe filtered by date range and rate code.
#' 
#' @return A plotly object created from a ggplot chart, with plotly's
#' modebar removed.
plot_revenue_over_time <- function(data, display_type){
  start.time <- Sys.time()
  if(display_type=="Revenue"){
    monthly_revenue <- data %>%  group_by(usage_date) %>% 
                      summarise(revenue=sum(total_bill, na.rm=TRUE),
                                baseline_revenue=sum(baseline_bill, na.rm=TRUE)) %>% 
                      mutate(Baseline = baseline_revenue/1000000) %>%
                      mutate(Hypothetical = revenue/1000000) %>%
                      select(usage_date,Baseline,Hypothetical)
    monthly_revenue <- melt(monthly_revenue, id="usage_date") %>% rename(Revenue=variable)
  
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print("Calcing monthly_revenue")
    print(time.taken)
    start.time <- Sys.time()

    p <- ggplot(monthly_revenue, aes(x=usage_date, y=value, color=Revenue)) + 
    # geom_ribbon(aes(x=usage_date, ymax=rev_mill, ymin=base_rev_mill), fill="grey", alpha=.5) +
         geom_line() + 
         #geom_vline(xintercept=as.numeric(max(df$usage_date)),color='red3',linetype=2) +
         scale_linetype_manual(values = c("Baseline"="dashed", "Hypothetical"="solid")) +
         scale_color_manual(values=c("Baseline"="black", "Hypothetical"="steelblue")) +
         xlab("") + ylab("Revenue (Million $)") + 
         # theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
         # scale_x_date(labels = date_format("%m-%y"), date_breaks="1 months") +
         scale_y_continuous(labels = comma)
         #geom_text(data=data.table(date=max(df$usage_date),extracol=0),aes(date,extracol),label="forecast",color='red3',angle=45,vjust=-0.5,hjust=-0.5)
  
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print("Making line chart") 
    print(time.taken)
  }
  else{ #if usage is selected, plot monthly usage
    monthly_usage <- data %>%  group_by(usage_date) %>%  
                     summarise(hypothetical_usage=sum(hypothetical_usage, na.rm=TRUE),
                               baseline_usage=sum(baseline_usage, na.rm=TRUE)) %>% 
                     mutate(Baseline = baseline_usage/1000000) %>%
                     mutate(Hypothetical = hypothetical_usage/1000000) %>%
                     select(usage_date,Baseline,Hypothetical)
    monthly_usage <- melt(monthly_usage, id="usage_date") %>% rename(Usage=variable)
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print("Calcing monthly_usage")
    print(time.taken)
    start.time <- Sys.time()
    
    p <- ggplot(monthly_usage, aes(x=usage_date, y=value, color=Usage)) + 
         # geom_ribbon(aes(x=usage_date, ymax=rev_mill, ymin=base_rev_mill), fill="grey", alpha=.5) +
         geom_line() + 
         #geom_vline(xintercept=as.numeric(max(df$usage_date)),color='red3',linetype=2) +
         scale_linetype_manual(values = c("Baseline"="dashed", "Hypothetical"="solid")) +
         scale_color_manual(values=c("Baseline"="black", "Hypothetical"="steelblue")) +
         xlab("") + ylab("Usage (Million ccf)") + 
         # theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
         # scale_x_date(labels = date_format("%m-%y"), date_breaks="1 months") +
         scale_y_continuous(labels = comma)
         #geom_text(data=data.table(date=max(df$usage_date),extracol=0),aes(date,extracol),label="forecast",color='red3',angle=45,vjust=-0.5,hjust=-0.5)
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print("Making line chart") 
    print(time.taken)
    
  }


  p#ggplotly(p) %>% config(displayModeBar = FALSE)
}

#' Histogram of bill changes.
#'
#' \code{plot_bill_change_histogram} returns a histogram of changes 
#' (hypothetical - baseline) in total amount paid during the time period for each customer
#'
#' @param data The dataframe filtered by date range and rate code.
#' 
#' @return A plotly object created from a ggplot chart, with plotly's
#' modebar removed.
plot_bill_change_histogram <- function(data, display_type, bar_type){
  start.time <- Sys.time()
  if(display_type=="Revenue"){
    if(sum(abs(data$changes)) < 1){
      p <- ggplot() + 
        geom_vline(xintercept = 0, color="#CC0000")
      if(bar_type == "Absolute"){
        p <- p + xlab("Change in total amount paid ($)")
      }else{
        p <- p + xlab("% Change in total amount paid")   
      }
    }
    else{
      p <- ggplot(data, aes(x=changes)) + geom_histogram() + 
        geom_vline(xintercept = mean(data$changes, na.rm=TRUE), color="#CC0000") +
        theme(axis.ticks = element_blank(), axis.text.y = element_blank())
      if(bar_type == "Absolute"){
        p <- p + xlab("Change in total amount paid ($)") + ylab("")
      }else{
        p <- p + xlab("% Change in total amount paid") + ylab("")   
      }
    }
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print("Making Revenue histogram") 
    print(time.taken)
  }
  else{ #if usage is selected, plot changes in usage
    if(sum(abs(data$changes_in_usage)) < 1){  
      p <- ggplot() + 
        geom_vline(xintercept = 0, color="#CC0000")
      if(bar_type == "Absolute"){
        p <- p + xlab("Change in total amount used (ccf)")
      }else{
        p <- p + xlab("% Change in total amount used")   
      }
    }
    else{
      p <- ggplot(data, aes(x=changes_in_usage)) + geom_histogram() + 
        geom_vline(xintercept = mean(data$changes_in_usage, na.rm=TRUE), color="#CC0000") +
        theme(axis.ticks = element_blank(), axis.text.y = element_blank())
      if(bar_type == "Absolute"){
        p <- p + xlab("Change in total amount used (ccf)") + ylab("")
      }else{
        p <- p + xlab("% Change in total amount used") + ylab("")   
      }
    }
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print("Making Usage histogram") 
    print(time.taken)
    
  }
  ggplotly(p) %>% config(displayModeBar = FALSE)
}

#' Boxplot of bill changes.
#'
#' \code{plot_bill_change_boxplot} returns a small boxplot of changes 
#' (hypothetical - baseline) in total amount paid during the time period 
#' for each customer. Designed to complement \code{\link{plot_bill_change_histogram}}.
#'
#' @param data The dataframe filtered by date range and rate code.
#' 
#' @return A plotly object created from a ggplot chart, with plotly's
#' modebar removed.
plot_bill_change_boxplot <- function(data, display_type, bar_type){
  if(display_type=="Revenue"){
     if(sum(abs(data$changes)) < 1){
       p <- ggplot() + 
       geom_vline(xintercept = 0, color="#CC0000") +
       xlab("")
     }
     else{
       p <- ggplot(data, aes(change_group, changes)) + geom_boxplot(outlier.size=1) +
       coord_flip() + xlab("") + ylab("") + 
       theme(axis.ticks = element_blank(), axis.text.y = element_blank())
     }
  }
  else{ #if usage is selected, plot changes in usage
     if(sum(abs(data$changes_in_usage)) < 1){  
       p <- ggplot() + 
       geom_vline(xintercept = 0, color="#CC0000") +
       xlab("")
    }
    else{
       p <- ggplot(data, aes(change_group, changes_in_usage)) + geom_boxplot(outlier.size=1) +
       coord_flip() + xlab("") + ylab("") + 
       theme(axis.ticks = element_blank(), axis.text.y = element_blank())
    } 
    
  }
  ggplotly(p) %>% config(displayModeBar = FALSE)
}

#' Barchart showing revenue/usage by tier.
#'
#' \code{plot_barchart_by_tiers} returns a bar chart showing the revenue/usage
#' generated under the baseline and hypthetical rate structures. These bars are 
#' further broken down and colored by tier, so that the amount of revenue/usage 
#' generated in each tier is visible.
#'
#' @param data The dataframe filtered by date range and rate code.
#' @param display_type A string specifying whether to display "Revenue" in millions
#' of dollars, of "Usage" in thousands of acre-feet.
#' @param bar_type A string showing whether to display the "Absolute" revenue 
#' in each tier, or the "Percent" of revenue in each tier.
#' 
#' @return A plotly object created from a ggplot chart, with plotly's
#' modebar removed.
plot_barchart_by_tiers <- function(data, display_type, bar_type){

  if(display_type=="Revenue"){
    # Select revenue in each tier
    d <- colSums(data %>% select(matches("[B|X]R[0-9]")), na.rm=TRUE)
    d <- tbl_df(data.frame(lapply(d, function(x) t(data.frame(x))))) %>%
         mutate(id=1)
    d <- melt(d, id.vars="id" ) %>% 
         mutate(type=ifelse(grepl("B.*", variable), "Baseline", "Hypothetical"),
                Tier = get_tier_name(variable),
                value = value/1000000.0)
    lab_str <- "Variable Rev. During Time Period (Mill. $)"
  }
  else{
    # Select usage in each tier
    d <- colSums(data %>% select(matches("[B|X][0-9]")), na.rm=TRUE)
    d <- tbl_df(data.frame(lapply(d, function(x) t(data.frame(x))))) %>%
         mutate(id=1)
    d <- melt(d, id.vars="id" ) %>% 
         mutate(type=ifelse(grepl("B.*", variable), "Baseline", "Hypothetical"),
                Tier = get_tier_name(variable),
                value = value*0.00229569/1000)
    lab_str <- "Usage During Time Period (Thousand AF)"
  }

  if(bar_type == "Absolute"){
    p <- ggplot(d, aes(type, value, fill=Tier)) + geom_bar(stat="identity") +
      xlab("") + ylab(lab_str)
  }
  else{
    hypothetical_perc_df <- d %>% filter(type == 'Hypothetical') %>% mutate(value = value/sum(value)*100)
    baseline_perc_df <- d %>% filter(type == 'Baseline') %>% mutate(value = value/sum(value)*100)
    perc_df <- rbind(hypothetical_perc_df, baseline_perc_df)
    p <- ggplot(perc_df, aes(type, value, fill=Tier)) + geom_bar(stat="identity") +
      xlab("") + ylab(paste("%",lab_str))
    
  }
  ggplotly(p) %>% config(displayModeBar = FALSE)
}

#' Barchart showing percentage fixed revenue.
#'
#' \code{plot_fixed_revenue} returns a bar chart where bars represent the
#' percent of revenue that comes from fixed charges (e.g. service charges based
#' on meter size).
#'
#' @param data The dataframe filtered by date range and rate code.
#' @param bar_type A string showing whether to display the "Absolute" 
#' amount of fixed revenue or the "Percent" of total revenue that is fixed.
#' 
#' @return A plotly object created from a ggplot chart, with plotly's
#' modebar removed.
plot_fixed_revenue <- function(data, bar_type){
  
  # Select revenue in each tier
  d <- colSums(data %>% select(baseline_variable_bill, baseline_bill, variable_bill, total_bill), 
               na.rm=TRUE)
  d <- tbl_df(data.frame(lapply(d, function(x) t(data.frame(x))))) %>%
       mutate(baseline_fixed=baseline_bill-baseline_variable_bill,
              hypthetical_fixed=total_bill-variable_bill, id=1) %>%
       mutate(Baseline=100*baseline_fixed/baseline_bill,
              Hypothetical=100*hypthetical_fixed/total_bill) %>%
       select(Baseline, Hypothetical, id)
  d <- melt(d, id.vars="id" ) 
  lab_str <- "Percent Fixed Revenue"

  if( (sum(d$value) < 0.1) || is.nan(d$value) ){
    p <- ggplot() + 
      geom_hline(yintercept = 0, color="#CC0000") +
      xlab("") + ylab(lab_str)
  }else{
    #if(bar_type == "Absolute"){
      p <- ggplot(d, aes(variable, value, fill=variable)) + geom_bar(stat="identity") +
        xlab("") + ylab(lab_str) + #coord_flip() + 
        scale_fill_manual( values=c("Hypothetical"="steelblue", "Baseline"="black") ) +
        guides(fill=FALSE)
    #}
    #else{
      
    #}
  }
  ggplotly(p) %>% config(displayModeBar = FALSE)
}

#' Convert generic tier labels to more descriptive labels.
#'
#' \code{get_tier_name} takes a generic label, like "X1" or "B1", that is 
#' generated when caculating tiered charges, and converts it to a more
#' descritive label for use when plotting.
#'
#' @param labels A default, nondescriptive label such as "X1" meaning usage in tier 1,
#' or "B1" meaning bill amount from tier 1.
#' 
#' @return A string representing the name of a tier, "Tier 1", "Tier 3", etc.
get_tier_name <- function(labels){
  return(paste("Tier", stri_sub(labels, -1, -1) ))
}







