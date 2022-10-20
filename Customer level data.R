
instlall.packages("rfm")#
library(rfm)
data <- read.csv ("customer.csv")# to import my data from working directory
str(customer1)
summary(customer1)
summary(data)
# customer information until 2006-12.30

analysis_date <- lubridate::as_date('2007-01-01')# fixing analysis date

?rfm_table_customer

rfm_result <- rfm_table_customer(data, customer_id = customer_id, n_transactions = number_of_orders,
                                 recency_days = recency_days, total_revenue = revenue, analysis_date = analysis_date)
rfm_result

write.csv(rfm_result$rfm, "myresult.csv")

# HEAT MAP
rfm_heatmap(rfm_result, brewer_name = "OrRd")
?rfm_heatmap
# BAR PLOT
rfm_bar_chart(rfm_result)
??brewer.pal.
# Histogram

rfm_histograms(rfm_result)

#Customers by Orders

rfm_order_dist(rfm_result)

## scatter plot

rfm_rm_plot(rfm_result) # recency and monetary value

rfm_fm_plot(rfm_result) # frequency and monetary value

rfm_rf_plot(rfm_result)# recency and frequency


#developing segments

?rfm_segment
segments <- c("First Grade", "Loyal", "Likely to be Loyal",
                    "New Ones", "Could be Promising", "Require Assistance", "Getting Less Frequent",
                    "Almost Out", "Can't Lose Them", "Don't Show Up at All")
r_low <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
r_high <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
f_low <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
f_high <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
m_low <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
m_high  <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

divisions<-rfm_segment(rfm_result, segments, r_low, r_high, f_low, f_high, m_low, m_high)
write.csv(divisions, "divisions.csv")

library(dplyr) # required for grouping

segments  <- divisions %>% count(segment) %>% arrange(desc(n)) %>% mutate(freq = n / sum(n)) %>% 
  mutate(percentage = freq*100) %>% rename(segment = segment, COUNT = n)

segments1 <- divisions %>% group_by(segment) %>%
  summarise(MONEY_VALUE = mean(amount), RECENCY = mean(recency_days), AVG_FREQ = mean(transaction_count))
  

overall <- merge (segments, segments1, BY = segment)


View (segments)
View (divisions)




## other plots
rfm_plot_median_recency(divisions)

rfm_plot_median_frequency(divisions)

rfm_plot_median_monetary(divisions)
