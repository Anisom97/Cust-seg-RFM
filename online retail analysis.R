
data <- read.csv("Online Retail.csv")# IMPORT THE DATA FROM WORKING DIRECTORY

data1 <- data[,-c(1,2,3,8)] # DELTING THOSE VARIABLES WHICH IS NOT RELEVANT TO DO RFM

summary(data1)

library(dplyr)

data1 <- filter(data1, Quantity > 0, UnitPrice > 0)# TO FILTER THOSE DATA WITHOUT NEGATIVES

sum(is.na(data1))# TO CHECK NUMBER OF MISSING VALUES

data1 <- na.omit(data1)

summary(data1)
library(dplyr)

data1 <- mutate(data1, Revenue = UnitPrice*Quantity)# create a revenue column
str(data1$InvoiceDate)
library("lubridate")
attach(data1)

#convert the date vector: from chr to date
data1$InvoiceDate <- mdy(data1$InvoiceDate)

summary(data1$InvoiceDate)

#perform the analysis
library(rfm)
analysis_date <- lubridate::as_date("2011-12-10")# this function doesnot require the package
rfm_result <- rfm_table_order(data1, CustomerID, InvoiceDate, Revenue, analysis_date)
rfm_result

write.csv(rfm_result$rfm, "ONLINE RETAIL RESULT.csv")


#developing segments

segment_titles <- c("First Grade", "Loyal", "Likely to be Loyal",
                    "New Ones", "Could be Promising", "Require Assistance", "Getting Less Frequent",
                    "Almost Out", "Can't Lose Them", "Don't Show Up at All")
r_low <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)# minimum value of recency
r_high <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)#maximum value of recency
f_low <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
f_high <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
m_low <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
m_high  <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

divisions<-rfm_segment(rfm_result, segment_titles, r_low, r_high, f_low, f_high, m_low, m_high)

write.csv(divisions, "MY RETAIL DATA SETMENT.csv")


segments  <- divisions %>% count(segment) %>% arrange(desc(n)) %>% mutate(freq = n / sum(n)) %>% 
  mutate(percentage = freq*100) %>% rename(Segment = segment, Count = n)

View (segments)


write.csv (data1, "MYMODIFIED DATA.csv")
