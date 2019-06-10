setwd("D:/project/FlixBus")

# load libraries -------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyverse)
library(stringr)
library(RcppRoll)

# data readding---------------------------------------------------------------

orders_channel = read_csv("./input_data/orders_channels.csv")
cat("No of records in Order Channels data: ", nrow(orders_channel))
orders_channel = unique(orders_channel)
cat("No of records in Order Channels after removing duplicates: ", nrow(orders_channel))


orders_country = read_csv("./input_data/orders_country.csv")
cat("No of records in Order Country data: ", nrow(orders_country))
orders_country = unique(orders_country)
cat("No of records in Order Country after removing duplicates: ", nrow(orders_country))
cat("Order Country data has no duplicates")

orders_ticket = read_csv("./input_data/orders_tickets.csv")
orders_ticket = orders_ticket %>% group_by(id,type) %>% 
  summarise(n_tickets = sum(n_tickets,na.rm = TRUE))
cat("No of records in Order Ticket data: ", nrow(orders_ticket))


# merge data-------------------------------------------------------------------

# get exhaustive list of orders
order_list = c(orders_channel$id,orders_country$id,orders_ticket$id)
order_list = unique(order_list)
cat("Unique length of order list: ", length(order_list))

data = data.frame(id = order_list)
data = left_join(data,orders_channel) # joining by ID
data = left_join(data,orders_country) # joining by ID
data = left_join(data,orders_ticket)  # joining by ID

# sort the data 
data = data[order(data$date,data$id),]

# impute country_1

data$country_1[is.na(data$country_1)] = data$country_2[is.na(data$country_1)]
table(is.na(data$country_1))

data_new = data %>% 
  fill(channel_id, country_1)

data_new$key = paste0(data_new$country_1,"_",data_new$channel_id)
write.csv(data_new, file = "./input_data/combined_data.csv",row.names = FALSE)


ticket_sold_dist = data_new %>% group_by(key) %>% 
  summarise(n_tickets = sum(n_tickets, na.rm = TRUE),
            row_count = n()
  ) %>%
  arrange(desc(n_tickets)) %>%
  mutate(freq = n_tickets/sum(n_tickets)) %>%
  mutate(cumsum = cumsum(freq))

ticket_sold_dist$class = ifelse(ticket_sold_dist$cumsum<=0.95,"A","B")
ticket_sold_dist$ratio = ticket_sold_dist$n_tickets/ticket_sold_dist$row_count

# add class info in data_new

data_new = left_join(data_new, ticket_sold_dist[,c("key","class")])

# Add new variables to the existing data ---------------------------------

# Machine learning models cannot simply 'understand' temporal data so we much explicitly create time-based features. 
# Here we create the temporal features day of week, isWeekend, isWeekday, month_end and 
# end_week_of_month,start_week_of_month, from the date field
# These new features can be helpful in identifying seasonal & cyclical patterns in the tickets booked.


data_1 = data_new %>% group_by(channel_id,country_1,date,class,key) %>%
  summarise(n_tickets = sum(n_tickets,na.rm = TRUE))

data_1 = data_1[order(data_1$channel_id,data_1$country_1,data_1$date),]
data_1 = data.frame(data_1)
data_1$Year = as.numeric(format(data_1$date,"%Y"))
data_1$month = as.numeric(format(data_1$date,"%m"))
data_1$weekday = (weekdays(data_1$date))
data_1$day_of_month = as.integer(format(data_1$date,"%d"))
data_1$isWeekend = as.numeric(ifelse(data_1$weekday=="Saturday" | data_1$weekday=="Sunday",1,0))
data_1$isWeekday = as.numeric(ifelse(data_1$weekday=="Saturday" | data_1$weekday=="Sunday",0,1))
data_1$month_end = as.numeric(ifelse(data_1$day_of_month>28,1,0))
data_1$end_week_of_month = as.numeric(ifelse(data_1$day_of_month>23 & data_1$day_of_month<=31,1,0))
data_1$start_week_of_month = as.numeric(ifelse(data_1$day_of_month>=1 & data_1$day_of_month<=7,1,0))
data_1$country_2 = NULL
data_1$weekday = NULL

vars = c("month","day_of_month","isWeekend","isWeekday","month_end","end_week_of_month",
         "start_week_of_month","lag_1","avg_7","avg_3")
