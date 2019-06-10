library(gplots)

# Relationship between weekdays and total booked tickets

# For channel 39

data_1$weekday = weekdays(data_1$date)

channel_dt = data_1 %>%
  filter(channel_id==39) %>%
  group_by(weekday) %>%
  summarise(n_tickets = sum(n_tickets,na.rm = TRUE))

channel_dt$n_tickets = channel_dt$n_tickets/1000

chisq.test(channel_dt$n_tickets)
# since p-value is 1, we accept the null the hypothesis
# number of tickets booked is independent of days of a week

# For channel 28

channel_dt = data_1 %>%
  filter(channel_id==28) %>%
  group_by(weekday) %>%
  summarise(n_tickets = sum(n_tickets,na.rm = TRUE))

channel_dt$n_tickets = channel_dt$n_tickets/1000

chisq.test(channel_dt$n_tickets)

# For channel 37

channel_dt = data_1 %>%
  filter(channel_id==37) %>%
  group_by(weekday) %>%
  summarise(n_tickets = sum(n_tickets,na.rm = TRUE))

channel_dt$n_tickets = channel_dt$n_tickets/1000

chisq.test(channel_dt$n_tickets)

#----------------------------------------------------------
# For channel 24

country_dt = data_1 %>%
  filter(country_1==24) %>%
  group_by(weekday) %>%
  summarise(n_tickets = sum(n_tickets,na.rm = TRUE))

country_dt$n_tickets = country_dt$n_tickets/1000

chisq.test(country_dt$n_tickets)
# since p-value is 1, we accept the null the hypothesis
# number of tickets booked is independent of days of a week

# For channel 4

country_dt = data_1 %>%
  filter(country_1==4) %>%
  group_by(weekday) %>%
  summarise(n_tickets = sum(n_tickets,na.rm = TRUE))

country_dt$n_tickets = country_dt$n_tickets/1000

chisq.test(country_dt$n_tickets)

# For channel 11

country_dt = data_1 %>%
  filter(country_1==11) %>%
  group_by(weekday) %>%
  summarise(n_tickets = sum(n_tickets,na.rm = TRUE))

country_dt$n_tickets = country_dt$n_tickets/1000

chisq.test(country_dt$n_tickets)

