rm(list=ls())
library(tidyverse)
library(tidyquant)
library(timetk)

# Q1. Import stock data “tej_2016_2018.txt” and rename collumn name Co_ID, CoName, Date and MV% to id, name, date and cap_share.

read_csv("tej_2016_2018.txt")

main_tbl <- tej_2016_2018 %>% 
  rename("id"="CO_ID","name"="CoName","date"="Date","cap_share"="MV.")

# Q2. Extract OHLC for stocks and show the first 10 rows.

my_tbl <- tej_2016_2018 %>% 
  rename("id"="CO_ID","name"="CoName","date"="Date","cap_share"="MV.") %>% 
  select(id, date, Open, High, Low, Close) %>% 
  head(10)

#Q3. Convert stocks from long to wide with closing prices as follows (show the first 10 by 10 data):

my_tbl_2 <- tej_2016_2018 %>% 
  rename("id"="CO_ID","name"="CoName","date"="Date","cap_share"="MV.") %>% 
  select(id, date, Close) %>% 
  spread(key = id, value = Close)

my_tbl_3 <- my_tbl_2[1:10, 1:10]

# Q4. Compute number of na for each stock and use glimpse() to show the results as follows:

na_count <- my_tbl_2 %>% 
  map_df(~sum(is.na(.))) %>% 
  gather() %>% 
  filter(value != 0)

glimpse(na_count)

# Q5. Fill leading NAs with the most recent price and fill trailing NAs with 0. 
# Hint: use na.locf() to fill leading NAs with the most recent available values. 
# Then use is.na() to fill trailing NAs with 0. Check your results using sum(is.na(data)) to confirm your results. Check the tail data of stock id ‘2025’ if they are converted to be zeros.

q5_tbl <- my_tbl_2 %>% 
  na.locf(fromLast = TRUE, na.rm = FALSE)

q5_tbl %>% 
  map_df(~sum(is.na(.))) %>% 
  gather() %>% 
  filter(value != 0)

q5_tbl_2 <- q5_tbl %>% 
  select(-c("2025", '6131'))

sum(is.na(q5_tbl_2))

# Q6. Compute daily log returns and show the first 10 by 10 data.

q6_tbl <- q5_tbl_2 %>% 
  tk_xts(select = -date, date_var = date) %>% 
  Return.calculate(method = 'log')

class(q6_tbl)

q6_tbl_2 <- q6_tbl[-1,]
q6_tbl_2[1:10, 1:10]

# Q7. Compute monthly log returns and show the first 10 by 10 data.

# Q8. Find the largest market share of 50 stocks at the end of 2016.

top_50 <- main_tbl %>% 
  filter(date == "20161230") %>% 
  arrange(desc(cap_share)) %>% 
  head(50) %>% 
  select(date, cap_share, id, name)

# Q9. Plot 10 largest market cap stocks in TWSE as of year end of 2016.

top_10 <- top_50 %>% 
  head(10) %>% 
  select(id, cap_share) %>% 
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.double, as.character)
  
red_plot <- top_10 %>% 
  mutate_at('cap_share', as.numeric) %>%
  mutate(id = fct_reorder(id, desc(cap_share))) %>% 
  ggplot(aes(id, cap_share)) +
  geom_col(fill = 'red') +
  geom_text(aes(label = cap_share), position=position_dodge(width=0.9), vjust=-0.25, color = "black")

print(red_plot + labs(title= "The ten largest market capitalization stocks in TWSE as of 2016",
                      y="Market share %", x = "id"))

# Q10. Plot the density of the 5 largest market cap stock monthly returns from 2017-2018.

main_tbl %>% 
  select(id, date, Market.Cap.) %>% 
  arrange(desc(Market.Cap.)) %>% 
  group_by(id) %>% 
  summarise(Market.Cap. = sum(Market.Cap)) %>% 
head(5) %>% 
  ggplot(aes(x = monthly returns, fill = asset)) +
  geom_density(alpha = 0.2)






