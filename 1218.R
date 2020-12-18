rm(list=ls())
library(forcats)
library(tidyverse)

bike_orderlines_tbl <- readRDS("bike_orderlines.rds")

# 1.2 What is a ggplot?

g

view(g)

# scatter plot: quantity vs. total price

order_value_tbl <- bike_orderlines_tbl %>%
  select(order_id, order_line, total_price, quantity) %>%
  group_by(order_id)%>%
  summarise(
    total_quantity = sum(quantity),
    total_price    = sum(total_price)) %>%
  ungroup() 

order_value_tbl %>%
  ggplot(aes(x = total_quantity, y = total_price)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = 'lm', se = FALSE)

# Line plot: plot total price vs. year month

revenue_by_month <- bike_orderlines_tbl %>% 
  select(order_date, total_price) %>%
  mutate(year_month = floor_date(order_date, "months") %>% ymd()) %>% 
  group_by(year_month) %>% 
  summarise(revenue = sum(total_price)) %>% 
  ungroup()

revenue_by_month %>% 
  ggplot(aes(year_month, revenue)) +
  geom_line(size = 0.5, linetype = 1) +
  geom_smooth(method = 'loess', span = 0.2)

# Bar plot: category_2 vs. revenue (order from largest to smallest)

revenue_by_category_2 <- bike_orderlines_tbl %>% 
  select(category_2, total_price) %>% 
  group_by(category_2) %>% 
  summarise(revenue = sum(total_price)) %>% 
  ungroup()

revenue_by_category_2 %>% 
  mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>%
  ggplot(aes(category_2, revenue)) +
  geom_col(fill = 'coral') +
  coord_flip()
  
























  