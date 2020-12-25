sales_by_category_2 <- bike_orderlines_tbl %>% 
  select(order_date, category_2, total_price) %>% 
  
  mutate(order_date = ymd(order_date)) %>% 
  mutate(year = year(order_date)) %>% 
  
  group_by(category_2, year) %>% 
  summarise(revenue = sum(total_price)) %>% 
  ungroup() %>% 
  
  mutate(category_2 = fct_reorder(category_2, year, revenue))

         