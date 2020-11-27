rm(list = ls())
# install.packages('pacman')
library(pacman)
p_load(nycflights13, tidyverse)
data(flights)
data(airlines)
data(airports)
data(weather)

# Q1: Find all flights on Jan 1.
# select, arrange, filter, mutate, transmutate, slice
# which one should we use?

glimpse(flights)
flights %>% filter(month == 1, day == 1)

# Q2: Try to find arrival delay (arr_delay) more than 120 minutes and show arr_delay in the 
# fourth column.

flights %>% filter(arr_delay >= 120) %>% 
  select(year:day, arr_delay, everything())

# Q3: We need to find the longest arr_delay and its carrier, flight information.
# we need to think which function can rank data in descending order. 
# Use arrange!

flights %>% arrange(desc(arr_delay)) %>% 
  select(carrier, flight, arr_delay, everything()) %>% 
  slice(1)

# Q4: Which carrier has the worst average arr_delay? its value?
# Use group_by, summarise/mutate and arrange

flights %>% group_by(carrier) %>% 
  # mutate(arr_delay_avg = mean(arr.delay, na,rm = TRUE )
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(desc(arr_delay))

arrange(desc(arr_delay_avg)) %>% select(carrier, arr_delay_avg) %>% dinstinct()


# Q5: which hour should we take by avoiding the possible arr_delay (average arr_delay)?
# group_by(hour)

flights %>% group_by(hour) %>% summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>% arrange(desc(arr_delay))

# Q6: Based on flights, select the first 100 data of year, month, day, hour, origin, dest, tailnum, carrier.
# Then Use left_join() to add airline names to answers in the above and save the joined resutls as 'answer06'.
# Finally show the first 6 rows of data.

answer06 <- flights %>% slice(1:100) %>% 
  select(year:day, hour, origin, dest, tailnum, carrier) %>% 
  left_join(y = airlines, by = "carrier") %>% 
  head()

# Q7: Use left_join() to combine 'answer06' and 'weather', and then combine with 'airports'.
# Hint: left_join(..., by = c("dest" = "faa"))

answer06 %>% left_join(weather) %>% 
  left_join(airports, by = c("dest" = "faa"))

# Q8: Count the number of flights for dest = ALB, BDL and BTV in each month. (use `n()`)

flights %>% filter(dest %in% c("ALB", "BDL", "BTV")) %>% 
            group_by(year, month, dest) %>% 
            summarise(count = n())

# Q9: Use carrier, month, origin to compute average departure delay time (dep_delay). 

flights %>% select(carrier, month, origin, dep_delay) %>% 
            group_by(carrier, month, origin) %>% 
            mutate(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
            arrange(desc(avg_dep_delay))





