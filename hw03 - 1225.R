library(tidyverse)
library(ggplot2)

d_funds_type <- tibble(
  type = c("Fund A", "Fund B", "Fund C"),
  freq = c(16, 4, 5)
)
knitr::kable(d_funds_type)

# Plot Barchart
d_funds_type %>% 
  ggplot(aes(type, freq)) +
  geom_col(fill = "dark gray")

# Barchart in descending order:

d_funds_type %>% 
    mutate(type = fct_reorder(type, desc(freq))) %>% 
    ggplot(aes(type,freq)) +
    geom_col(fill = 'dark grey')

# Barchart in flip coordinates:
  
d_funds_type %>% 
  mutate(type = fct_reorder(type, desc(freq))) %>% 
  ggplot(aes(type,freq)) +
  geom_col(fill = 'dark grey')
  coord_flip()