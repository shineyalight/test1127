library(forcats)
library(tidyverse)

multiple_choice_responses <- read_csv("multipleChoiceResponses[1].csv")

glimpse(multiple_choice_responses)


# Q1. Change all the character columns to factor columns and save the new dataset as
# responses_as_factors. (hint: use mutate_if() )

responses_as_factors <- multiple_choice_responses %>% 
  mutate_if(is.character, as.factor)

# Q2. Create a new dataset, number_of_levels, where you:
# Use summarise_all to apply the function nlevels to each column.
# And change the dataset's format from wide to long.

number_of_levels <- responses_as_factors %>% 
  summarise_all(nlevels) %>% 
  gather (category, number_of_levels)


# Q3. Use top_n() to print out the 3 rows with the highest number of factor levels.

number_of_levels %>% 
  top_n(3, number_of_levels)

# Q4. Filtering for the variable CurrentJobTitleSelect, pull the number of levels it has.

number_of_levels %>% 
  filter(category == "CurrentJobTitleSelect") %>%
  pull(number_of_levels)