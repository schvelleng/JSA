# load package

library(tidyverse)
library(readxl)

# Turn criteria into list of list
criteria_df <- read_excel('data/JSA I DO.xlsx', sheet = 'Percentile Sport') %>% 
  janitor::clean_names()
  

criteria_split <- split(criteria_df, criteria_df$sport) %>% 
  map( ~ .x %>% select_if(~ !(is.na(.))))
  

criteria <- lapply(criteria_split, function(df) {
  df <- df[-1]
  df <- lapply(df, function(x) x[!is.na(x)])
  as.list(df)
})

# check
criteria[['Bowling']][['torso']]
criteria[['Gymnastics']][['height']]


# function for criteria match



