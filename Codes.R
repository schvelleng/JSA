# libraries
library(readxl)
library(tidyverse)

### STEP 1: Import data ----

# import files and clean
df_height <- read_xlsx('data/JSA_masterlist.xlsx', sheet = 'Height and weight') %>% 
  janitor::clean_names() %>% 
  mutate(id = stringr::str_sub(student_id, -4, -1 )) %>% # add last 4 digits identifier
  select(id, everything()) %>% 
  select(-student_id) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  rename('jsa_test_year' = 'year')

df_trials <- read_xlsx('data/JSA_masterlist.xlsx', sheet = 'JSA Selection') %>% 
  rename('ID' = 'Column1') %>% 
  janitor::clean_names() %>% 
  select(-student_id) %>% 
  group_by(id, jsa_item) %>% 
  slice(1) %>% 
  ungroup() 

# convert to long
df_height_long <- df_height %>% 
  select(-weight, -level, -jsa_test_year) %>% 
  pivot_longer(!id, names_to = 'jsa_item', values_to = 'jsa_score') %>% 
  mutate(jsa_item = str_to_title(jsa_item))

# merge height and trial data 
df_all <- df_trials %>% 
  bind_rows(df_height_long) %>% 
  mutate(jsa_score = as.numeric(jsa_score),
         jsa_test_year = first(na.omit(jsa_test_year)))

str(df_all)

# get percentile ranks
df_percentiles <- df_all %>% 
  mutate(jsa_score = case_when(jsa_item %in% c('Sprint', 'Agility') & jsa_score == 0 ~ NA,
                               TRUE ~ jsa_score)) %>% 
  group_by(jsa_item) %>% 
  mutate(percentile = case_when(jsa_item %in% c('Sprint', 'Agility') ~ (1 - percent_rank(jsa_score))*100,
                                TRUE~percent_rank(jsa_score)*100)) %>% 
  pivot_wider(names_from = jsa_item, values_from = c(percentile, jsa_score)) %>% 
  mutate(gender = sample(c("Male", "Female"), n(), replace = TRUE))


### STEP 2: Import criteria into list ----
criteria_df <- read_excel('data/JSA I DO.xlsx', sheet = 'Percentile Sport') %>% 
  janitor::clean_names() %>% 
  select(-torso, -lower_leg) %>% 
  rename(percentile_Height = height,
         percentile_Jump = vj,
         percentile_Agility = x505,
         percentile_Sprint = x20m,
         percentile_Balance = y_balance,
         percentile_Toss = alt_hand_wt)
         


criteria_split <- split(criteria_df, criteria_df$sport) %>% 
  map( ~ .x %>% select_if(~ !(is.na(.))))


criteria <- lapply(criteria_split, function(df) {
  df <- df[-1]
  df <- lapply(df, function(x) x[!is.na(x)])
  as.list(df)
})

# check
criteria[['Bowling']][['percentile_Balance']]
criteria[['Gymnastics']][['percentile_Toss']]


### STEP 3: FUNCTION FOR MATCHING CRITERIA ----

match_criteria <- function(athlete_info, sport_criteria) {
  
  for (criterion in names(sport_criteria)) {

    # extract numbers
    condition <- sport_criteria[[criterion]]
    athlete_value <- athlete_info[[criterion]]
    
    # skip if athlete value is NA (should not fail)
    if (is.na(athlete_value) || length(athlete_value) == 0) {
      return(FALSE)
    }
    
    # process condition
    if (grepl('>', condition)){
      treshold <- as.numeric(gsub('>', '', condition))
      
      if (athlete_value < treshold) return(FALSE) # criteria failed, not eligible
    }
    
    else if (grepl('<', condition)){
      treshold <- as.numeric(gsub('<', '', condition))
      
      if (athlete_value > treshold) return(FALSE)
    }
    
    else {
      if (athlete_value != condition) return(FALSE)
    }
  }
  
  return(TRUE) # athlete does not return FALSE in any step, all criterion passed and is eligible for sport
}


# test function
match_criteria(athlete, criteria[['Netball']])

### STEP 4: GET LIST OF SPORT ----
sport_matches <- list(Other = c())

athlete_df <- df_percentiles %>% 
  select(id, percentile_Agility:percentile_Height, gender)


for (i in 1:nrow(athlete_df)) {
  
  athlete <- athlete_df[i, , drop = FALSE]
  matched_sports <- c() 
  
  matched <- FALSE  
  
  for (sport in names(criteria)) {
    if (match_criteria(athlete, criteria[[sport]])) {
      sport_matches[[sport]] <- c(sport_matches[[sport]], athlete$id)
      matched_sports <- c(matched_sports, sport) # add on sport into vector
    }
  }
  
  if (length(matched_sports) == 0) {
    sport_matches$Other <- c(sport_matches$Other, athlete$id)
  }
}
  
### TEST 
athlete_test <- df_percentiles %>% 
  select(id, percentile_Agility:percentile_Height, gender) %>% 
  sample_n(size = 300)

sport_matches <- list(Other = c())

for (i in 1:nrow(athletes_test)) {
  
  athlete <- athletes_test[i, , drop = FALSE]
  matched_sports <- c() 
  
  matched <- FALSE  
  
  for (sport in names(criteria)) {
    if (match_criteria(athlete, criteria[[sport]])) {
      sport_matches[[sport]] <- c(sport_matches[[sport]], athlete$id)
      matched_sports <- c(matched_sports, sport) # add on sport into vector
    }
  }
  
  if (length(matched_sports) == 0) {
    sport_matches$Other <- c(sport_matches$Other, athlete$id)
  }
}

