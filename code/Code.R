# libraries
library(readxl)
library(tidyverse)
library(tibble)
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

# get DOB, age and gender
dob_data <- read_xlsx('data/JSA_masterlist.xlsx', sheet = 'DOB') %>% 
  janitor::clean_names() %>% 
  mutate(sex = na_if(sex, "NA")) %>% 
  mutate(dob = as.Date(as.numeric(dob),  origin = "1899-12-30")) %>% # windows original date
  mutate(id = stringr::str_sub(student_id, -4, -1 )) %>% 
  select(-student_id, -sex)

df_percentiles <- df_percentiles %>% 
  merge(dob_data, by = 'id') %>% 
  mutate(age = jsa_test_year - year(ymd(dob))) %>% 
  mutate_at(3:14, round, 1) %>% 
  distinct(id,.keep_all = TRUE)

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
  
  total_score <- 0
  
  for (criterion in names(sport_criteria)) {
    
    # extract numbers
    condition <- sport_criteria[[criterion]]
    athlete_value <- athlete_info[[criterion]]
    
    score <- 0
    
    # skip if athlete value is NA ((should fail criteria)
    if (is.na(athlete_value) || length(athlete_value) == 0) {
      return(-1)
    }
    
    # process condition
    if (grepl('>', condition)){
      threshold <- as.numeric(gsub('>', '', condition))
      if (athlete_value < threshold) {
        return(-1) # criteria failed, not eligible
      }
      score <- athlete_value - threshold
    }
    
    else if (grepl('<', condition)){
      threshold <- as.numeric(gsub('<', '', condition))
      
      if (athlete_value > threshold) {
        return(-1)
      }
      score <- threshold - athlete_value
    }
    
    else {
      if (athlete_value != condition) {
        return(-1)
      }
      
      score <- 0
    }
    total_score <- total_score + score
    
  }
  
  return(total_score) # athlete does not return FALSE in any step, all criterion passed and is eligible for sport
}


# test function
athlete_test <- df_percentiles %>% 
  select(id, percentile_Agility:percentile_Height, gender) %>% 
  filter(id == '870M')

match_criteria(athlete_test, criteria[['Bowling']])

### STEP 4: GET LIST OF SPORT ----

match_list <- function(input_df, input_criteria){
  
  match_by_sport <- list()
  match_by_athlete <- list()
  
  athlete_df <- input_df %>% 
    select(id, percentile_Agility:percentile_Height, gender)
  
  
  for (i in 1:nrow(athlete_df)) {
    
    athlete <- athlete_df[i, , drop = FALSE]
    athlete_id <- as.character(athlete$id)
    matched_sports_counter <- c() 
    
    for (sport in names(input_criteria)) {
      
      total_score <- match_criteria(athlete, input_criteria[[sport]]) # get score
      
      if (total_score > 0) { #if eligble,
        
        match_by_sport[[sport]] <- bind_rows(match_by_sport[[sport]], tibble(id = athlete_id,
                                                                             score = round(total_score,2)))
          
          #add in id and total score (running list)
        
        matched_sports_counter <- c(matched_sports_counter, sport) # add on sport into counter
        
        match_by_athlete[[athlete_id]] <-  bind_rows(match_by_athlete[[athlete_id]], tibble(sport = sport,
                                                                                     score = round(total_score,2)))
      }
      
    }
    if (length(matched_sports_counter) == 0) {
      
      match_by_sport$Other <- bind_rows(match_by_sport$Other, tibble( id = athlete$id, score = NA))
      
      match_by_athlete[[athlete_id]] <-  bind_rows(match_by_athlete[[athlete_id]], tibble(sport = 'Other',
                                                                                          score = NA))
    }
  }
  
  
  list(match_by_sport = match_by_sport,
       match_by_athlete = match_by_athlete)
}

### APPLY FUNCTION
match_data <- match_list(df_percentiles, criteria)

### FOR OTHERS REDUCE TRESHOLD BY 15%
criteria_df_others <- criteria_df %>% 
  mutate(across(c(percentile_Height:percentile_Balance),  
                ~ str_replace_all(., "\\d+", \(x) as.character(as.integer(x) - 15))))
                             
criteria_split_others <- split(criteria_df_others, criteria_df_others$sport) %>% 
  map( ~ .x %>% select_if(~ !(is.na(.))))

criteria_others <- lapply(criteria_split_others, function(df) {
  df <- df[-1]
  df <- lapply(df, function(x) x[!is.na(x)])
  as.list(df)
})

others_df <- match_data[['match_by_sport']][['Other']] 

others_df_merged <- others_df %>% 
  merge(df_percentiles, by= 'id') %>% 
  select(-score)


# rerun the function 

match_data_others <- match_list(others_df_merged, criteria_others)
