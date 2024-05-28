library(dplyr)
library(tidyverse)


national_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-jail-pop.csv")
washington_data <- national_data %>% 
  filter(state != 'WA') %>% 
  tail(151881)


highest_diff_urban <- washington_data %>% 
  filter(urbanicity == "urban" | urbanicity == "suburban") %>% 
  select(year, county_name, white_jail_pop, black_jail_pop) %>% 
  filter(year == max(year)) %>% 
  mutate(diff = (black_jail_pop - white_jail_pop)) %>% 
  filter(diff == max(diff, na.rm = TRUE)) %>% 
  pull(county_name, diff)

highest_diff_urban_val <- washington_data %>% 
  filter(urbanicity == "urban" | urbanicity == "suburban") %>% 
  select(year, county_name, white_jail_pop, black_jail_pop) %>% 
  filter(year == max(year)) %>% 
  mutate(diff = (black_jail_pop - white_jail_pop)) %>% 
  filter(diff == max(diff, na.rm = TRUE)) %>% 
  pull(diff)
 


highest_diff_rural <- washington_data %>% 
  filter(urbanicity == "rural" | urbanicity == "small/mid") %>% 
  select(year, county_name, white_jail_pop, black_jail_pop) %>% 
  filter(year == max(year)) %>% 
  mutate(diff = (black_jail_pop - white_jail_pop)) %>% 
  filter(diff == max(diff, na.rm = TRUE)) %>% 
  pull(county_name, diff)

highest_diff_rural_val <- washington_data %>% 
  filter(urbanicity == "rural" | urbanicity == "small/mid") %>% 
  select(year, county_name, white_jail_pop, black_jail_pop) %>% 
  filter(year == max(year)) %>% 
  mutate(diff = (black_jail_pop - white_jail_pop)) %>% 
  filter(diff == max(diff, na.rm = TRUE)) %>% 
  pull(diff)


state_with_highest_diff <- national_data %>% 
  filter(year == max(year)) %>% 
  mutate(diff = (black_jail_pop - white_jail_pop))%>% 
  group_by(state) %>% 
  summarize(sum = sum(diff , na.rm = TRUE)) %>% 
  filter(sum == max(sum, na.rm = TRUE)) %>% 
  pull(state, sum)
  

state_with_lowest_diff <- national_data %>% 
  filter(year == max(year)) %>% 
  mutate(diff = (black_jail_pop - white_jail_pop))%>% 
  group_by(state) %>% 
  summarize(sum = sum(diff , na.rm = TRUE)) %>% 
  filter(sum == min(sum, na.rm = TRUE)) %>% 
  pull(state, sum)

  
  
  
 
  
  