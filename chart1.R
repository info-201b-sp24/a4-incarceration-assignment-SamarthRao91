
library(dplyr)
library(tidyverse)
library(ggplot2)

national_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-jail-pop.csv")
washington_data <- national_data %>% 
  filter(state != 'WA') %>% 
  tail(151881)




urban_city_data <- washington_data %>% 
  filter(urbanicity == "urban" | urbanicity == "suburban") %>% 
  select(year, white_jail_pop, black_jail_pop) %>% 
  group_by(year) %>%  
  summarise(black_total = mean(black_jail_pop , na.rm = TRUE), white_total = mean(white_jail_pop , na.rm = TRUE)) %>% 
  mutate(urban_black_minus_white = (black_total - white_total))%>% 
  select(year, urban_black_minus_white) 

rural_city_data <- washington_data %>% 
  filter(urbanicity == "rural" | urbanicity == "small/mid") %>% 
  select(year, white_jail_pop, black_jail_pop) %>% 
  group_by(year) %>%  
  summarise(black_total = mean(black_jail_pop , na.rm = TRUE), white_total = mean(white_jail_pop , na.rm = TRUE)) %>% 
  mutate(rural_black_minus_white = (black_total - white_total)) %>% 
  select(year, rural_black_minus_white) 

combined_set <- full_join(rural_city_data, urban_city_data)

combined_set <- combined_set %>% 
  select(year, rural_black_minus_white, urban_black_minus_white)

colors <- c("Urban" = "blue", "Rural" = "red")


chart_1 <- ggplot() +
  geom_line(data = combined_set, aes(x = year, y = urban_black_minus_white, color = "Urban")) +
  geom_line(data = combined_set, aes(x = year, y = rural_black_minus_white, color = "Rural"))  + 
  labs(title = "Difference in Black/White Incarceration Rate Over the Years in an Average Urban Town vs. Average Rural Town (WA))",
       x = "Years",
       y = "Difference in Incarceration Rate (Black - White)",
       color = "Legend") +
       scale_color_manual(values = colors) 
       
