library(ggplot2)
library(dplyr)

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
  select(urban_black_minus_white) 

rural_city_data <- washington_data %>% 
  filter(urbanicity == "rural" | urbanicity == "small/mid") %>% 
  select(year, white_jail_pop, black_jail_pop) %>% 
  group_by(year) %>%  
  summarise(black_total = mean(black_jail_pop , na.rm = TRUE), white_total = mean(white_jail_pop , na.rm = TRUE)) %>% 
  mutate(rural_black_minus_white = (black_total - white_total)) %>% 
  select( rural_black_minus_white) 

urban_and_rural <- cbind(urban_city_data$urban_black_minus_white, rural_city_data$rural_black_minus_white)
urban_and_rural <- as.data.frame(urban_and_rural) 

unique_sorted_urban_rural <- urban_and_rural %>% 
  distinct(V1, .keep_all =  TRUE) %>% 
  arrange(V1)

chart_2 <- ggplot() +
  geom_point(data = unique_sorted_urban_rural, aes(x = V1, y = V2),  color = "black", size = 3) +
  labs(title = "Relationship between difference of black and white incarcerations in urban vs. rural counties in WA",
       x = "Difference in Black and White Incarceration In Washington's Urban Cities",
       y = "Difference in Black and White Incarceration In Washington's Rural Cities") +
  theme( plot.title = element_text(size = 20, face = "bold"),
         axis.text.x=element_text(size = 16), 
         axis.text.y=element_text(size = 16) 
  )
        



