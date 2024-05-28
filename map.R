library(maps)
library(mapproj)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(usdata)

national_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-jail-pop.csv")
states_vector <- national_data %>% 
  pull(state)

states_vector <- abbr2state(states_vector)
states_vector <- tolower(states_vector)

national_data <- cbind(national_data, region = states_vector)

national_map_data <- national_data %>% 
  filter(year == max(year)) %>% 
  mutate(diff = (black_jail_pop - white_jail_pop))%>% 
  group_by(region) %>% 
  summarize(sum = sum(diff , na.rm = TRUE)) 
  
state_shapes <- map_data("state")
shape_national_combined <- merge(state_shapes, national_map_data, sort = FALSE, by = "region")
shape_national_combined <- shape_national_combined[order(shape_national_combined$order),]

differnce_heat_map <- ggplot(data = shape_national_combined) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = sum, color = "black")) +
  scale_fill_continuous(low = 'white', high = 'blue') +
  coord_map() + 
  labs(title = "U.S.Chloropeth Map of Total Black Minus White People Incarcerated In the United States In 2018" , x = "", y = "", fill = "Total Black People Incarcerated -  Total White People Incarcerated") +
  theme( plot.title = element_text(size = 16, face = "bold"),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 
