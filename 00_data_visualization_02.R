###############################################################
#       Data Visualization  Challenge 01 Chapter 05           #
#                                                             #
#   This chapter visualize Covid 19 data, based on an CSV     #
###############################################################

# Load libraries 

library(tidyverse)
library(ggplot2)
library(maps)


# get data from opendata

  covid19_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")


# summarize the same content  

  covid19_data_tbl <- covid19_data_tbl                                  %>% 
    mutate(across(countriesAndTerritories, str_replace_all, "_", " "))  %>%
    mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories))



  mortality_tbl <- covid19_data_tbl                                         %>%
  
# only compute relevant information
  select(countriesAndTerritories, deaths, popData2019, cases)               %>%
  group_by(countriesAndTerritories)                                         %>%
  summarize(population_2019 = mean(popData2019), deaths_sum = sum(deaths))  %>%
  mutate(`Mortality Rate [%]`  = 100 * deaths_sum / population_2019)        %>%
  ungroup()

  
# Generate world map
  world <- map_data("world")
  world <- left_join(world, mortality_tbl, by = c("region" = "countriesAndTerritories"))
  world <- select(world, -c("population_2019","deaths_sum"))

# plot relevant data
  ggplot() + 
  geom_polygon(data = world,
               aes(x=long, y = lat, fill = `Mortality Rate [%]`, group = group))+ 
  
  coord_fixed(1.3) + 
  scale_fill_gradient(low='#EC4440',
                      high='#2F142C') +
  
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  
  
  labs(title = "Confirmed COVID-19 deaths relativ to the size of the population",
       subtitle = "More then 1.2 Million confirmed COVID-19 deaths worldwide")