###############################################################
#       Data Visualization  Challenge 01 Chapter 05           #
#                                                             #
#   This chapter visualize Covid 19 data, based on an CSV     #
###############################################################

# Load libraries 

  library(tidyverse)
  library(lubridate)
  library(dplyr)

# get data from opendata

  covid19_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# summarize the same content 

  covid19_data_tbl                                                      %>% 
    mutate(across(countriesAndTerritories, str_replace_all, "_", " "))  %>%
    mutate(countriesAndTerritories = case_when(
      countriesAndTerritories == "USA" ~ "United States of America",
      countriesAndTerritories == "Czechia" ~ "Czech Republic",
      countriesAndTerritories == "UK" ~ "United Kingdom",
      TRUE ~ countriesAndTerritories))


# Manipulate data in a new table
  cumulative_covid_cases_tbl <- covid19_data_tbl              %>%
  mutate(date = dmy(dateRep))                                 %>%
  #sort by date
  arrange(date)                                               %>%
  filter(countriesAndTerritories %in% c("Germany","Spain", "France", "United_Kingdom", "United_States_of_America")) %>%
  filter(year == "2020")                                      %>%
  group_by(countriesAndTerritories)                           %>%
  mutate(cumulative_cases = cumsum(cases))                    %>%
  ungroup()


# Graphical output

  cumulative_covid_cases_tbl                                    %>%

# x-axis -> date; y-axis -> Cumulative cases   
  ggplot(aes(date,cumulative_cases), color = countriesAndTerritories) +

  geom_line(aes(x     = date,
                y     = cumulative_cases,
                color = countriesAndTerritories),
                size = 1.2) + 

    
  geom_label(aes(label = cumulative_cases),
             size  = 5,
             nudge_x  = -30,
             nudge_y  = 5,
             fill  = "purple",
             color = "white",
             fontface = "italic",
             data = filter(cumulative_covid_cases_tbl,date == max(date) & cumulative_cases == max(cumulative_cases)))+
  
  
  
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "lates status 2020",
    x = "2020",
    y = "Cumulative cases",
    # Legend
    color = "countries/continents" )+
    
    
  theme_light() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(face = "bold.italic"),
      plot.background = element_blank(),
      axis.title = element_text(size=14, face = "bold"),
      legend.position = "bottom")



















    