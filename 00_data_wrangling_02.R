###############################################################
#       Data Wrangling  Challenge 02 Chapter 04               #
#         TOP 10 companies with the most patents in 2014      #
#         This chapter wrangle with patent data               #
###############################################################


# Load libraries 

  library(vroom)
  library(tidyverse)
  library(readxl)
  library(lubridate)
  library("writexl")
  library(stringr)
  library(dplyr)
  library(tidyr)
  library(knitr)

  col_types <- list(
    id = col_character(),
    type = col_character(),
    number = col_character(),
    country = col_character(),
    date = col_date("%Y-%m-%d"),
    abstract = col_character(),
    title = col_character(),
    kind = col_character(),
    num_claims = col_double(),
    filename = col_character(),
    withdrawn = col_double(),
    patent_id = col_character())

#open the downloaded documents 
  
  assignee_tbl <- vroom(
  file       = "./00_data/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL"))
  
patent_assignee_tbl <- vroom(
  file       = "./00_data/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL"))

patent_tbl <- vroom(
  file       = "./00_data/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL"))

# generate new table/database
overall_tbl <- tibble()

# merge single tables together in a database
assignee_tbl <- assignee_tbl %>%
  filter(type == 2)

overall_tbl <- assignee_tbl                                     %>%
  left_join(patent_assignee_tbl, by = c("id" = "assignee_id"))  %>%
  left_join(patent_tbl, by = c("patent_id" = "id"))             %>%
  mutate(year = year(date))                                     %>%
  filter(year == 2014)                                          %>%
  group_by(organization)                                        %>%
  summarise(count = n())                                        %>%
  arrange(desc(count))                                          %>%
  
  # cut off the table after the best 10 companies
  slice(1:10)

# print out the table in a readable view
  glimpse(overall_tbl)
  kable(overall_tbl)
  # glimpse(assignee_tbl)
  # glimpse(patent_assignee_tbl)
  