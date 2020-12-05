###############################################################
#       Data Wrangling  Challenge 03 Chapter 04               #
#         Most innovative tech sector                         #
#         This chapter wrangle with patent data               #
###############################################################


# Load libraries 

  library(tidyverse)
  library(vroom)
  library(stringr)
  library(dplyr)
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
    patent_id = col_character(),
    mainclass_id =  col_character())
    
  
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

  uspc_tbl <- vroom(
    file       = "./00_data/uspc.tsv", 
    delim      = "\t", 
    col_types  = col_types,
    na         = c("", "NA", "NULL"))

  # additional opened this file for the translation from ID to a readable name
  mainclass_current_tbl <- vroom(
    file       = "./00_data/mainclass_current.tsv", 
    delim      = "\t", 
    col_types  = col_types,
    na         = c("", "NA", "NULL"))


  findten_tbl   <- tibble()
  overall_tbl    <- tibble()
  filterdTable  <- tibble()
  output_tbl  <- tibble()

  assignee_tbl <- assignee_tbl    %>%
    filter(type == 2 | type == 3)


# find the ten biggest companies
  findten_tbl <- assignee_tbl                                     %>%
    left_join(patent_assignee_tbl, by = c("id" = "assignee_id"))  %>%
    left_join(uspc_tbl, by = c("patent_id" = "patent_id"))        %>%
    group_by(organization)                                        %>%
    
      summarise(count = n())                                      %>%
      arrange(desc(count))                                        %>%
    ungroup() %>%
# seperate the 10 best companies from the rest 
     slice(1:10)

  topten <- findten_tbl[,c("organization")]
  overall_tbl <- assignee_tbl                                     %>%
    left_join(patent_assignee_tbl, by = c("id" = "assignee_id"))  %>%
    left_join(uspc_tbl, by = c("patent_id" = "patent_id"))

  filterdTable <- subset(overall_tbl, overall_tbl$organization %in% c(topten[1,1],topten[2,1],topten[3,1],topten[4,1],topten[5,1],topten[6,1],topten[7,1],topten[8,1],topten[9,1],topten[10,1]))
  
  output_tbl <- filterdTable                                      %>%
  filter(!is.na(mainclass_id))                                    %>% # kick the n/a
    group_by(mainclass_id)                                        %>%
      summarise(count = n())                                      %>%
      arrange(desc(count))                                        %>%
    ungroup()                                                     %>%
  left_join(mainclass_current_tbl, by = c("mainclass_id" = "id")) %>%
  slice(1:10)

# generate readable output
  glimpse(output_tbl)
  kable(output_tbl)

















