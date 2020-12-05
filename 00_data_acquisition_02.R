###############################################################
#       Data Acquisition  Challenge 02 Chapter 03             #
#                         Web scrapper                        #
#         This chapter wrangle with Data Acquisition          #
###############################################################

# Load libraries 

  library(tidyverse)
  library(rvest)    
  library(xopen)     
  library(jsonlite) 
  library(glue)     
  library(stringi)  
  library(furrr)    
  library(htm2txt)

# scrap html data from certain categorie
    url_mtb_enduro <- "https://www.rosebikes.de/fahrr%C3%A4der/mtb/cross-country"
    xopen(url_mtb_enduro) 

# read html text of the website
  url_mtb_enduro         <- read_html(url_mtb_enduro)
  name_bike <- url_mtb_enduro                     %>%
    html_nodes(".catalog-category-bikes__title")  %>%
    str_remove_all(".*>")                         %>%
    str_remove_all("\n")
  
  price_bike <- url_mtb_enduro                          %>%
    html_nodes(".catalog-category-bikes__price-title")  %>%
    str_remove_all(".*>")                               %>%
    str_remove_all("\n")
  

  bikes_tbl <- tibble()
  bikes_tbl_names <- as_tibble(name_bike)
  bikes_tbl_names <- tibble::rowid_to_column(bikes_tbl_names, "ID")
# for debug
  # bikes_tbl_names


  bikes_tbl_prices <- as_tibble(price_bike)
  bikes_tbl_prices <- tibble::rowid_to_column(bikes_tbl_prices, "ID")
# for debug
  # bikes_tbl_prices



  bikes_tbl <- bikes_tbl_names                    %>%
    left_join(bikes_tbl_prices, by = c("ID" = "ID"))  %>%
# give the column uniques names  
    rename("name" = "value.x")                            %>%
    rename("price" = "value.y")



# save the requested rds data
  saveRDS(bikes_tbl, "bikes_tbl.rds")
  
# output table
  glimpse(bikes_tbl)
  
  
  
  