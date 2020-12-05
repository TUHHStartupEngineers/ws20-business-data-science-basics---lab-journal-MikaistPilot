###############################################################
#       Data Acquisition  Challenge 02 Chapter 02             #
#                       Sale analysis                         #
#                           INTRO                             #
###############################################################

# Load libraries 

  library(tidyverse)
  library(readxl)
  library(lubridate)
  library("writexl")
  library(stringr)
  library(dplyr)
  library(tidyr)


#import the requested files

  ordering_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
  shops_tbl  <- read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
  bikes_tbl <- read_excel(path = "00_data/01_bike_sales/01_raw_data/bikes.xlsx")


# merge data to generate a database
  bikeSaleLocation_tbl <- ordering_tbl                          %>%
    left_join(bikes_tbl, by = c("product.id" = "bike.id"))      %>%
    left_join(shops_tbl, by = c("customer.id" = "bikeshop.id"))

  bikes_loc_edit_tbl <- bikeSaleLocation_tbl                    %>%
  separate("location",c("city","state"),", ")                   %>%
  mutate(total.price = price * quantity)                        %>%
  select(-order.line, -gender,-model.year,-frame.material,-weight,-category,-url,-lat,-lng)   




  salesPerStateAndYear_tbl <- bikes_loc_edit_tbl                %>%
    select(total.price,state,order.date)                        %>%
    mutate(year = year(order.date))                             %>%
    group_by(year, state)                                       %>%
      summarize(sales = sum(total.price))                       %>%
    ungroup()                                                   %>%
    mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

  
# plot data
  ggplot(salesPerStateAndYear_tbl, aes(x = year, y = sales, fill = state)) +
  
  geom_col() + 
  facet_wrap(~ state) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
# formating data  
  labs(
    title = "Income by year and state",
    subtitle = "statewise plot",
    fill = "States")


