###############################################################
#       Data Acquisition  Challenge 01 Chapter 02             #
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



  sales_by_state <- bikes_loc_edit_tbl                          %>%
    select(total.price,state)                                   %>%
    group_by(state)                                             %>%
      summarize(sales = sum(total.price))                       %>%
      mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                   decimal.mark = ",", 
                                   prefix = "", 
                                   suffix = " €"))              %>%
      arrange(desc(sales))                                      %>%
    ungroup()


# plot data
  
  ggplot(sales_by_state,aes(x = reorder (state,-sales), y = sales)) +
  
  geom_col(fill = "darkred") + 
  geom_label(aes(label = sales_text)) + 

  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Income per state",
    subtitle = "Income in descending order",
    x = "", # Override defaults for x and y
    y = "Income"
  )

