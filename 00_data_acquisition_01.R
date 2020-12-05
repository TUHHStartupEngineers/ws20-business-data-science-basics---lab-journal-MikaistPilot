###############################################################
#       Data Acquisition  Challenge 01 Chapter 03             #
#                         Web API                             #
#         This chapter wrangle with Data Acquisition          #
###############################################################

# Load libraries 

  library("httr")
  library("jsonlite")
  library(tidyverse)
  library(knitr)

#collect API data
  res = GET("https://official-joke-api.appspot.com/jokes/ten")
  APIdata = fromJSON(rawToChar(res$content))

# generate a nice table
  #glimpse (APIdata)
  kable(APIdata)