## Prep latitude and longitude data for leaflet in R

library(dplyr)
library(tidyr)

load("data/map_data_final.rds")

locations <- select(map_data_final,aid,Study_location,Study_country) %>% distinct()

Moz <- filter(locations, Study_country == "Mozambique") %>% distinct()
Indo <- filter(locations, Study_country == "Indonesia") %>% distinct()
Nepal <- filter(locations, Study_country == "Nepal") %>% distinct()
