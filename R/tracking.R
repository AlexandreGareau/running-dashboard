library(tidyverse)
library(sf)
library(leaflet)

gpx <- st_read("data/trajet/20260102.gpx", layer = "track_points", quiet = T)

route <- gpx %>%
  # arrange(time) %>% 
  st_combine() %>% 
  st_cast("POINT")


leaflet(route) %>% 
  # addTiles() %>% 
  # addProviderTiles(providers$CartoDB.Positron) %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolylines(
    color = "#2C7FB8",
    weight = 4,
    opacity = 0.9
  )
