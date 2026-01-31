library(tidyverse)
library(sf)

extract_gpx_stats <- function(gpx_file) {
  gpx <- sf::st_read(gpx_file, layer = "track_points", quiet = TRUE)
  
  date <- str_extract(gpx_file, "[0-9]+") %>% ymd()
  
  route <- gpx %>% 
    sf::st_combine() %>% 
    sf::st_cast("LINESTRING")
    
  km <- route %>% 
    sf::st_length() %>% 
    units::set_units("km")
  
  ele <- gpx %>% 
    mutate(
      diff = ele - lag(ele)
    ) %>% 
    summarise(
      up = sum(diff[diff > 0], na.rm = TRUE),
      down = sum(diff[diff < 0], na.rm = TRUE)
    )
  
  stats <- sf::st_sf(
    tibble(
      date = date,
      km = km,
      up = ele$up,
      down = ele$down
    ),
    geometry = route
  )
  
  return(stats)
}

all <- list.files("data/trajet", full.names = T) %>% 
  map_df(~extract_gpx_stats(.x))

library(leaflet)
library(leaflet.extras)
all %>% 
  leaflet() %>%  
  # addTiles() %>% 
  # addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolylines(
    color = "red",
    weight = 3,
    opacity = 0.8
  )

# Route heatmap ----
segmentize_weighted_route <- function(all) {
  segments <- all %>%
    sf::st_transform(3857) %>%
    sf::st_segmentize(dfMaxLength = 10) %>%
    sf::st_cast("LINESTRING")
  
  roi <- st_union(segments) %>% st_buffer(50)

  grid <- st_make_grid(
    roi,
    cellsize = 10,
    square = TRUE
  ) %>%
    st_sf(grid_id = seq_len(length(.)))
  
  seg_grid <- st_intersection(
    segments %>%  mutate(seg_id = row_number()),
    grid
  )
  
  grid_weights <- seg_grid %>%
    count(grid_id, name = "weight")
  
  grid_weighted <- grid %>%
    left_join(
      st_drop_geometry(grid_weights),
      by = "grid_id"
    ) %>%
    mutate(weight = replace_na(weight, 0))
  
  segments_weighted <- seg_grid %>%
    left_join(
      st_drop_geometry(grid_weights),
      by = "grid_id"
    ) %>% 
    st_transform(4326)
  
  return(segments_weighted)
}
weighted_routes <- segmentize_weighted_route(all)

pal <- colorNumeric(
  palette = "plasma",
  domain = weighted_routes$weight,
  # reverse = TRUE,
)

weighted_routes %>% 
  leaflet() %>% 
  addProviderTiles(providers$Stadia.AlidadeSmoothDark) %>%
  addPolylines(
    color = ~pal(weight),
    weight = 4,
    opacity = 0.6
  ) %>% 
  addLegend(
    pal = pal,
    values = ~weight,
    title = "Route usage"
  )



# raster-like heatmap ----
all %>%
  st_transform(3857) %>%
  mutate(
    n = pmax(10, round(as.numeric(st_length(geometry)) / 25)),
    pts = purrr::map2(geometry, n, st_line_sample)
  ) %>%
  tidyr::unnest(pts) %>% 
  st_cast("POINT") %>% 
  st_transform(4326) %>% 
  
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  leaflet.extras::addHeatmap(
    lng = ~st_coordinates(geometry)[,1],
    lat = ~st_coordinates(geometry)[,2]
  )


pts <-
  all %>%
  mutate(
    pts = map(geometry, ~ st_line_sample(.x, density = 1))
  ) %>% 
  tidyr::unnest(pts)
  sf::st_cast("POINT") %>%
  sf::st_transform(4326)


  