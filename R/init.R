# Loading package
library(tidyverse)

# Reading gpx data ----
extract_gpx_stats <- function(gpx_file) {
  gpx <- sf::st_read(gpx_file, layer = "track_points", quiet = TRUE)
  
  date <- str_extract(gpx_file, "[0-9]+") %>% ymd()
  
  route <- gpx %>%
    sf::st_combine() %>%
    sf::st_cast("LINESTRING") #%>%
  # sf::st_transform(3857)
  
  km <- route %>% 
    sf::st_length() %>% 
    units::set_units("km") %>% 
    as.numeric()
  
  ele <- gpx %>% 
    mutate(
      diff = ele - lag(ele)
    ) %>% 
    summarise(
      up = sum(diff[diff > 0], na.rm = TRUE),
      down = sum(diff[diff < 0], na.rm = TRUE)
    )
  
  stats <-tibble(
    date = date,
    km = km,
    up = ele$up,
    down = ele$down
  )
  
  route <- sf::st_sf(
    tibble(date = date),
    geometry = route
  )
  
  out <- list(
    stats = stats,
    route = route
  )
  
  return(out)
}

files <- list.files("data/gpx", full.names = TRUE)
res <- purrr::map(files, extract_gpx_stats)
stats  <- purrr::list_rbind(purrr::map(res, "stats"))
routes <- dplyr::bind_rows(purrr::map(res, "route"))