# Loading package
library(tidyverse)

# Reading gpx data ----
extract_gpx_stats <- function(gpx_file) {
  gpx <- sf::st_read(gpx_file, layer = "track_points", quiet = TRUE)
  
  date <- str_extract(gpx_file, "[0-9]+") %>% ymd()
  
  route <- gpx %>%
    sf::st_combine() %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_transform(3857)
  
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

# Icon ----
n <- nrow(stats)
km_sum <- sum(stats$km)
ele_sum <- sum(stats$up)

# Heatmap ----
source("R/weighting_routes.R")
library(leaflet)
pal <- colorNumeric(
  # palette = c("orange","red", "darkred"),
  palette = c("#f37714", "#fe340d", "#de2315"),
  # palette = c("#12B8FF", "#1a9ad0", "#1780ad"),
  domain = weighted_routes$weight,
)

heatmap <-
weighted_routes %>% 
  leaflet(options = leafletOptions(minZoom = 12, maxZoom = 15)) %>% 
  # addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addPolylines(
    color = ~pal(weight),
    weight = ~scales::rescale(weight, c(2,6)),
    opacity = ~scales::rescale(weight, c(.3,1))
  ) %>%
addLegend(
  pal = pal,
  values = ~weight,
  title = "Route usage"
)

# Calendrier----
year_start <- as.Date("2026-01-01")
year_end <- as.Date("2026-12-31")
all_dates <- data.frame(date = seq(year_start, year_end, by = "day"))

trend <-
  stats %>% 
  right_join(all_dates) %>% 
  arrange(date) %>% 
  filter(date <= Sys.Date()) %>%
  mutate(
    icon = if_else(is.na(km), "❌", "👟"),
    day_trend = cummean(!is.na(km)) * 365
    ) %>% 
  
  # Plot
  ggplot(aes(date, day_trend, group = 1)) +
  geom_line() +
  geom_point(aes(colour = is.na(km))) +
  geom_text(aes(label = icon, colour = is.na(km), size = !is.na(km)), vjust = -.4) +
  geom_hline(yintercept = 100, linewidth = 1) +
  
  # Scale
  scale_colour_manual(
    name = "",
    labels = c("Jour de course", "Jour de patate"),
    values = c("darkred", "grey"),
    ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%B"
  ) +
  scale_y_continuous(
    name = "Contribution final en $",
    labels = ~paste0(.x, "$"),
    n.breaks = 10
  ) +
  
  # Theme
  guides(size = "none", colour = "none") +
  labs(x = "", title = "Contribution final si la tendance journalière se maintien") +
  theme_classic() +
  theme(
    legend.position = "top", legend.direction = "vertical", legend.justification = "left",
    panel.grid.major.x = element_line("black"),
    panel.grid.major.y = element_line("grey", linetype = 2)
  )



# elevation stack ----

list.files("data/gpx", full.names = T) %>%
  map_df(function(gpx_file) {
    gpx <- sf::st_read(gpx_file, layer = "track_points", quiet = TRUE)

    date <- str_extract(gpx_file, "[0-9]+") %>% ymd()

    gpx %>%
      st_drop_geometry() %>%
      mutate(date = date)
  }) %>%



  ggplot() +
  geom_line(aes(track_seg_point_id, ele, group = date)) +
  theme_bw()


