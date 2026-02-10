# Loading package
library(tidyverse)

# Reading gpx data ----
extract_gpx_stats <- function(gpx_file) {
  gpx <- sf::st_read(gpx_file, layer = "track_points", quiet = TRUE)
  
  date <- str_extract(gpx_file, "[0-9]+") %>% ymd()
  
  route <- gpx %>% 
    sf::st_combine() %>% 
    sf::st_cast("LINESTRING")
  
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
all <- list.files("data/gpx", full.names = T) %>% 
  map_df(~extract_gpx_stats(.x))

# Icon ----
n <- nrow(all)

weekly_average <- all %>% 
  mutate(week = floor_date(date, "week")) %>%
  count(week) %>% 
  summarise(avg_entries = mean(n)) %>%
  pull(avg_entries)

km <- all %>% 
  summarise(km = sum(km)) %>% 
  pull(km)

elevation <-
  all %>% 
  as_tibble() %>% 
  select(up, down) %>% 
  summarise(
    up = sum(up),
    down = sum(down),
    )
# Jauge don ----
fig <-
data.frame(
  name = "Montant",
  type = c("don", "rate"),
  value = c(n, weekly_average * 53 - n)
) %>% 
  ggplot(aes(name, value)) +
  geom_col(aes(alpha = type), fill = "gold", position = position_stack(reverse = T)) +
  scale_alpha_manual(values = c(1, .2)) +
  geom_hline(yintercept = 100) +
  annotate("text", label = "Objectif (100$)", x = 1, y = 105) +
  scale_y_continuous(
    limits = c(0,120),
    n.breaks = 12,
    labels = ~paste0(.x, "$")
  ) +
  labs(x = "", y = "") +
  guides(alpha = "none") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

# all %>% 
#   as_tibble() %>% 
#   mutate(
#     month = month(date, label = T, abbr = F),
#     # week = week(date)
#     ) %>%  
#   summarise(n = n(), .by = c(month)) %>%  
#   complete(month, fill = list(1:12)) %>% 
#   mutate(rate = mean(sum(n, na.rm = T), na.tm = T) *, .by = month)
#   
#   ggplot(aes(month)) +
#   geom_col(aes(y = n), fill = "gold") +
#   geom_col(aes(y = rate), fill = "gold", alpha = .2) +
#   # geom_col(fill = "gold") +
#   geom_text(aes(y = n, label = paste0(n, "$")), vjust = -1) +
#   geom_abline(slope = 6, linetype = 2) +
#   annotate("text", label = "Tendance moyenne", x = 11 , y = 80) +
#   geom_hline(yintercept = 100) +
#   annotate("text", label = "Objectif (100$)", x = 11, y = 105) +
#   
#   scale_y_continuous(
#     limits = c(0,120),
#     n.breaks = 12,
#     labels = ~paste0(.x, "$")
#   ) +
#   
#   labs(x = "", y = "") +
#   theme_bw() +
#   theme(panel.grid.major.x = element_blank())


# Trajet ----
library(sf)
library(leaflet)

## Function for calculating routes ----
segmentize_routes <- function(all, max_len = 10) {
  all %>%
    st_transform(3857) %>%
    st_segmentize(dfMaxLength = max_len) %>%
    st_cast("LINESTRING") %>%
    mutate(seg_id = row_number())
}
build_segment_grid <- function(segments, buffer = 50, cellsize = 10) {
  roi <- st_union(segments) |> st_buffer(buffer)
  
  grid <- st_make_grid(roi, cellsize = cellsize, square = TRUE)
  grid <- st_sf(
    grid_id = seq_len(length(grid)),
    geometry = grid
  )
  
  list(
    segments = segments,
    grid = grid
  )
}
compute_segment_density <- function(grid_segments) {
  segments <- grid_segments$segments
  grid     <- grid_segments$grid
  
  seg_grid <- st_intersection(segments, grid)
  
  grid_weights <- seg_grid %>%
    count(grid_id, name = "weight")
  
  weighted_segments <- seg_grid %>%
    left_join(st_drop_geometry(grid_weights), by = "grid_id") %>%
    st_transform(4326)
  
  return(weighted_segments)
}

## Caching workflow for computing only when added route ----
make_signature <- function(all, max_len, buffer, cellsize) {
  list(
    dates    = sort(all$date),
    n        = nrow(all),
    max_len  = max_len,
    buffer   = buffer,
    cellsize = cellsize
  )
}

sig_path <- "data/weighted_routes_signature.rds"
res_path <- "data/weighted_routes.rds"

signature <- make_signature(all, 10, 50, 10)

if (
  !file.exists(sig_path) ||
  !file.exists(res_path) ||
  !identical(signature, readRDS(sig_path))
) {
  
  message("Recomputing weighted routes")
  
  weighted_routes <-
    segmentize_routes(all, max_len = 10) %>%
    build_segment_grid(buffer = 50, cellsize = 10) %>%
    compute_segment_density()
  
  saveRDS(weighted_routes, res_path)
  saveRDS(signature, sig_path)
  
} else {
  message("Using cached weighted routes")
  weighted_routes <- readRDS(res_path)
}

## Creating route heatmap ----
pal <- colorNumeric(
  palette = c("orange","red", "darkred"),
  domain = weighted_routes$weight,
)

heatmap <-
weighted_routes %>% 
  leaflet() %>% 
  # addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addPolylines(
    color = ~pal(weight),
    weight = ~scales::rescale(weight, c(2,6)),
    opacity = ~scales::rescale(weight, c(.3,1))
  ) #%>% 
  # addLegend(
  #   pal = pal,
  #   values = ~weight,
  #   title = "Route usage"
  # )

# Calendrier ----


# Step 1: Create full date sequence for the month
year_start <- as.Date("2026-01-01")
year_end <- as.Date("2026-12-31")
all_dates <- data.frame(date = seq(year_start, year_end, by = "day"))

week_in_month <- function(date, week_start = 1) {
  stopifnot(inherits(date, "Date"))
  
  month_start <- lubridate::floor_date(date, "month")
  wday_start  <- lubridate::wday(month_start, week_start = week_start)
  days_w1     <- 8 - wday_start
  
  ifelse(
    lubridate::day(date) <= days_w1,
    1L,
    2L + (lubridate::day(date) - days_w1 - 1) %/% 7
  )
}

alex_cal <-
  all %>% 
  as_tibble() %>% 
  select(1:2) %>% 
  right_join(all_dates) %>% 
  mutate(
    day   = mday(date),
    month = month(date, label = TRUE, abbr = FALSE),
    wday  = wday(date, label = TRUE, week_start = 1),
    week = week_in_month(date)
  ) %>% 
  filter(as.numeric(month) <= month(Sys.Date())) %>% 
  
  ggplot(aes(wday, week, fill = km)) +
  facet_wrap(~month, ncol = 1, scales = "free") +
  geom_tile(color = "grey", linewidth = 0.4) +
  geom_text(aes(label = round(km, 1)), size = 3, color = "white") +
  scale_y_reverse(breaks = 1:5) +
  scale_fill_continuous(
    na.value = "grey100", palette = c("darkorange", "darkred")
  ) +
  theme_classic() +
  labs(x = "", y = "", fill = "km")
theme(
  legend.position = "top",
)
