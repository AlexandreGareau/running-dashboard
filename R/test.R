source("R/init.R")
source("R/solar.R")
# cumulatif don ----
stats %>%
  mutate(
    month = month(date, label = T, abbr = F),
    # week = week(date)
  ) %>%
  summarise(n = n(), .by = c(month)) %>%
  complete(month, fill = list(1:12)) %>% 
  mutate(cum = cumsum(n)) %>% 
  # mutate(rate = mean(sum(n, na.rm = T), na.tm = T) *, .by = month)
  
  ggplot(aes(month)) +
  geom_col(aes(y = n), fill = "gold") +
  geom_line(aes(y = cum, group = 1), fill = "gold") +
  geom_point(aes(y = cum)) +
  # geom_text(aes(y = cum, label = paste0(cum, "$")), vjust = -1) +
  
  geom_hline(yintercept = 100) +
  annotate("text", label = "Objectif (100$)", x = 11, y = 105) +
  
  scale_y_continuous(
    limits = c(0,120),
    n.breaks = 12,
    labels = ~paste0(.x, "$")
  ) +
  
  labs(x = "", y = "") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

# elevation stack ----

list.files("data/gpx", full.names = T) %>%
  map_df(function(gpx_file) {
    gpx <- sf::st_read(gpx_file, layer = "track_points", quiet = TRUE)
    
    date <- str_extract(gpx_file, "[0-9]+") %>% ymd()
    
    gpx %>%
      sf::st_drop_geometry() %>%
      mutate(date = date)
  }) %>% 
  select(date, track_seg_point_id, ele) %>% 
  ggplot() +
  geom_line(aes(track_seg_point_id, ele, group = date, alpha = date), color = "#B58900") +
  labs(x = "", y = "Élévation (m)") +
  theme_solar()