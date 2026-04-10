source("R/init.R")

# Icon ----
n <- nrow(stats)
km_sum <- sum(stats$km)
ele_sum <- sum(stats$up)
# Heatmap ----
source("R/weighting_routes.R")
library(leaflet)
# library(leafgl)

pal <- colorNumeric(
  # palette = c("orange","red", "darkred"),
  # palette = c("#f37714", "#fe340d", "#de2315"),
  palette = c("#B58900"),
  # palette = c("#12B8FF", "#1a9ad0", "#1780ad"),
  domain = weighted_routes$weight,
)

heatmap <-
weighted_routes %>% 
  leaflet(options = leafletOptions(minZoom = 12, maxZoom = 15)) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  # addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolylines(
    color = ~pal(weight),
    weight = ~scales::rescale(weight, c(2,6)),
    opacity = ~scales::rescale(weight, c(.3,1))
  )

# Cumulatif ---
# cumulative stats ----
year_start <- as.Date("2026-01-01")
year_end <- as.Date("2026-12-31")
all_dates <- data.frame(date = seq(year_start, year_end, by = "day"))

cum_df <-
  stats %>% 
  select(-down) %>% 
  right_join(all_dates) %>% 
  arrange(date) %>% 
  filter(date <= Sys.Date()) %>% 
  pivot_longer(
    cols = -date,
    names_to = c("metric")
  ) %>% 
  mutate(
    .by = metric,
    cum = cumsum(replace(value, is.na(value), 0)),
    scale_factor = max(cum) / max(value, na.rm = T),
    cum_scaled = cum / scale_factor
  )

scale_factor <- data.frame(km = unique(cum_df$scale_factor)[1], up = unique(cum_df$scale_factor)[2])

source("R/solar.R")
cum_plot <- function(col) {
  if (col == "km") {
    metric_label <- "Kilomètre (km)"
    unit <- "km"
    daily_break <- seq(0,20,1)
    cum_break <- seq(0,1000,10)
  } else if (col == "up") {
    metric_label <- "Élévation (m)"
    unit <- "m"
    daily_break <- seq(0,500, 10)
    cum_break <- seq(0,3000,100)
  } 
  
  
  # plot
  cum_df %>% 
    filter(metric == col) %>%
    mutate(metric = metric_label) %>% 
    ggplot(aes(date)) +
    facet_wrap(~metric) +
    # Daily bars
    geom_col(
      aes(y = value),
      fill = "#2AA198", color = "#2C2721",
      alpha = 0.6,
    ) +
    # Cumulative line (already scaled per metric)
    geom_step(
      aes(y = cum_scaled),
      color = "#B58900",
      linewidth = 1
    ) +
    # Points only when activity exists
    geom_point(
      aes(y = dplyr::if_else(value != 0, cum_scaled, NA)),
      color = "#B58900",
      size = 2
    ) +
    scale_y_continuous(
      name = paste(metric_label, "/ par course"),
      labels = scales::label_number(suffix = unit),
      breaks = daily_break,
      sec.axis = sec_axis(
        name = "Cumulatif",
        labels = scales::label_number(suffix = unit),
        breaks = cum_break,
        ~ . * scale_factor[[col]],
      )
    ) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%B"
    ) +
    labs(x = "") +
    theme_solar() +
    theme(
      strip.text = element_text(face = "bold", color = "#EEE8D5", size = 14)
    )
}
cum_km <- cum_plot("km")
cum_up <- cum_plot("up")

## Money pace ----
money_pace_df <-
  stats %>% 
  right_join(all_dates) %>% 
  arrange(date) %>% 
  filter(date <= Sys.Date()) %>%
  mutate(
    icon = if_else(is.na(km), "x", "👟"),
    nudge = if_else(is.na(km), 1, 5),
    day_trend = cummean(!is.na(km)) * 365
  )

  # Plot
money_pace <- 
money_pace_df %>% 
  ggplot(aes(date, day_trend, group = 1)) +
  geom_line(color = "#2AA198", linewidth = 1) +
  geom_point(aes(color = is.na(km))) +
  geom_text(aes(label = icon, nudge_y = nudge, color = is.na(km), size = !is.na(km)), show.legend = F) +
  geom_hline(yintercept = 100, linewidth = 1, color = "#B58900") +
  annotate("text", x = Sys.Date(), y = 110, label = "Objectif 100$", color = "#B58900") +
  
  # Scale
  scale_colour_manual(
    name = "",
    labels = c("Jour de course", "Jour de patate"),
    values = c("#CB4B16", "#93A1A1"),
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
  guides(size = "none", color = "none") +
  labs(x = "", title = "") +
  theme_solar()
