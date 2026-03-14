solar_cols <- c(
  bg        = "#002B36",
  bg_light  = "#073642",
  text      = "#93A1A1",
  text_light= "#EEE8D5",
  primary   = "#B58900",
  secondary = "#CB4B16",
  accent    = "#2AA198",
  grid      = "#586E75"
)

library(ggplot2)

theme_solar <- function(base_size = 12, base_family = "") {
  
  solar_cols <- list(
    bg        = "#002B36",
    bg_light  = "#073642",
    text      = "#93A1A1",
    text_light= "#EEE8D5",
    primary   = "#B58900",
    grid      = "#586E75"
  )
  
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background  = element_rect(fill = solar_cols$bg, color = NA),
      panel.background = element_rect(fill = solar_cols$bg, color = NA),
      
      text = element_text(color = solar_cols$text),
      
      axis.text  = element_text(color = solar_cols$text),
      axis.title = element_text(color = solar_cols$text_light),
      
      plot.title = element_text(
        color = solar_cols$text_light,
        face = "bold"
      ),
      
      panel.grid.major = element_line(color = solar_cols$grid, linewidth = .3),
      panel.grid.minor = element_blank(),
      
      legend.background = element_rect(fill = solar_cols$bg),
      legend.key = element_rect(fill = solar_cols$bg)
    )
}

scale_color_solar <- function() {
  scale_color_manual(values = c(
    "#B58900",
    "#CB4B16",
    "#2AA198",
    "#859900",
    "#268BD2"
  ))
}