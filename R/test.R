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


year_start <- as.Date("2026-01-01")
year_end <- as.Date("2026-12-31")
all_dates <- data.frame(date = seq(year_start, year_end, by = "day"))

stats %>% 
  right_join(all_dates) %>% 
  arrange(date) %>% 
  filter(date <= Sys.Date()) %>% 
  mutate(
    across(2:4, ~replace(.x, is.na(.x), 0)),
    cum_km = cumsum(km)
  ) %>% 
  
  ggplot(aes(date)) +
  geom_col(aes(y = km)) +
  geom_line(aes(y = cum_km))


Step 1: Create full date sequence for the month
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

# alex_cal <-
#   all %>% 
#   as_tibble() %>% 
#   select(1:2) %>% 
#   right_join(all_dates) %>% 
#   mutate(
#     day   = mday(date),
#     month = month(date, label = TRUE, abbr = FALSE),
#     wday  = wday(date, label = TRUE, week_start = 1),
#     week = week_in_month(date)
#   ) %>% 
#   filter(as.numeric(month) <= month(Sys.Date())) %>% 
#   
#   ggplot(aes(wday, week, fill = km)) +
#   facet_wrap(~month, ncol = 1, scales = "free") +
#   geom_tile(color = "grey", linewidth = 0.4) +
#   geom_text(aes(label = round(km, 1)), size = 3, color = "white") +
#   scale_y_reverse(breaks = 1:5) +
#   scale_fill_continuous(
#     na.value = "grey100", palette = c("darkorange", "darkred")
#   ) +
#   theme_classic() +
#   labs(x = "", y = "", fill = "km") +
#   theme(
#     legend.position = "top"
#     )