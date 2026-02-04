km_dons <- data.frame(
  month = fct_inorder(month.abb),
  dons = c(1637, rep(NA, 11)),
  km = c(428.5, rep(NA, 11))
)


dons_bar <- 
  km_dons %>% 
  ggplot(aes(month, dons)) +
  geom_col(fill = "gold", alpha = .8) +
  geom_text(aes(label = paste0(dons, "$")), vjust = -1) +
  scale_y_continuous(
    limits = c(0, 5000),
    n.breaks = 10,
    labels = ~paste0(.x, "$")
  ) +
  labs(x = "", y = "") +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line("grey")
  )

km_line <-
  km_dons %>% 
  ggplot(aes(month, km)) +
  geom_point() + 
  geom_hline(yintercept = 4000) +
  annotate("text", label = "Objectif 4000 km", x = 11, y = 4200) +
  scale_y_continuous(
    limits = c(0, 5000),
    n.breaks = 10,
    labels = ~paste(.x, "km")
  ) +
  labs(x = "", y = "") +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line("grey")
  )

ggpubr::ggarrange(dons_bar, km_line, ncol = 2)