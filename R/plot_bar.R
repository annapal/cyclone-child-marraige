
plot_bar <- function(wind_dat, all_dat) {
  meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
    filter(exclude==0)
  
  p1 <- ggplot(data = wind_dat, aes(x = fct_rev(country), y = windsp_ms)) +
    geom_violin(fill = "darkblue", color = "darkblue", scale = "width") + # Separate violin for each country
    # scale_size_continuous(range = c(3, 10)) + # Adjust bubble size range
    labs(y = "Distribution of windspeed\nexposure (m/s)",
         x = NULL) + 
    coord_flip() +
    theme(
      panel.grid.major.y = element_line(color = "grey90", linewidth=0.5),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(linewidth = 0.25),
      axis.ticks.x = element_line(linewidth = 0.25),
      legend.key = element_blank(),
      plot.margin = margin(10, 10, 10, 3),
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(size = 8),
      axis.text.y = element_blank()
    )
  
  prob <- all_dat %>%
    filter(iso3 %in% meta_dat$iso) %>%
    group_by(iso3) %>%
    summarise(prob = weighted.mean(married, Denorm_Wt)) %>%
    mutate(rate = prob*10000,
           country = countrycode(iso3, "iso3c", "country.name"))
  
  p2 <- ggplot(prob, aes(x = fct_rev(country), y = rate)) +
    geom_bar(stat = "identity", fill="#7B3F8F") +
    labs(y = "Annual rate of child marriage\n(per 10,000)",
         x = NULL) + 
    coord_flip() +
    theme(
      panel.grid.major.y = element_line(color = "grey90", linewidth=0.5),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(linewidth = 0.25),
      axis.ticks.x = element_line(linewidth = 0.25),
      legend.key = element_blank(),
      plot.margin = margin(10, 10, 10, 3),
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(size = 8)
    )
  
  plot <- grid.arrange(p2, p1, ncol=2, widths=c(0.5, 0.5), padding = unit(0, "lines"))
  ggsave("figures/bar_plot.jpeg", plot = plot, height = 4)
}


