
plot_panels <- function(wind_dat) {
  meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
    filter(exclude==0)
  wind_dat <- wind_dat %>% filter(iso %in% meta_dat$iso)
  
  # Make the plot
  plot <- ggplot(wind_dat, aes(x = year, y = gid, fill = windsp_ms)) +
    geom_tile(color="white") +
    scale_fill_gradientn(
      colors = c("white", "#2166AC", "#B2182B"),
      values = scales::rescale(c(0, 40, 80)),
      limits = c(0, 75),
      na.value = "grey80",
    ) +
    labs(x = "Year", y = "Subnational region",
         fill = "Avg. windspeed (m/s)") +
    theme_minimal() +
    scale_x_continuous(limits=c(1979, 2016), expand = c(0, 0),
                       breaks = seq(1980, 2015, by=5)) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.ticks.x = element_line(color = "black", linewidth = 0.5),
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "top",
      legend.title = element_text(size = 12, color = "black", face = "bold"),
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.y = element_blank()
    ) +
    # theme(axis.text.x = element_text(angle = 90, hjust = 1),
    #       axis.text.y = element_blank(),
    #       axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    #       panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    #       panel.grid.major = element_blank(),
    #       legend.position = "top", 
    #       legend.justification = "center") +
    facet_wrap(~country, scales = "free_y", ncol=4)
  ggsave("figures/panel_plots.jpeg", height=10, width=8, plot=plot)
}



