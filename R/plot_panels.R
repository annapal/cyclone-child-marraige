
wind_dat <- readRDS("data/wind_dat_all.rds")

# Make the plot
plot <- ggplot(wind_dat, aes(x = year, y = gid, fill = windsp_ms)) +
  geom_tile(color="white") +
  scale_fill_gradientn(
    colors = c("white", "#2166AC", "#B2182B"),
    values = scales::rescale(c(0, 40, 80)),
    limits = c(0, 75),
    na.value = "grey80",
  ) +
  labs(x = "Year", y = "Subnational region (GID code)",
       fill = "Avg. windspeed (m/s)") +
  theme_minimal() +
  scale_x_continuous(limits=c(1979, 2016), expand = c(0, 0),
                     breaks = seq(1980, 2015, by=5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(size = 3.5),
        # axis.text.y = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25),
        panel.grid.major = element_blank(),
        legend.position = "top", 
        legend.justification = "center") +
  facet_wrap(~country, scales = "free_y", ncol=6, nrow=4)
ggsave("figures/panel_plots1.jpeg", width = 9, height = 10, plot=plot)

