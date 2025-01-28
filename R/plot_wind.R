
plot_wind <- function(wind_dat) {
  
  summed_data <- wind_dat %>%
    group_by(iso, country, gid, level) %>%
    summarise(total_windsp_ms = sum(windsp_ms, na.rm = TRUE))
  
  wind_adm1 <- summed_data %>% filter(level=="Adm1")
  wind_adm2 <- summed_data %>% filter(level=="Adm2") 
  
  # Get polygons
  countries <- gadm(country_codes()$ISO3, level=0, path="data", version="3.6") %>%
    st_as_sf()
  adm1_reg <- gadm(unique(wind_adm1$iso), level=1, path="data", version="3.6") %>% st_as_sf()
  adm2_reg <- gadm(unique(wind_adm2$iso), level=2, path="data", version="3.6") %>% st_as_sf()
  
  # Merge wind data
  adm1_reg <- left_join(adm1_reg, wind_adm1, by = c("GID_1" = "gid"))
  adm2_reg <- left_join(adm2_reg, wind_adm2, by = c("GID_2" = "gid"))
  
  # Plot the total wind exposure in each region
  plot <- 
    ggplot(countries) +
    geom_sf(aes(color="Excluded from study", geometry = geometry), lwd = 0) +
    scale_color_manual(values = c("Excluded from study" = "black")) +
    geom_sf(data = adm1_reg, aes(fill = total_windsp_ms, geometry = geometry), lwd = 0) + # Region fills
    geom_sf(data = adm2_reg, aes(fill = total_windsp_ms, geometry = geometry), lwd = 0) + # Region fills
    scale_fill_gradientn(
      colors = c("#E3F2FD", "#2166AC", "#800000"),
      values = scales::rescale(c(0, 250, 1300)),
      na.value = "grey80",
    ) +
    ggtitle(~bold("b.")) +
    labs(fill = "Total wind speed (m/s)", color = "") +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank()) +
    geom_sf(fill = NA, color = "black", lwd = 0.1) + # Add country borders
    coord_sf(xlim = c(-85, 140), ylim = c(-40, 50))
  
  # Save the plot
  ggsave("figures/wind_map.jpeg", plot, width = 9, height = 4, dpi= 600)
  
  # Wind data histogram
  ggplot(wind_dat[wind_dat$windsp_ms != 0, ], aes(x = windsp_ms)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "white", alpha = 0.8) +
    labs(
      x = "Average wind speed (m/s)",
      y = "Frequency"
    ) +
    scale_x_continuous(expand = c(0.02, 0.02)) +
    scale_y_continuous(expand = c(0.02, 0.02)) +
    theme_minimal() +
    theme(
      axis.line = element_line(color = "black", linewidth = 0.25),
      panel.grid = element_blank(),
      axis.ticks.y = element_line(color = "black"),
    )
  
  # Save the panel plot
  ggsave("figures/windspeed.jpeg", height = 3, width = 4)
}
