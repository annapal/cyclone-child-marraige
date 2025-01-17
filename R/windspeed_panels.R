
# Create panel data for windspeeds

create_panels <- function() {

  meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>% filter(exclude==0)
  iso_adm1 <- meta_dat$iso[meta_dat$level == "Adm1"] # Countries at Adm1 level
  iso_adm2 <- meta_dat$iso[meta_dat$level == "Adm2"] # Countries at Adm2 level
  
  wind_dat_all <- data.frame()
  
  for (year in 1980:2015) {
    wind_dat_adm1 <- vect(paste0("data/avg_windspeed/avg_windspeed_", year, "_adm1.shp")) %>%
      as.data.frame() %>% filter(GID_0 %in% iso_adm1) %>%
      select("GID_0", "NAME_0", "GID_1", "avg_windsp")
    colnames(wind_dat_adm1) <- c("iso", "country", "gid", "avg_windsp_knt")
    wind_dat_adm1$level <- "Adm1"
    
    wind_dat_adm2 <- vect(paste0("data/avg_windspeed/avg_windspeed_", year, "_adm2.shp")) %>%
      as.data.frame() %>% filter(GID_0 %in% iso_adm2) %>%
      select("GID_0", "NAME_0", "GID_2", "avg_windsp")
    colnames(wind_dat_adm2) <- c("iso", "country", "gid", "avg_windsp_knt")
    wind_dat_adm2$level <- "Adm2"
    
    wind_dat <- rbind(wind_dat_adm1, wind_dat_adm2)
    wind_dat$windsp_ms <- wind_dat$avg_windsp * 0.514444
    wind_dat$year <- year
    
    # Combine the data
    wind_dat_all <- rbind(wind_dat_all, wind_dat)
  }
  
  # Save the panel data
  saveRDS(wind_dat_all, file="data/wind_dat_all.rds")
  
  # Make panel plots
  for (iso3 in unique(wind_dat_all$iso)) {
    # Get country data
    wind_iso <- wind_dat_all %>% filter(iso==iso3)
    
    # Make the plot
    plot <- ggplot(wind_iso, aes(x = year, y = gid, fill = windsp_ms)) +
      geom_tile() +
      scale_fill_gradient2(
        low = "white",
        mid = "blue",
        high = "red",
        midpoint = 50,
      ) +
      labs(x = "Year", y = "Subnational Administrative Region", title = paste0(iso3, ": Wind Speed (m/s)")) +
      theme_minimal() +
      scale_x_continuous(limits=c(1979, 2016), expand = c(0, 0)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_blank(),
            panel.grid.major = element_blank())
    
    # Save the panel plot
    ggsave(paste0("data/panel_plots/", iso3, "_panel.jpeg"), plot  = plot,
           # height = max(length(unique(wind_iso$gid))/10, 5), 
           height = 6,
           width = 8)
  }

}

