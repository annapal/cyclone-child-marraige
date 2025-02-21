
# Create panel data for categories

create_panels_cat <- function() {
  
  meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>% filter(exclude==0)
  iso_adm1 <- meta_dat$iso[meta_dat$level == "Adm1"] # Countries at Adm1 level
  iso_adm2 <- meta_dat$iso[meta_dat$level == "Adm2"] # Countries at Adm2 level
  
  wind_dat_all <- data.frame()
  
  for (year in 1980:2015) {
    wind_dat_adm1 <- vect(paste0("data/cat_windspeed/cat_windspeed_", year, "_adm1.shp")) %>%
      as.data.frame() %>% filter(GID_0 %in% iso_adm1) %>%
      select("GID_0", "NAME_0", "GID_1", "max_cat")
    colnames(wind_dat_adm1) <- c("iso", "country", "gid", "max_cat")
    wind_dat_adm1$level <- "Adm1"
    
    wind_dat_adm2 <- vect(paste0("data/cat_windspeed/cat_windspeed_", year, "_adm2.shp")) %>%
      as.data.frame() %>% filter(GID_0 %in% iso_adm2) %>%
      select("GID_0", "NAME_0", "GID_2", "max_cat")
    colnames(wind_dat_adm2) <- c("iso", "country", "gid", "max_cat")
    wind_dat_adm2$level <- "Adm2"
    
    wind_dat <- rbind(wind_dat_adm1, wind_dat_adm2)
    wind_dat$year <- year
    
    # Combine the data
    wind_dat_all <- rbind(wind_dat_all, wind_dat)
  }
  
  # Save the panel data
  saveRDS(wind_dat_all, file="data/wind_dat_all_cat.rds")
  
  # Make panel plots
  for (iso3 in unique(wind_dat_all$iso)) {
    # Get country data
    wind_iso <- wind_dat_all %>% filter(iso==iso3)
    
    # Make the plot
    plot <- ggplot(wind_iso, aes(x = year, y = gid, fill = max_cat)) +
      geom_tile(color="white") +
      scale_fill_gradientn(
        colors = c("white", "#2166AC", "#B2182B"),
        limits = c(0, 5),
        na.value = "grey80",
      ) +
      labs(x = "Year", y = "Subnational Region", 
           title = countrycode(iso3, "iso3c", "country.name"),
           fill = "Storm Category") +
      theme_minimal() +
      scale_x_continuous(limits=c(1979, 2016), expand = c(0, 0),
                         breaks = seq(1980, 2015, by=5)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25),
            panel.grid.major = element_blank())
    
    # Save the panel plot
    ggsave(paste0("data/panel_plots_cat/", iso3, "_panel_cat.jpeg"), plot  = plot,
           # height = max(length(unique(wind_iso$gid))/10, 5), 
           height = 5,
           width = 7)
  }
  
}

