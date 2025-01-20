
# Function that creates panel data
# `regions_aff_all` is created from the function `get_affected_regions`

create_panels <- function(regions_aff_all, thres) {

  # Country codes where DHS-MICS data are available
  ccodes <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
    filter(level!="NA")
  
  # Create directories
  dir.create(paste0("data/panel_plots_", thres, "/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0("data/panel_dat_", thres, "/"), showWarnings = FALSE, recursive = TRUE)
  
  for (i in 1:nrow(ccodes)) {
    
    iso <- ccodes$iso[i] # country code
    
    # If no cyclones, then skip
    if (!(iso %in% regions_aff_all$GID_0)) next
    
    lvl <- ccodes$level[i] # Administrative level
    level <- paste0("GID_", lvl) # Region level code
    
    # Get GADM regions
    regions <- gadm(iso, level = lvl, "data", version="3.6")
    
    # Set as sf object & remove geometry
    regions <- st_as_sf(regions)
    regions <- st_drop_geometry(regions) # Improves efficiency for now
    
    # Create panel data for 1980-2015
    year <- 1980:2015
    panel_dat <- regions %>% crossing(year)
    
    # Extract affected regions for country & select relevant columns
    regions_aff <- regions_aff_all %>% filter(GID_0==iso) %>%
      select(GID_0, !!sym(level), year, cyclone) %>%
      distinct()
    
    # Merge affected regions with panel data
    panel_dat_merged <- suppressMessages(left_join(panel_dat, regions_aff))
    
    # Fix cyclone variable
    panel_dat_merged$cyclone <- ifelse(is.na(panel_dat_merged$cyclone), 0, panel_dat_merged$cyclone)
    
    # Save the panel data
    saveRDS(panel_dat_merged, paste0("data/panel_dat_", thres, "/", iso, ".rds"))
    
    # Plot the panel for country
    panel_dat_merged$cyclone_plot <- factor(panel_dat_merged$cyclone, levels = c(0, 1), labels = c("No Cyclone", "Cyclone"))
    plot <- ggplot(panel_dat_merged, aes(x = year, y = !!sym(level), fill = cyclone_plot)) +
      geom_tile(color = "white") +
      scale_fill_manual(values = c("lightblue", "darkblue")) +
      labs(x = "Year", y = "Region", title = paste0("Tropical Cyclones: ", iso)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save the panel plot
    ggsave(paste0("data/panel_plots_", thres, "/", iso, ".jpeg"), plot,
           height = 6, width = 10)
  }
}
