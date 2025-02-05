
# Create directory if needed
dir.create("figures", showWarnings = FALSE)

# Get countries to include in the analysis
iso_inc <- read_excel("data/meta_dhs_mics_updated.xlsx") %>% 
  filter(!is.na(include))

# Get isos for all countries
all_countries <- country_codes()$ISO3

# Get all country (Adm0) geometries
countries <- gadm(all_countries, level=0, path="data", version="3.6")
country_data <- st_as_sf(countries) # Set as sf object

# Dataframe to store results from all regions
regions_all <- data.frame()

for (iso in iso_inc$iso) {

  # Load in cyclones panel data
  dat <- readRDS(paste0("data/panel_dat/", iso, ".rds"))
  
  # If analysis is at the adm2 level
  if ("GID_2" %in% colnames(dat)) {
    
    # Get number of cyclones in each region
    dat_cy <- dat %>% group_by(GID_2) %>%
      summarize(total_cyclones = sum(cyclone, na.rm = TRUE))
    
    # Get geometries from the GADM
    regions <- gadm(iso, level=2, path="data", version="3.6")
    regions <- st_as_sf(regions)
    
    # Merge with cyclones data
    regions <- left_join(regions, dat_cy) %>%
      select(GID_0, GID_2, total_cyclones, geometry)
    colnames(regions) <- c("iso", "region", "total_cyclones", "geometry")
    
    # Add to all results
    regions_all <- rbind(regions_all, regions)
    
  } else {
    # Analysis is at the adm1 level
    
    # Get number of cyclones in each region
    dat_cy <- dat %>% group_by(GID_1) %>%
      summarize(total_cyclones = sum(cyclone, na.rm = TRUE))
    
    # Get geometries from the GADM
    regions <- gadm(iso, level=1, path="data", version="3.6")
    regions <- st_as_sf(regions)
    
    # Merge with cyclones data
    regions <- left_join(regions, dat_cy) %>%
      select(GID_0, GID_1, total_cyclones, geometry)
    colnames(regions) <- c("iso", "region", "total_cyclones", "geometry")
    
    # Add to all results
    regions_all <- rbind(regions_all, regions)
  }
}

# Plot the map
plot <- 
  ggplot(country_data) +
  geom_sf(aes(color="Excluded from study", geometry = geometry), lwd = 0) +
  scale_color_manual(values = c("Excluded from study" = "black")) +
  geom_sf(data = regions_all, aes(fill = total_cyclones, geometry = geometry), lwd = 0) + # Region fills
  scale_fill_gradient(low = "#fdecea", high = "#7f0000", na.value = "grey80") +
  ggtitle(~bold("a.")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(), legend.title=element_blank(),
        panel.background = element_blank()) +
  geom_sf(fill = NA, color = "black", lwd = 0.03) # Add country borders

# Save the plot
ggsave(paste0("figures/map1.jpeg"), 
       plot, width = 9, height = 4, dpi= 1200)

