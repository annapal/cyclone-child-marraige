
# Function that calculates affected regions at the smallest administrative level
# `thres` is the min windspeed (in knts) used to determine whether a region is "affected"
# One dataframe is produced for all countries that have DHS-MICS data

get_affected_regions <- function(thres) {
  
  # Country codes where DHS-MICS data are available
  ccodes <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
    filter(!is.na(include))
  
  # TC-DAT files
  files <- list.files("data/TC_data/")
  
  # Dataframe to store region data
  regions_aff_all <- data.frame()
  
  for (i in 1:length(files)) {
    
    # Get file name
    file_name <- files[i]
    
    # Extract year of track
    year <- as.numeric(substr(file_name, 1, 4))
    
    # If the year is pre 1980, move to the next
    if (year<1980) {
      next
    }
    
    # Read in the TC data for cyclone track
    data <- read.csv(paste0("data/TC_data/", file_name)) %>%
      filter(windspeed>=thres)
    
    iso <- unique(data$ISO)
    
    # Remove countries that aren't available
    iso <- intersect(iso, country_codes()$ISO3)
    
    # Extract data for each country in the path of the track
    for (k in iso) {
      
      # If there's no DHS-MICS data for that country, then pass
      if (!(k %in% ccodes$iso)) {
        next
      }
      
      # Get the smallest GADM regions that exist
      regions <- suppressMessages(gadm(k, level = 3, "data", version="3.6"))
      if (is.null(regions)) {
        regions <- suppressMessages(gadm(k, level = 2, "data", version="3.6"))
      }
      if (is.null(regions)) {
        regions <- suppressMessages(gadm(k, level = 1, "data", version="3.6"))
      }
      
      # Set as sf object
      regions <- st_as_sf(regions)
      
      # Check whether polygons are valid
      validity_check <- st_is_valid(regions)
      
      # If polygons are invalid, make them valid
      if (!(all(validity_check))) {
        regions <- st_make_valid(regions)
      }
      
      # Convert the cyclones data to sf object
      data <- st_as_sf(data, coords = c("LON", "LAT"), crs=4326)
      
      # Spatial join the dataframes
      data_joined <- st_join(data, regions)
      
      # Get affected regions
      regions_aff <- data_joined %>% select(starts_with("GID_"))
      regions_aff <- st_drop_geometry(regions_aff)
      regions_aff <- regions_aff %>% na.omit() %>% distinct()
      
      # If there are no affected regions, then go to the next country
      if (nrow(regions_aff)==0) {
        next
      }
      
      # Add variable that indicates cyclone affected
      regions_aff$cyclone <- 1
      
      # Add missing columns (if applicable)
      if (!"GID_1" %in% names(regions_aff)) {
        regions_aff$GID_1 <- NA
      }
      if (!"GID_2" %in% names(regions_aff)) {
        regions_aff$GID_2 <- NA
      }
      if (!"GID_3" %in% names(regions_aff)) {
        regions_aff$GID_3 <- NA
      }
      
      # Add year 
      regions_aff$year <- year
      
      # Reorder columns
      regions_aff <- regions_aff %>%
        select(GID_0, GID_1, GID_2, GID_3, year, cyclone)
      
      # Add to dataframe
      regions_aff_all <- rbind(regions_aff_all, regions_aff)
    }
  }
  
  # Save affected regions data
  dir.create("data/cyclone", showWarnings = FALSE)
  saveRDS(regions_aff_all, paste0("data/cyclone/affected_regions_", thres, ".rds"))
  
  # Return affected regions dataframe
  regions_aff_all
}
