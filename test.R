thres <- 64

# Set up parallel processing
cl <- makeCluster(detectCores() - 1) # Use one less core than available
registerDoParallel(cl)

# Read the country codes and filter
ccodes <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
  filter(!is.na(include))

# List TC-DAT files
files <- list.files("data/TC_data/")

# Function to get smallest valid GADM region
get_valid_regions <- function(country_code) {
  levels <- 3:1  # Check from level 3 to 1
  regions <- NULL
  for (level in levels) {
    regions <- suppressMessages(gadm(country_code, level = level, "data", version = "3.6"))
    if (!is.null(regions)) break
  }
  
  if (is.null(regions)) return(NULL)
  
  regions_sf <- st_as_sf(regions)
  if (!all(st_is_valid(regions_sf))) {
    regions_sf <- st_make_valid(regions_sf)
  }
  
  return(regions_sf)
}

# Use foreach with rbind to combine results
regions_aff_all <- foreach(i = seq_along(files), .combine = rbind, .packages = c("dplyr", "sf", "geodata")) %dopar% {
  
  # Extract year of track directly from the file name
  year <- as.numeric(substr(files[i], 1, 4))
  if (year < 1980) return(NULL)  # Skip early years
  
  # Read and filter cyclone data
  data <- read.csv(file.path("data/TC_data", files[i])) %>%
    filter(windspeed >= thres)
  
  # Extract only relevant iso codes
  iso_codes <- intersect(unique(data$ISO), ccodes$iso)
  
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
  }
  
  return(results)
}

# Stop parallel processing
stopCluster(cl)