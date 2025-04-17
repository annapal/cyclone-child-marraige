
# Calculate the category of each storm (Saffir-Simpson Scale)

storm_cat <- function() {

  # Get meta data on surveys
  meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx")
  
  # Get subnational regions from the GADM
  reg_adm1 <- gadm(meta_dat$iso[meta_dat$level == "Adm1"], level=1, path="data", version="3.6")
  reg_adm2 <- gadm(meta_dat$iso[meta_dat$level == "Adm2"], level=2, path="data", version="3.6")
  
  # Path to the TC data
  path <- "data/TC_data"
  
  for (year in 1980:2015) {
    # Get all files that pertain to that year
    files <- list.files(path, pattern = paste0("^", year), full.names = TRUE)
    combined_data <- do.call(rbind, lapply(files, function(file) {
      data <- read.csv(file)
      data <- data[, !names(data) %in% c("exposed_assets", "exposed_pop")]  # Remove unwanted columns
      
      # Add category
      max_wind <- max(data$windspeed) # Max sustained windspeed
      data <- data %>%
        mutate(cat = case_when(
          windspeed>64 & max_wind >= 64 & max_wind <= 82 ~ 1,
          windspeed>64 & max_wind >= 83 & max_wind <= 95 ~ 2,
          windspeed>64 & max_wind >= 96 & max_wind <= 112 ~ 3,
          windspeed>64 & max_wind >= 113 & max_wind <= 136 ~ 4,
          windspeed>64 & max_wind >= 137 ~ 5,
          TRUE ~ 0  # Assigns NA for values outside the given ranges
        ))
      return(data)
    }))
    
    # Where the same pixel had multiple exposures, take the max cat
    final_data <- combined_data %>%
      group_by(ISO, LAT, LON) %>%
      slice_max(cat, with_ties = FALSE) %>%
      ungroup() %>%
      select(LON, LAT, cat)
    
    # Convert this to SpatRaster
    # Note: this ensures that the average is computed across the entire area of the subnational region
    global_grid <- rast(extent = c(-180, 180, -90, 90), resolution = 0.1, crs = crs(reg_adm1))
    points <- vect(final_data, geom = c("LON", "LAT"), crs = crs(reg_adm1))
    rasterized <- rasterize(points, global_grid, field = "cat", fun = mean)
    rasterized[is.na(rasterized)] <- 0
    
    # Calculate category within each polygon in reg_adm1
    max_cat_adm1 <- terra::extract(rasterized, reg_adm1, fun = max, na.rm = TRUE)
    reg_adm1$max_cat <- max_cat_adm1$mean
    
    # Calculate category within each polygon in reg_adm2
    max_cat_adm2 <- terra::extract(rasterized, reg_adm2, fun = max, na.rm = TRUE)
    reg_adm2$max_cat <- max_cat_adm2$mean
    
    # Save files
    writeVector(reg_adm1, paste0("data/cat_windspeed/cat_windspeed_", year, "_adm1.shp"), 
                filetype = "ESRI Shapefile", overwrite = TRUE)
    writeVector(reg_adm2, paste0("data/cat_windspeed/cat_windspeed_", year, "_adm2.shp"), 
                filetype = "ESRI Shapefile", overwrite = TRUE)
    writeRaster(rasterized, paste0("data/cat_windspeed/windspeed_", year, ".tif"), 
                filetype = "GTiff", overwrite = TRUE)
    
    # Tell me its complete
    print(paste0("Complete: ", year))
  }
}
