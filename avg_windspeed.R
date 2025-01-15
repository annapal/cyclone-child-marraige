
library(rnaturalearth)
# Load world boundaries for plotting
countries <- ne_countries(returnclass = "sf")
countries_vect <- vect(countries)

# Get subnational regions from the GADM
reg_adm1 <- gadm(country_codes()$ISO3, level=1, path="data", version="3.6")

# Path to the TC data
path <- "data/TC_data"

for (year in 1980:2015) {
  # Get all files that pertian to that year
  files <- list.files(path, pattern = paste0("^", year), full.names = TRUE)
  combined_data <- do.call(rbind, lapply(files, function(file) {
    data <- read.csv(file)
    data <- data[, !names(data) %in% c("exposed_assets", "exposed_pop")]  # Remove unwanted columns
    return(data)
  }))
  
  # Where the same pixel had multiple exposures, take the max windspeed value
  final_data <- combined_data %>%
    group_by(ISO, LAT, LON) %>%
    slice_max(windspeed, with_ties = FALSE) %>%
    ungroup() %>%
    select(LON, LAT, windspeed)
  
  # Convert this to SpatRaster
  # Note: this ensures that the average is computed across the entire area of the subnational region
  global_grid <- rast(extent = c(-180, 180, -90, 90), resolution = 0.1, crs = crs(reg_adm1))
  points <- vect(final_data, geom = c("LON", "LAT"), crs = crs(reg_adm1))
  rasterized <- rasterize(points, global_grid, field = "windspeed", fun = mean)
  rasterized[is.na(rasterized)] <- 0
  
  # # Plot the raster
  # plot(rasterized, main = "Global Windspeed Distribution")
  # plot(countries_vect, add = TRUE, border = "black", lwd = 0.5)
  
  # Calculate average windspeed within each polygon in reg_adm1
  average_windspeed <- terra::extract(rasterized, reg_adm1, fun = mean, na.rm = TRUE)
  
  # Assign the average windspeed to the reg_adm1 polygons
  reg_adm1$avg_windspeed <- average_windspeed$mean
  
  # # View the result
  # reg_adm1_clean <- reg_adm1[reg_adm1$avg_windspeed!=0, ]
  # plot(reg_adm1_clean, "avg_windspeed")
  
  # Save files
  dir.create(paste0("data/avg_windspeed/", year), recursive = TRUE, showWarnings = FALSE)
  writeVector(reg_adm1, paste0("data/avg_windspeed/", year, "/avg_windspeed_", year, ".shp"), filetype = "ESRI Shapefile")
  writeRaster(rasterized, paste0("data/avg_windspeed/", year, "/windspeed_", year, ".tif"), filetype = "GTiff", overwrite = TRUE)

  print(paste0("Complete: ", year))
}


