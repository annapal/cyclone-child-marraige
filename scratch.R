library(terra)
library(ggplot2)

# Read in the raster file
wind_raster <- rast("data/avg_windspeed/windspeed_1983.tif")

# Get Timor-Leste boundaries using GADM
wind_timor <- gadm("TLS", path = "data", version = "3.6")

# Ensure CRS match
wind_timor <- project(wind_timor, crs(wind_raster))

# Crop and mask the raster to Timor-Leste
wind_timor_raster <- crop(wind_raster, wind_timor) |> mask(wind_timor)

# Convert raster to dataframe for ggplot
wind_df <- as.data.frame(wind_timor_raster, xy = TRUE)

# Plot using ggplot
ggplot() +
  geom_raster(data = wind_df, aes(x = x, y = y, fill = mean)) +
  geom_sf(data = st_as_sf(wind_timor), fill = NA, color = "black", linewidth = 0.8) +
  scale_fill_viridis_c(name = "Wind Speed") +
  theme_minimal()
