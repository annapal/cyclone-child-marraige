

# Calculate which regions are coastal -------------------------------------

meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>% filter(exclude==0)

# Get subnational regions from the GADM
reg_adm1 <- gadm(meta_dat$iso[meta_dat$level == "Adm1"], level=1, path="data", version="3.6")
reg_adm2 <- gadm(meta_dat$iso[meta_dat$level == "Adm2"], level=2, path="data", version="3.6")

# Convert both to sf
reg_adm1_sf <- st_as_sf(reg_adm1)
reg_adm2_sf <- st_as_sf(reg_adm2)
coastline_sf <- st_as_sf(coastline)

# Calculate intersections
intersects_matrix <- st_intersects(reg_adm1_sf, coastline_sf, sparse = TRUE)
intersects_matrix_2 <- st_intersects(reg_adm2_sf, coastline_sf, sparse = TRUE)

# Convert to binary (1 = coastal, 0 = non-coastal)
reg_adm1_sf$is_coastal <- as.integer(lengths(intersects_matrix) > 0)
reg_adm2_sf$is_coastal <- as.integer(lengths(intersects_matrix_2) > 0)

# # Plot the regions to check
# plot <- ggplot() +
#   geom_sf(data = reg_adm1_sf, aes(fill = is_coastal), color = "black", linewidth = 0.05) +  # Admin regions
#   geom_sf(data = coastline_sf, color = "blue", linewidth = 0.05) +  # Coastline
#   ggtitle("GADM Regions & Coastline") +
#   theme_minimal()
# ggsave("coastline_plots/all_coastal.jpeg", plot=plot, dpi=1000)
# 
# plot <- ggplot() +
#   geom_sf(data = reg_adm2_sf, aes(fill = is_coastal), color = "black", linewidth = 0.05) +  # Admin regions
#   geom_sf(data = coastline_sf, color = "blue", linewidth = 0.05) +  # Coastline
#   ggtitle("GADM Regions & Coastline") +
#   theme_minimal()
# ggsave("coastline_plots/all_coastal_2.jpeg", plot=plot, dpi=1000)

# Read in the data
all_dat_merged <- readRDS("data/all_dat_merged.rds")
wind_dat <- readRDS("data/wind_dat_all.rds")



