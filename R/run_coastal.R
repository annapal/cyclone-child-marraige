
meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>% filter(exclude==0)
# Get subnational regions from the GADM
reg_adm1 <- gadm(country_codes()$ISO3, level=1, path="data", version="3.6")
png("coastline_plots/reg_adm1_plot.png", width = 1000, height = 800, res = 150)  # Adjust size & resolution
plot(reg_adm1, ylim=c(-90, 90))
dev.off()

# reg_adm2 <- gadm(meta_dat$iso[meta_dat$level == "Adm2"], level=2, path="data", version="3.6")

# Load coastline data
coastline <- vect("data/ne_10m_coastline/ne_10m_coastline.shp")
coastline <- project(coastline, crs(reg_adm1))
png("coastline_plots/coastline.png", width = 1000, height = 800, res = 150)  # Adjust size & resolution
plot(coastline, ylim=c(-90, 90))
dev.off()

# Identify coastal regions
coastline_buffer <- buffer(coastline, width = 5000)
reg_adm1$is_coastal <- as.integer(relate(reg_adm1, coastline_buffer, "intersects"))
# reg_adm2$is_coastal <- as.integer(lengths(intersect(reg_adm2, coast_buffer)) > 0)

png("coastline_plots/coastline_regions.png", width = 1000, height = 800, res = 150)  # Adjust size & resolution
plot(reg_adm1, "is_coastal", col = c("gray80", "blue"), main = "Coastal Regions")
dev.off()

