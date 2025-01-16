
# Create panel data for windspeeds

meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx")
iso_adm1 <- meta_dat$iso[meta_dat$level == "Adm1"]
iso_adm2 <- meta_dat$iso[meta_dat$level == "Adm2"]

adm1_reg <- gadm(meta_dat[meta_dat$level=="Adm1", ]$iso, 
                 level=1, path="data", version="3.6") %>% st_as_sf() %>%
  select("GID_0", "NAME_0", "GID_1", "geometry")
colnames(adm1_reg) <- c("iso", "country", "gid", "geometry")
adm2_reg <- gadm(meta_dat[meta_dat$level=="Adm2", ]$iso, 
                 level=2, path="data", version="3.6") %>% st_as_sf() %>%
  select("GID_0", "NAME_0", "GID_2", "geometry")
colnames(adm2_reg) <- c("iso", "country", "gid", "geometry")
regs <- rbind(adm1_reg, adm2_reg)

year <- 1980

# Create blank panel data for the year
regs_year <- regs %>%
  mutate(year = year)

wind_dat_adm1 <- vect(paste0("data/avg_windspeed/avg_windspeed_", year, "_adm1.shp")) %>%
  as.data.frame() %>% filter(GID_0 %in% iso_adm1) %>%
  select("GID_0", "NAME_0", "GID_1", "avg_windsp")
colnames(wind_dat_adm1) <- c("iso", "country", "gid", "avg_windsp")

wind_dat_adm2 <- vect(paste0("data/avg_windspeed/avg_windspeed_", year, "_adm2.shp")) %>%
  as.data.frame() %>% filter(GID_0 %in% iso_adm2) %>%
  select("GID_0", "NAME_0", "GID_2", "avg_windsp")
colnames(wind_dat_adm2) <- c("iso", "country", "gid", "avg_windsp")

wind_dat <- rbind(wind_dat_adm1, wind_dat_adm2)
regs_year <- left_join(regs_year, wind_dat)

# Plot the wind data
countries <- gadm(country_codes()$ISO3, level=0, path="data", version="3.6") %>%
  st_as_sf()
plot <- 
  ggplot(countries) +
  geom_sf(aes(color="Excluded from study", geometry = geometry), lwd = 0) +
  scale_color_manual(values = c("Excluded from study" = "black")) +
  geom_sf(data = regs_year, aes(fill = avg_windsp, geometry = geometry), lwd = 0) + # Region fills
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey80") +
  ggtitle(~bold("b.")) +
  labs(fill = paste0("Average windspeed ", year), color = "") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank()) +
  geom_sf(fill = NA, color = "black", lwd = 0.1) + # Add country borders
  coord_sf(xlim = c(-85, 140), ylim = c(-40, 50))

# Save the plot
ggsave(paste0("figures/windspeed/", year, ".jpeg"), plot, width = 9, height = 4, dpi= 600)

# Plot the grid data

