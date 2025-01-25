
# Get meta data
meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
  filter(exclude==0) %>%
  arrange(iso)

# Read in DHS/MICS data
all_dat <- readRDS("data/all_dat.rds")

# Get TC data
files <- list.files("data/TC_data")
filtered_files <- files[grepl("^(198[0-9]|199[0-9]|200[0-9]|201[0-5])", files)]

iso_cde <- "BGD"

# Get only the TC data relevant to the country
cy_dat_all <- data.frame()
for (i in 1:length(filtered_files)) {
  cy_dat <- read.csv(paste0("data/TC_data/", filtered_files[i])) %>%
    filter(ISO==iso_cde)
  if (nrow(cy_dat)>0) {
    cy_dat$year <- as.numeric(substr(filtered_files[i], 1, 4))
    cy_dat_all <- rbind(cy_dat_all, cy_dat)
  }
}

# Where there are multiple measurements in the same year
cy_dat_all <- cy_dat_all %>%
  group_by(LAT, LON, year) %>%
  summarise(windspeed = max(windspeed, na.rm = TRUE), .groups = "drop")

# Convert to raster
raster_data <- terra::rast(
  x = cy_dat_all[, c("LON", "LAT", "year", "windspeed")],
  type = "xyz",
  crs = "EPSG:4326"
)

# Filter MICS/DHS data for that country
dat <- all_dat %>% filter(iso3 == iso_cde & !is.na(lat) & !is.na(long))
dat_coords <- dat %>%
  select(long, lat, year, case_id)

dat_vect <- vect(dat_coords, geom = c("long", "lat"), crs = "EPSG:4326")

# TODO this isn't correct
windspeed <- terra::extract(raster_data, dat_vect, method = "simple")
