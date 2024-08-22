## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# Get affected regions
# regions_aff_all <- get_affected_regions(thres=64)
# regions_aff_all <- readRDS("data/cyclone/affected_regions_64.rds")

# Create panel data
# create_panels(regions_aff_all, level = "GID_1", thres=64)
# create_panels(regions_aff_all, level = "GID_2", thres=64)

# Create meta data for DHS-MICS
# create_dhsmics_meta_data()

# Get countries to include in the analysis
iso_inc <- read_excel("data/meta_dhs_mics_updated.xlsx") %>% 
  filter(!is.na(include)) %>% select(iso, include)

# Dataframe to store the results
results_all <- data.frame()

# Generate the results for each country
for (i in 1:nrow(iso_inc)) {
  
  iso <- iso_inc$iso[i] # Country iso code
  
  # Level at which to conduct the analysis
  adm_level <- as.numeric(substr(iso_inc$include[i], 4, 5))
  
  # Message
  print(paste0("Running analysis for ", iso, " ..."))
  
  # Get DHS-MICS data
  dat <- readRDS(paste0("data/dhs-mics/", iso, ".rds")) %>%
    filter(!is.na(get(paste0("Adm", adm_level))))
  
  # Get the cyclone panel data
  cy_dat <- readRDS(paste0("data/panel_dat/GID_", adm_level, "-64/", iso, ".rds"))
  
  # Merge dhs-mics data and cyclone data
  dat_all <- suppressMessages(merge_cy_dhs_dat(dat, cy_dat, iso, adm_level))
  
  # Run ETWFE model
  result <- run_etwfe(dat_all, adm_level, iso)
  results_all <- rbind(results_all, result) # Store the results
}
  
