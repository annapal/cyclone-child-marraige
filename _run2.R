## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# create_dhsmics_meta_data() # Create DHS/MICS meta data
# avg_windspeed() # Calculate average windspeed in each subnat region

# all_dat <- combine_data()
all_dat <- readRDS("data/all_dat.rds")
# plot_rate(all_dat)
create_panels()
wind_dat <- readRDS("data/wind_dat_all.rds")
# plot_wind(wind_dat)

run_analysis(all_dat, wind_dat)
