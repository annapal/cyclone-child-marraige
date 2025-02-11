## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# create_dhsmics_meta_data() # Create DHS/MICS meta data
# avg_windspeed() # Calculate average windspeed in each subnat region

# all_dat <- combine_data()
all_dat <- readRDS("data/all_dat.rds")
# create_panels()
wind_dat <- readRDS("data/wind_dat_all.rds")

run_analysis(all_dat, wind_dat)
run_analysis_het()
run_analysis_gps()

plot_child_marriage(all_dat)
plot_avg_windspeed()
plot_panels(wind_dat)
plot_lines(all_dat)
plot_eff_het()
plot_migration(all_dat)
plot_missing(all_dat)
