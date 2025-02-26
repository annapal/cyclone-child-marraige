## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# create_dhsmics_meta_data() # Create DHS/MICS meta data
# avg_windspeed() # Calculate average and max windspeed in each subnat region
# storm_cat() # Calculate storm categories

# all_dat <- combine_data()
all_dat <- readRDS("data/all_dat.rds")
# create_panels()
# create_panels_max()
# create_panels_cat()
wind_dat <- readRDS("data/wind_dat_all.rds")
wind_dat_max <- readRDS("data/wind_dat_all_max.rds")
wind_dat_cat <- readRDS("data/wind_dat_all_cat.rds")

run_analysis(all_dat, wind_dat)
run_analysis_het()
run_analysis_gps()
run_analysis_max(all_dat, wind_dat_max)
run_analysis_binary(all_dat, wind_dat)
run_analysis_cat(all_dat, wind_dat_cat)
# run_coastal()

plot_child_marriage(all_dat)
plot_avg_windspeed()
plot_panels(wind_dat)
plot_lines(all_dat)
plot_eff_het()
plot_migration(all_dat)
plot_missing(all_dat)
