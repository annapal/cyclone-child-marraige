## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

create_dhsmics_meta_data() # Create DHS/MICS meta data
avg_windspeed() # Calculate average and max windspeed in each subnat region
storm_cat() # Calculate storm categories

all_dat <- combine_data() # Pool all DHS/MICS surveys
all_dat <- readRDS("data/all_dat.rds") # Run this if line above has already been run

create_panels() # Create panels using average windspeed
create_panels_max() # Create panels using max windspeed
create_panels_cat() # Create panels using storm cat

# Read in the panel data
wind_dat <- readRDS("data/wind_dat_all.rds")
wind_dat_max <- readRDS("data/wind_dat_all_max.rds")
wind_dat_cat <- readRDS("data/wind_dat_all_cat.rds")

run_analysis(all_dat, wind_dat) # Run main analysis
run_analysis_het() # Run treatment effect hetereogeneity analysis
run_analysis_gps() # Run analysis using GPS coordinates
run_analysis_max(all_dat, wind_dat_max) # Run analysis using max windspeeds
run_analysis_binary(all_dat, wind_dat) # Run analysis using binary exposure
run_analysis_cat(all_dat, wind_dat_cat) # Run analysis with storm categories

# Plot the results
plot_child_marriage(all_dat) # Figure 1
plot_avg_windspeed() # Plot average windspeed (Figure 2)
plot_panels(wind_dat) # Plot windspeeds in subnational regions
plot_lines(all_dat) # Line plots of child marriage rates
plot_eff_het() # Plot treatment effect hetereogeneity (Figure 4)
plot_migration(all_dat) # Plot proportion who have migrated
plot_missing(all_dat) # Plot proportion of the data that is missing
