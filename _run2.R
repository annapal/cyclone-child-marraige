## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# create_dhsmics_meta_data() # Create DHS/MICS meta data
avg_windspeed() # Calculate average windspeed in each subnat region

all_dat <- combine_data()
plot_rate(all_dat)
