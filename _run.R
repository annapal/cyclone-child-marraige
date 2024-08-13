## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# Get affected regions
regions_aff_all <- get_affected_regions(thres=96)
regions_aff_all <- readRDS("data/cyclone/affected_regions_96.rds")

# Create panel data
create_panels(regions_aff_all, level = "GID_1")
create_panels(regions_aff_all, level = "GID_2")
