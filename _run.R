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
results_etwfe <- data.frame()
results_etwfe_pt <- data.frame()
results_etwfe_logit <- data.frame()
results_etwfe_logit_pt <- data.frame()

results_etwfe_adm1 <- data.frame()
results_etwfe_logit_adm1 <- data.frame()

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
  results_etwfe <- rbind(results_etwfe, result) # Store the results
  result_pt <- run_etwfe_pt(dat_all, adm_level, iso) # Run PT test
  results_etwfe_pt <- rbind(results_etwfe_pt, result_pt) # Store the results
  
  # Run ETWFE model with logit link function
  result2 <- run_etwfe_logit(dat_all, adm_level, iso)
  results_etwfe_logit <- rbind(results_etwfe_logit, result2) # Store the results
  result2_pt <- run_etwfe_logit_pt(dat_all, adm_level, iso) # Run PT test
  results_etwfe_logit_pt <- rbind(results_etwfe_logit_pt, result2_pt) # Store the results
  
  # Run Adm1 analysis for countries that are analysed at Adm2 level
  if (adm_level==2) {
    
    # Get the cyclone panel data
    cy_dat2 <- readRDS(paste0("data/panel_dat/GID_", adm_level=1, "-64/", iso, ".rds"))
    
    # Merge dhs-mics data and cyclone data
    dat_all2 <- suppressMessages(merge_cy_dhs_dat(dat, cy_dat2, iso, adm_level=1))
    
    # Run ETWFE model
    result_1 <- run_etwfe(dat_all2, adm_level=1, iso)
    results_etwfe_adm1 <- rbind(results_etwfe_adm1, result_1)
    
    # Run ETFE logit model
    result_2 <- run_etwfe_logit(dat_all2, adm_level=1, iso)
    results_etwfe_logit_adm1 <- rbind(results_etwfe_logit_adm1, result_2)
  }
}
  
write_xlsx(results_etwfe, "results/results_etwfe.xlsx")
write_xlsx(results_etwfe_pt, "results/results_etwfe_pt.xlsx")
write_xlsx(results_etwfe_logit, "results/results_etwfe_logit.xlsx")
write_xlsx(results_etwfe_logit_pt, "results/results_etwfe_logit_pt.xlsx")

write_xlsx(results_etwfe_adm1, "results/results_etwfe_adm1.xlsx")
write_xlsx(results_etwfe_logit_adm1, "results/results_etwfe_logit_adm1.xlsx")
