
# Function that cleans and merges the cyclones and DHS-MICS data

merge_cy_dhs_dat <- function(dat, cy_dat, iso, adm_level) {
  
  # Admin level variable
  gid <- paste0("GID_", adm_level)
  
  # Assign year after cyclone as exposed
  cy_dat <- cy_dat %>%
    arrange(get(gid), year) %>%
    group_by(get(gid)) %>%
    mutate(
      cyclone2=ifelse(lag(cyclone, default=0)==1, 1, cyclone), # New exposure variable
      lagged=ifelse((cyclone2-cyclone==1), 1, 0), # Indicator of when exposure is lagged
    ) %>%
    ungroup()
  
  # Assign cohorts to the cyclone data based on treatment history
  cy_cohort <- cy_dat %>%
    group_by(get(gid)) %>%
    summarize(cyclone_sequence = paste(cyclone2, collapse = ""))
  colnames(cy_cohort) <- c(gid, "cyclone_sequence")
  cy_cohort$cohort <- as.numeric(factor(cy_cohort$cyclone_sequence))
  cy_dat <- suppressMessages(left_join(cy_dat, cy_cohort))
  
  # Add GID_1 codes to dhs-mics data
  adm1_reg <- st_as_sf(gadm(iso, level = 1, path="data", version="3.6")) %>%
    select(GID_1, NAME_1) %>%
    st_drop_geometry() %>%
    rename(Adm1 = NAME_1)
  dat <- left_join(dat, adm1_reg)
  
  # Add GID_2 codes to the dhs-mics data (if necessary)
  if (adm_level==2) {
    adm2_reg <- st_as_sf(gadm(iso, level = 2, path="data", version="3.6")) %>%
      select(GID_1, GID_2, NAME_2) %>%
      st_drop_geometry() %>%
      rename(Adm2 = NAME_2)
    dat <- left_join(dat, adm2_reg)
  }
  
  # Get necessary columns from cyclone data
  if (adm_level==2) {
    cy_dat <- cy_dat %>% select(GID_1, GID_2, year, cyclone, cyclone2, lagged, cohort)
  } else {
    cy_dat <- cy_dat %>% select(GID_1, year, cyclone, cyclone2, lagged, cohort)
  }
  
  # Merge cyclone data with the dhs-mics data
  dat_all <- left_join(dat, cy_dat)
  
  # Remove years without cyclone data
  dat_all <- dat_all %>% filter(!is.na(cyclone))
  
  # Return the merged data
  dat_all
}
