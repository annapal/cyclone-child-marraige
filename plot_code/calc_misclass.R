
# Get countries to include in the analysis
iso_inc <- read_excel("data/meta_dhs_mics_updated.xlsx") %>% 
  filter(!is.na(include))

move_dat <- data.frame()

for (i in 1:nrow(iso_inc)) {

  iso <- iso_inc$iso[i] # Country iso code
  adm_level <- as.numeric(substr(iso_inc$include[i], 4, 5)) # Adm level of analysis
  
  # Read in data
  dat <- readRDS(paste0("data/merged_dat/", iso, ".rds"))
  
  # Make sure there's at least some moving data
  if (adm_level==1) {
    if (all(is.na(dat$Prev_Adm1))) next
  } else {
    if (all(is.na(dat$Prev_Adm2))) next
  }
  
  if (adm_level==1) {
    dat <- dat %>%
      mutate(
        # Create variable indicating if woman has moved region
        moved = case_when(
          is.na(al_lived) ~ "Unknown", # Unknown if woman moved
          al_lived==1 ~ "No", # Woman always lived in same location
          al_lived==0 & age-yrs_lived<13 ~ "No", # Woman moved prior to being at risk of child marriage
          al_lived==0 & Adm1==Prev_Adm1 ~ "No", # Woman has moved, but moved within same region
          al_lived==0 & is.na(Prev_Adm1) ~ "Unknown", # Woman has moved, but to unknown region
          Adm1!=Prev_Adm1 ~ "Yes"), # Woman moved to a different region
        # Create a variable with the correct Adm1 region where applicable
        Adm1_moved = case_when(
          moved=="No" ~ Adm1,
          moved=="Yes" ~ Prev_Adm1,
          TRUE ~ NA
        ))
  } else {
    dat <- dat %>%
      mutate(
        # Create variable indicating if woman has moved region
        moved = case_when(
          is.na(al_lived) ~ "Unknown", # Unknown if woman moved
          al_lived==1 ~ "No", # Woman always lived in same location
          al_lived==0 & age-yrs_lived<13 ~ "No", # Woman moved prior to being at risk of child marriage
          al_lived==0 & Adm2==Prev_Adm2 ~ "No", # Woman has moved, but moved within same region
          al_lived==0 & is.na(Prev_Adm2) ~ "Unknown", # Woman has moved, but to unknown region
          Adm2!=Prev_Adm2 ~ "Yes"), # Woman moved to a different region
        # Create a variable with the correct Adm1 region where applicable
        Adm2_moved = case_when(
          moved=="No" ~ Adm2,
          moved=="Yes" ~ Prev_Adm2,
          TRUE ~ NA
        ))
  }
  
  # Add moving data to dataframe
  move_dat <- rbind(move_dat, 
                    c(iso, sum(dat$moved=="No"), 
                      sum(dat$moved=="Yes"), 
                      sum(dat$moved=="Unknown")))
  
  # TODO Calculate actual proportion misclassified
}

# Set colnames of moving data
colnames(move_dat) <- c("iso", "No", "Yes", "Unknown")
