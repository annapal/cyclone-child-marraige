
# Aggregate the DHS data

# Calculate normalised weight
dat_all$Norm_wt <- dat_all$Denorm_Wt * (nrow(dat_all)/sum(dat_all$Denorm_Wt))

# Create rural factor
dat_all$rural <- ifelse(dat_all$res=="rural", 1, 0) # Make indicator for rural status

dat_agg <- dat_all %>%
  group_by(GID_2, year, cohort) %>%
  summarise(
    pop = sum(Norm_wt),
    married = sum(married * Norm_wt),
    avg_age = weighted.mean(age_turned, Norm_wt),
    prop_rural = weighted.mean(rural, Norm_wt) 
  ) %>%
  ungroup()
  
