
# Get meta data
meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx")
iso_adm1 <- meta_dat$iso[meta_dat$level == "Adm1"] # Countries at Adm1 level
iso_adm2 <- meta_dat$iso[meta_dat$level == "Adm2"] # Countries at Adm2 level

# Add lagged variables to wind_dat (k=3)
wind_dat2 <- wind_dat %>%
  arrange(gid, year) %>%
  group_by(gid) %>% 
  mutate(
    windsp_ms_lag1 = lag(windsp_ms, 1),
    windsp_ms_lag2 = lag(windsp_ms, 2),
    windsp_ms_lag3 = lag(windsp_ms, 3)
  ) %>%
  ungroup() %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) # TODO: come back to this to see if correct!

# Merge wind data with DHS data
all_dat$gid <- ifelse(all_dat$iso3 %in% iso_adm1, all_dat$GID_1, all_dat$GID_2)
all_dat_merged <- left_join(all_dat, wind_dat2)

# Get country data
iso_cde <- "IND"
dat <- all_dat_merged %>% filter(iso == iso_cde)
dat$gid_cde <- as.numeric(factor(dat$gid))

# Run model
mod <- feols(married ~ windsp_ms + windsp_ms_lag1 + windsp_ms_lag2 + windsp_ms_lag3 +
               i(gid_cde, year, ref=1)|
               gid + year + age_turned + rural,
             data=dat, vcov=~gid, weights=~Denorm_Wt)
mod

# Get cumulative effect
coefficients <- coef(mod)
vcov_matrix <- vcov(mod)
windsp_vars <- grepl("^windsp_ms", names(coefficients))
windsp_coeffs <- coefficients[windsp_vars]
windsp_vcov <- vcov_matrix[windsp_vars, windsp_vars]
aggregated_coeff <- sum(windsp_coeffs)
aggregated_se <- sqrt(sum(windsp_vcov))

aggregated_coeff
aggregated_se

