
# Create meta data for DHS-MICS
# Saved in the data folder

create_dhsmics_meta_data <- function() {
  # Get number of countries with data
  n_countries <- length(list.files("data/dhs-mics"))
  
  # Dataframe to store meta data
  meta_dhs_mics <- data.frame(
    iso = rep(NA, n_countries),
    adm1_n = rep(NA, n_countries),
    adm1_yrs = rep(NA, n_countries),
    adm1_svys = rep(NA, n_countries),
    adm2_n = rep(NA, n_countries),
    adm2_yrs = rep(NA, n_countries),
    adm2_svys = rep(NA, n_countries),
    ll_n = rep(NA, n_countries),
    ll_yrs = rep(NA, n_countries),
    ll_svys = rep(NA, n_countries)
  )
  
  for (i in 1:n_countries) {
    iso <- substr(list.files("data/dhs-mics")[i], 1,3)
    
    meta_dhs_mics$iso[i] <- iso
    dat <- readRDS(paste0("data/dhs-mics/", iso, ".rds"))
    
    dat_adm1 <- dat %>% filter(!is.na(Adm1))
    meta_dhs_mics$adm1_n[i] <- nrow(dat_adm1)
    meta_dhs_mics$adm1_yrs[i] <- paste0(min(dat_adm1$year)+3, "-", max(dat_adm1$year)-3)
    meta_dhs_mics$adm1_svys[i] <- paste0(unique(dat_adm1$surveyid), collapse = ", ")
    
    dat_adm2 <- dat %>% filter(!is.na(Adm2))
    meta_dhs_mics$adm2_n[i] <- nrow(dat_adm2)
    meta_dhs_mics$adm2_yrs[i] <- paste0(min(dat_adm2$year)+3, "-", max(dat_adm2$year)-3)
    meta_dhs_mics$adm2_svys[i] <- paste0(unique(dat_adm2$surveyid), collapse = ", ")
    
    dat_ll <- dat %>% filter(!is.na(lat))
    meta_dhs_mics$ll_n[i] <- nrow(dat_ll)
    meta_dhs_mics$ll_yrs[i] <- paste0(min(dat_ll$year)+3, "-", max(dat_ll$year)-3)
    meta_dhs_mics$ll_svys[i] <- paste0(unique(dat_ll$surveyid), collapse = ", ")
  }
  
  # Save to excel
  writexl::write_xlsx(meta_dhs_mics, "data/meta_dhs_mics.xlsx")
  
}