
# Create meta data for DHS-MICS
# Saved in the data folder

create_dhsmics_meta_data <- function() {
  # Get iso codes for countries that have DHS/MICS data
  isos <- substr(list.files("data/dhs-mics"), 1,3)
  
  # Get iso codes for countries that have had a cyclone
  files <- list.files("data/TC_data", full.names = TRUE)
  year_files <- files[as.numeric(substr(basename(files), 1, 4)) %in% 1980:2015]
  cyc_isos <- unique(unlist(lapply(year_files, function(file) {
    data <- read.csv(file)
    unique(data$ISO)
  })))
  
  # Get iso codes for countries that have DHS/MICS data and cyclone data
  all_isos <- intersect(cyc_isos, isos)
  
  # Dataframe to store meta data
  meta_dhs_mics <- data.frame(
    iso = rep(NA, length(all_isos)),
    adm1_n = rep(NA, length(all_isos)),
    adm1_yrs = rep(NA, length(all_isos)),
    adm2_n = rep(NA, length(all_isos)),
    adm2_yrs = rep(NA, length(all_isos)),
    cluster_n = rep(NA, length(all_isos)),
    cluster_yrs = rep(NA, length(all_isos))
  )
  
  for (i in 1:length(all_isos)) {
    iso <- all_isos[i]
    
    meta_dhs_mics$iso[i] <- iso
    dat <- readRDS(paste0("data/dhs-mics/", iso, ".rds"))
    
    dat_adm1 <- dat %>% filter(!is.na(Adm1))
    meta_dhs_mics$adm1_n[i] <- nrow(dat_adm1)
    meta_dhs_mics$adm1_yrs[i] <- paste0(min(dat_adm1$year)+3, "-", max(dat_adm1$year)-3)
    
    dat_adm2 <- dat %>% filter(!is.na(Adm2))
    meta_dhs_mics$adm2_n[i] <- nrow(dat_adm2)
    meta_dhs_mics$adm2_yrs[i] <- paste0(min(dat_adm2$year)+3, "-", max(dat_adm2$year)-3)
    
    dat_ll <- dat %>% filter(!is.na(lat))
    meta_dhs_mics$cluster_n[i] <- nrow(dat_ll)
    meta_dhs_mics$cluster_yrs[i] <- paste0(min(dat_ll$year)+3, "-", max(dat_ll$year)-3)
  }
  
  # Remove any countries that have no spatial info
  meta_dhs_mics <- meta_dhs_mics %>% filter(adm1_n!=0)
  
  # Save to excel
  writexl::write_xlsx(meta_dhs_mics, "data/meta_dhs_mics.xlsx")
  
  # Make figure
  long_data <- meta_dhs_mics %>%
    pivot_longer(cols = c(adm1_n, adm2_n, cluster_n), 
                 names_to = "level", 
                 values_to = "count")
  long_data$country <- countrycode(long_data$iso, "iso3c", "country.name")
  ggplot(long_data, aes(x = level, y = count, fill = level)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ country, scales="free_y") +
    labs(title = "No. of observations with spatial data at each level", 
         x = "Administrative Level", 
         y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  ggsave("figures/obs.jpeg", width=12)
  
}