
# Combine all country datasets into one dataset

combine_data <- function() {
  
  meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx")
  
  # Data frame to store all the data
  all_dat <- data.frame()
  
  for (i in 1:nrow(meta_dat)) {
    
    iso <- meta_dat$iso[i]
    level <- meta_dat$level[i]
    
    start_yr <- meta_dat$start_yr[i]
    end_yr <- meta_dat$end_yr[i]
    
    # Read in the DHS-MICS data
    data <- readRDS(paste0("data/dhs-mics/", iso, ".rds")) %>%
      filter(year%in%start_yr:end_yr) %>%
      filter(!is.na(.data[[level]]))
    
    # Make indicator for rural status
    data$rural <- ifelse(data$res=="rural", 1, 0) 
    
    # Add GID codes
    data$level <- level
    gadm_reg <- gadm(iso, level=as.numeric(substr(level, 4, 5)), path="data", version = "3.6") %>%
      as.data.frame()
    if (level=="Adm2") {
      gadm_reg <- gadm_reg %>% select(GID_1, NAME_1, GID_2, NAME_2)
      colnames(gadm_reg) <- c("GID_1", "Adm1", "GID_2", "Adm2")
    } else {
      gadm_reg <- gadm_reg %>% select(GID_1, NAME_1)
      colnames(gadm_reg) <- c("GID_1", "Adm1")
    }
    data_merged <- left_join(data, gadm_reg)
    
    # Add to dataframe
    all_dat <- rbind(all_dat, data_merged)
  }
  
  # Save the dataframe
  saveRDS(all_dat, file="data/all_dat.rds")
  
  # Return the dataframe
  all_dat
}
