
# Run analysis using GPS coordinates

run_analysis_gps <- function() {

  # Get meta data
  meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
    filter(exclude==0) %>%
    arrange(iso)
  iso_adm1 <- meta_dat$iso[meta_dat$level == "Adm1"] # Countries at Adm1 level
  iso_adm2 <- meta_dat$iso[meta_dat$level == "Adm2"] # Countries at Adm2 level
  
  # Read in DHS/MICS data
  all_dat <- readRDS("data/all_dat.rds")
  
  # Add GID code
  all_dat$gid <- ifelse(all_dat$iso3 %in% iso_adm1, all_dat$GID_1, all_dat$GID_2)
  
  # Get TC data
  files <- list.files("data/TC_data")
  filtered_files <- files[grepl("^(198[0-9]|199[0-9]|200[0-9]|201[0-5])", files)]
  
  # Main dataframe to store all the results
  results_all <- data.frame()
  
  for (iso_cde in meta_dat$iso) {
  
    # Get only the TC data relevant to the country
    cy_dat_all <- data.frame()
    for (i in 1:length(filtered_files)) {
      cy_dat <- read.csv(paste0("data/TC_data/", filtered_files[i])) %>%
        filter(ISO==iso_cde)
      if (nrow(cy_dat)>0) {
        cy_dat$year <- as.numeric(substr(filtered_files[i], 1, 4))
        cy_dat_all <- rbind(cy_dat_all, cy_dat)
      }
    }
    
    # Where there are multiple measurements in the same year take the max
    cy_dat_all <- cy_dat_all %>%
      group_by(LAT, LON, year) %>%
      summarise(windspeed = max(windspeed, na.rm = TRUE), .groups = "drop")
    
    # Filter MICS/DHS data for that country
    dat <- all_dat %>% filter(iso3 == iso_cde & !is.na(lat) & !is.na(long) & year%in%1983:2015)
    if (nrow(dat)==0) {next} # If there are not GPS coordinate data, then go to next country
    
    # Number of lags
    n <- 3
    
    # List to store all the data
    dat_list <- list()
    
    for (i in 0:n) {
    
      # Dataframe to store windspeed measurements that align with DHS/MICS lat/lon
      dat_coords_all <- data.frame()
      
      # Iterate over years
      for (yr in 1983:2015) {
        # Get TCE-DAT data for year of the lag
        cy_dat_yr <- cy_dat_all[cy_dat_all$year==yr-i, c("LON", "LAT", "windspeed")]
        
        # Get coordinate data for that year
        dat_coords <- dat %>% filter(year==yr) %>%
          select(long, lat, year, case_id)
        
        if (nrow(cy_dat_yr)>2) {
          # Convert to raster
          raster_data <- terra::rast(
            x = cy_dat_yr,
            type = "xyz",
            crs = "EPSG:4326"
          )
          
          # Extract windspeed
          dat_vect <- vect(dat_coords, geom = c("long", "lat"), crs = "EPSG:4326")
          extracted_data <- terra::extract(raster_data, dat_vect, method = "simple")
          extracted_data <- cbind(dat_coords, windspeed = extracted_data[, -1])
          
          # Merge back with dat_coord data
          dat_coords <- left_join(dat_coords, extracted_data, by = c("case_id", "long", "lat", "year"))
          dat_coords$windspeed_ms <- dat_coords$windspeed*0.514444
          
          # Convert NA values to 0
          dat_coords$windspeed <- ifelse(is.na(dat_coords$windspeed), 0, dat_coords$windspeed)
          dat_coords$windspeed_ms <- ifelse(is.na(dat_coords$windspeed_ms), 0, dat_coords$windspeed_ms)
          
        } else {
          # If there is no TCE-DAT for that year, set windspeed values to 0
          dat_coords$windspeed <- 0
          dat_coords$windspeed_ms <- 0
        }
        
        # Rename the columns
        dat_coords <- dat_coords %>%
          rename(!!paste0("windsp_ms_", i) := windspeed_ms,
                 !!paste0("windsp_", i) := windspeed,)
        
        # Store the data
        dat_coords_all <- rbind(dat_coords_all, dat_coords)
      }
      
      # Store data to list
      dat_list[[i+1]] <- dat_coords_all
    }
    
    # Merge the lagged data
    dat_list_merged <- reduce(dat_list, left_join, by = c("long", "lat", "year", "case_id")) %>%
      select(-windsp_0, -windsp_1, -windsp_2, -windsp_3)
    if (sum(dat_list_merged$windsp_ms_0)==0) {next} # Go to next country if no TC exposure
    
    # Merge with analysis data
    dat <- left_join(dat, dat_list_merged)
    
    # Run the analysis
    dat$gid_cde <- as.numeric(factor(dat$gid)) # Give GID a numeric code
    mod <- feols(married ~ windsp_ms_0 + windsp_ms_1 + windsp_ms_2 + windsp_ms_3 +
                   i(gid_cde, year, ref=1)|
                   gid + year + age_turned + rural,
                 data=dat, vcov=~gid, weights=~Denorm_Wt)
    
    # Get exposure response curve
    coefficients <- coef(mod)
    ses <- se(mod)
    windsp_vars <- grepl("windsp_ms", names(coefficients))
    windsp_coeffs <- coefficients[windsp_vars]
    windsp_se <- ses[windsp_vars]
    curve_results <- data.frame(coefs = windsp_coeffs, se = windsp_se,
                                lb = windsp_coeffs - 2*windsp_se,
                                ub = windsp_coeffs + 2*windsp_se,
                                iso = iso_cde, type = "marginal",
                                year = 0:3)
    
    # Get cumulative effect
    vcov_matrix <- vcov(mod)
    windsp_vcov <- vcov_matrix[windsp_vars, windsp_vars]
    aggregated_coeff <- cumsum(windsp_coeffs)
    block_cumsum <- sapply(1:nrow(windsp_vcov), function(k) {
      sum(windsp_vcov[1:k, 1:k])
    })
    aggregated_se <- sqrt(block_cumsum)
    agg_results <- data.frame(coefs = aggregated_coeff, se = aggregated_se,
                              lb = aggregated_coeff - 2*aggregated_se,
                              ub = aggregated_coeff + 2*aggregated_se,
                              iso = iso_cde, type = "cumulative", year = 0:3)
    
    # Put results together
    results <- rbind(curve_results, agg_results)
    
    # Save to main dataframe
    results_all <- rbind(results_all, results)
  }
  
  # Get country name
  results_all$country <- countrycode(results_all$iso, "iso3c", "country.name")
  results_all$region <- countrycode(results_all$iso, "iso3c", "region")
  
  # Save the results
  write_xlsx(results_all, "results/results_gps.xlsx")
  
  # Plot the results
  plot <- ggplot(results_all, aes(x = year, y = coefs*10000, color = type)) +
    geom_line(linewidth = 0.5) +                             
    geom_ribbon(aes(ymin = lb*10000, ymax = ub*10000, fill = type), alpha = 0.2, color = NA) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.25) +
    labs(
      x = "Years since tropical cyclone",
      y = "Change in the annual rate of child marriage\n(per 10,000 per m/s)"
    ) +
    theme_minimal() + 
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(),
      axis.ticks.length = unit(3, "pt"),
      axis.ticks = element_line(color = "black", linewidth = 0.25),
      plot.title = element_text(face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      strip.text = element_text(hjust = 0)
    ) +
    facet_wrap(~country, ncol=4) +
    coord_cartesian(ylim = c(-50, 50)) +
    scale_color_manual(
      values = c("#2166AC","#B2182B"),
      name = "Effect Type"
    ) +
    scale_fill_manual(
      values = c("#2166AC","#B2182B"),
      name = "Effect Type"
    )
  
  ggsave("figures/gps.jpeg", plot = plot)
}

