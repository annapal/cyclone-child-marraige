
# Run treatment effect hetereogeneity analysis

run_analysis_het <- function() {
  
  # Calculate which regions are coastal -------------------------------------
  
  meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>% filter(exclude==0)
  
  # Get subnational regions from the GADM
  reg_adm1 <- gadm(meta_dat$iso[meta_dat$level == "Adm1"], level=1, path="data", version="3.6")
  reg_adm2 <- gadm(meta_dat$iso[meta_dat$level == "Adm2"], level=2, path="data", version="3.6")
  
  # Load and reproject coastline
  coastline <- vect("data/ne_10m_coastline/ne_10m_coastline.shp")
  coastline <- project(coastline, crs(reg_adm1))
  
  # Convert both to sf
  reg_adm1_sf <- st_as_sf(reg_adm1)
  reg_adm2_sf <- st_as_sf(reg_adm2)
  coastline_sf <- st_as_sf(coastline)
  
  # Calculate intersections
  intersects_matrix <- st_intersects(reg_adm1_sf, coastline_sf, sparse = TRUE)
  intersects_matrix_2 <- st_intersects(reg_adm2_sf, coastline_sf, sparse = TRUE)
  
  # Convert to binary (1 = coastal, 0 = non-coastal)
  reg_adm1_sf$is_coastal <- as.integer(lengths(intersects_matrix) > 0)
  reg_adm2_sf$is_coastal <- as.integer(lengths(intersects_matrix_2) > 0)

  # Set up the analysis -----------------------------------------------------

  # Read in the data
  all_dat_merged <- readRDS("data/all_dat_merged.rds")
  wind_dat <- readRDS("data/wind_dat_all.rds")
  
  # Compute average windspeed in each region
  avg_windsp <- wind_dat %>%
    group_by(iso, gid) %>%
    summarise(avg_windsp_ms = mean(windsp_ms, na.rm = TRUE)) %>%
    mutate(
      quartile = ifelse(avg_windsp_ms == 0, 0, NA)
    ) %>%
    group_by(iso) %>%
    mutate(
      quartile = ifelse(is.na(quartile), 
                        ifelse(avg_windsp_ms < quantile(avg_windsp_ms[avg_windsp_ms != 0], 0.75, na.rm = TRUE), 1, 2), 
                        quartile)
    )
  all_dat_merged <- left_join(all_dat_merged, avg_windsp)
  
  # Get meta data
  meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
    filter(exclude==0) %>%
    arrange(iso)
  
  # Dataframe to store results
  coefs_all <- data.frame()
  prop_all <- data.frame()
  
  for (iso_cde in meta_dat$iso) {
    
    # Get the data for the country
    dat <- all_dat_merged %>% filter(iso == iso_cde)
    
    # Run main analysis -------------------------------------------------------
    
    # Run model
    dat$gid_cde <- as.numeric(factor(dat$gid)) # Give GID a numeric code
    mod <- feols(married ~ windsp_ms + windsp_ms_lag1 + windsp_ms_lag2 + windsp_ms_lag3 +
                   i(gid_cde, year, ref=1)|
                   gid + year + age_turned + rural,
                 data=dat, vcov=~gid, weights=~Denorm_Wt)
    
    # Get exposure response curve
    coefficients <- coef(mod)
    windsp_vars <- grepl("^windsp_ms", names(coefficients))
    windsp_coeffs <- coefficients[windsp_vars]
    
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
                              iso = iso_cde, type = "cumulative", strata = "all", year = 0:3)
    coefs_all <- rbind(coefs_all, agg_results)
    
    # Run rural analysis ------------------------------------------------------
    
    # Proportion of the population that is rural
    prop_all <- rbind(prop_all, 
                      data.frame(iso = iso_cde, type = "rural",
                        prop = sum(dat$rural)/nrow(dat)))
    
    # Run model
    dat$rural <- as.factor(dat$rural)
    mod <- feols(married ~ windsp_ms*rural + windsp_ms_lag1*rural + windsp_ms_lag2*rural + windsp_ms_lag3*rural +
                   i(gid_cde, year, ref=1)|
                   gid + year + age_turned + rural,
                 data=dat, vcov=~gid, weights=~Denorm_Wt)
    
    # Get rural coefficients
    coefficients <- coef(mod)
    windsp_vars <- grepl("rural1", names(coefficients)) & grepl("windsp_ms", names(coefficients))
    windsp_coeffs <- coefficients[windsp_vars]
    vcov_matrix <- vcov(mod)
    windsp_vcov <- vcov_matrix[windsp_vars, windsp_vars]
    aggregated_coeff <- cumsum(windsp_coeffs)
    block_cumsum <- sapply(1:nrow(windsp_vcov), function(k) {
      sum(windsp_vcov[1:k, 1:k])
    })
    aggregated_se <- sqrt(block_cumsum)
    agg_results_rural <- data.frame(coefs = aggregated_coeff, se = aggregated_se,
                              lb = aggregated_coeff - 2*aggregated_se,
                              ub = aggregated_coeff + 2*aggregated_se,
                              iso = iso_cde, type = "cumulative", strata = "rural", year = 0:3)
    coefs_all <- rbind(coefs_all, agg_results_rural)
    
    # Run year analysis ------------------------------------------------------
    
    if (!(iso_cde %in% c("COL", "CRI", "NIC", "TLS", "ZWE"))) {
      # Proportion of the population that is 2000 onwards
      prop_all <- rbind(prop_all, 
                        data.frame(iso = iso_cde, type = "2000-2015",
                                   prop = length(dat$year[dat$year>1999])/nrow(dat)))
      
      # Run model
      dat$yr_2000 <- factor(ifelse(dat$year>1999, 1, 0))
      mod <- feols(married ~ windsp_ms*yr_2000 + windsp_ms_lag1*yr_2000 + windsp_ms_lag2*yr_2000 + windsp_ms_lag3*yr_2000 +
                     i(gid_cde, year, ref=1)|
                     gid + year + age_turned + rural,
                   data=dat, vcov=~gid, weights=~Denorm_Wt)
      
      # Get post 2000 coefficients
      coefficients <- coef(mod)
      windsp_vars <- grepl("yr_20001", names(coefficients)) & grepl("windsp_ms", names(coefficients))
      windsp_coeffs <- coefficients[windsp_vars]
      vcov_matrix <- vcov(mod)
      windsp_vcov <- vcov_matrix[windsp_vars, windsp_vars]
      aggregated_coeff <- cumsum(windsp_coeffs)
      block_cumsum <- sapply(1:nrow(windsp_vcov), function(k) {
        sum(windsp_vcov[1:k, 1:k])
      })
      aggregated_se <- sqrt(block_cumsum)
      agg_results_2000 <- data.frame(coefs = aggregated_coeff, se = aggregated_se,
                                     lb = aggregated_coeff - 2*aggregated_se,
                                     ub = aggregated_coeff + 2*aggregated_se,
                                     iso = iso_cde, type = "cumulative", strata = "post-2000", year = 0:3)
      coefs_all <- rbind(coefs_all, agg_results_2000)
      
      # Get coefficients for pre-2000
      coefficients <- coef(mod)
      windsp_vars <- !(grepl("yr_20001", names(coefficients))) & grepl("windsp_ms", names(coefficients))
      windsp_coeffs <- coefficients[windsp_vars]
      vcov_matrix <- vcov(mod)
      windsp_vcov <- vcov_matrix[windsp_vars, windsp_vars]
      aggregated_coeff <- cumsum(windsp_coeffs)
      block_cumsum <- sapply(1:nrow(windsp_vcov), function(k) {
        sum(windsp_vcov[1:k, 1:k])
      })
      aggregated_se <- sqrt(block_cumsum)
      agg_results_1999 <- data.frame(coefs = aggregated_coeff, se = aggregated_se,
                                     lb = aggregated_coeff - 2*aggregated_se,
                                     ub = aggregated_coeff + 2*aggregated_se,
                                     iso = iso_cde, type = "cumulative", strata = "pre-2000", year = 0:3)
      coefs_all <- rbind(coefs_all, agg_results_1999)
    }
    
    # Run frequency of exposure analysis --------------------------------------
    
    dat$quartile <- factor(dat$quartile, levels = c(2,1,0))
    
    prop_all <- rbind(prop_all, 
                      data.frame(iso = iso_cde, type = "Q2",
                                 prop = sum(dat$quartile==2)/nrow(dat)))
    prop_all <- rbind(prop_all, 
                      data.frame(iso = iso_cde, type = "Q1",
                                 prop = sum(dat$quartile==1)/nrow(dat)))
    
    mod <- feols(married ~ windsp_ms*quartile + windsp_ms_lag1*quartile + windsp_ms_lag2*quartile + windsp_ms_lag3*quartile +
                   i(gid_cde, year, ref=1)|
                   gid + year + age_turned + rural,
                 data=dat, vcov=~gid, weights=~Denorm_Wt)
    
    # Get coefficients for Q1
    coefficients <- coef(mod)
    windsp_vars <- grepl("quartile1", names(coefficients)) & grepl("windsp_ms", names(coefficients))
    windsp_coeffs <- coefficients[windsp_vars]
    vcov_matrix <- vcov(mod)
    windsp_vcov <- vcov_matrix[windsp_vars, windsp_vars]
    aggregated_coeff <- cumsum(windsp_coeffs)
    block_cumsum <- sapply(1:nrow(windsp_vcov), function(k) {
      sum(windsp_vcov[1:k, 1:k])
    })
    aggregated_se <- sqrt(block_cumsum)
    agg_results_q1 <- data.frame(coefs = aggregated_coeff, se = aggregated_se,
                                    lb = aggregated_coeff - 2*aggregated_se,
                                    ub = aggregated_coeff + 2*aggregated_se,
                                    iso = iso_cde, type = "cumulative", strata = "Q1", year = 0:3)
    coefs_all <- rbind(coefs_all, agg_results_q1)
    
    # Get coefficients for Q2
    coefficients <- coef(mod)
    windsp_vars <- !(grepl("quartile1", names(coefficients))) & grepl("windsp_ms", names(coefficients))
    windsp_coeffs <- coefficients[windsp_vars]
    vcov_matrix <- vcov(mod)
    windsp_vcov <- vcov_matrix[windsp_vars, windsp_vars]
    aggregated_coeff <- cumsum(windsp_coeffs)
    block_cumsum <- sapply(1:nrow(windsp_vcov), function(k) {
      sum(windsp_vcov[1:k, 1:k])
    })
    aggregated_se <- sqrt(block_cumsum)
    agg_results_q2 <- data.frame(coefs = aggregated_coeff, se = aggregated_se,
                                 lb = aggregated_coeff - 2*aggregated_se,
                                 ub = aggregated_coeff + 2*aggregated_se,
                                 iso = iso_cde, type = "cumulative", strata = "Q2", year = 0:3)
    coefs_all <- rbind(coefs_all, agg_results_q2)
    
    # Run the coastal analysis ------------------------------------------------
    
    # Extract variable for coastal regions
    level <- meta_dat[meta_dat$iso==iso_cde,]$level
    if (level=="Adm2") {
      reg_dat <- reg_adm2_sf %>% filter(GID_0 == iso_cde) %>%
        select(GID_2, is_coastal) %>%
        st_drop_geometry()
    } else {
      reg_dat <- reg_adm1_sf %>% filter(GID_0 == iso_cde) %>%
        select(GID_1, is_coastal) %>%
        st_drop_geometry()
    }
    
    # Add variable for coastal regions to the data
    dat <- left_join(dat, reg_dat)
    
    # Just get coastal regions
    dat_coastal <- dat %>% filter(is_coastal==1)
    if (nrow(dat_coastal)>0) {
      # Run model
      dat_coastal$gid_cde <- as.numeric(factor(dat_coastal$gid)) # Give GID a numeric code
      mod <- feols(married ~ windsp_ms + windsp_ms_lag1 + windsp_ms_lag2 + windsp_ms_lag3 +
                     i(gid_cde, year, ref=1)|
                     gid + year + age_turned + rural,
                   data=dat_coastal, vcov=~gid, weights=~Denorm_Wt)
      
      # Get exposure response curve
      coefficients <- coef(mod)
      windsp_vars <- grepl("^windsp_ms", names(coefficients))
      windsp_coeffs <- coefficients[windsp_vars]
      
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
                                iso = iso_cde, type = "cumulative", strata = "coastal", year = 0:3)
      coefs_all <- rbind(coefs_all, agg_results)
    }
  }
  
  # Save the results
  write_xlsx(coefs_all, "results/coefs_all_het.xlsx")
  write_xlsx(prop_all, "results/prop_all_het.xlsx")
  
  # Get country name
  coefs_all$country <- countrycode(coefs_all$iso, "iso3c", "country.name")
  coefs_all$region <- countrycode(coefs_all$iso, "iso3c", "region")
  
  # Plot the rural results
  coefs_all_rural <- subset(coefs_all, strata %in% c("all", "rural"))
  ggplot(coefs_all_rural, aes(x = year, y = coefs*10000, color = strata)) +
    geom_line(linewidth = 0.5) +                             
    geom_ribbon(aes(ymin = lb*10000, ymax = ub*10000, fill = strata), alpha = 0.2, color = NA) +
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
      values = c("black","#B2182B"),
      name = "Strata"
    ) +
    scale_fill_manual(
      values = c("black","#B2182B"),
      name = "Strata"
    )
  ggsave("figures/main_rural.jpeg")
  
  # Plot the results pre and post 2000
  coefs_all_2000 <- subset(coefs_all, strata %in% c("all", "pre-2000", "post-2000"))
  ggplot(coefs_all_2000, aes(x = year, y = coefs*10000, color = strata)) +
    geom_line(linewidth = 0.5) +                             
    geom_ribbon(aes(ymin = lb*10000, ymax = ub*10000, fill = strata), alpha = 0.2, color = NA) +
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
      values = c("black","blue", "orange"),
      name = "Strata"
    ) +
    scale_fill_manual(
      values = c("black","blue", "orange"),
      name = "Strata"
    )
  ggsave("figures/main_2000.jpeg")
  
  # Plot the results by exposure
  coefs_all_q <- subset(coefs_all, strata %in% c("all", "Q1", "Q2"))
  coefs_all_q$strata <- factor(coefs_all_q$strata,
                               levels = c("all", "Q1", "Q2"),
                               labels = c("all", "low exposure", "high exposure"))
  ggplot(coefs_all_q, aes(x = year, y = coefs*10000, color = strata)) +
    geom_line(linewidth = 0.5) +                             
    geom_ribbon(aes(ymin = lb*10000, ymax = ub*10000, fill = strata), alpha = 0.2, color = NA) +
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
      values = c("black","blue", "orange"),
      name = "Strata"
    ) +
    scale_fill_manual(
      values = c("black","blue", "orange"),
      name = "Strata"
    )
  ggsave("figures/main_exposure.jpeg")
  
  # Plot the coastal results
  coefs_all_coastal <- subset(coefs_all, strata %in% c("all", "coastal"))
  ggplot(coefs_all_coastal, aes(x = year, y = coefs*10000, color = strata)) +
    geom_line(linewidth = 0.5) +                             
    geom_ribbon(aes(ymin = lb*10000, ymax = ub*10000, fill = strata), alpha = 0.2, color = NA) +
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
      values = c("black","#B2182B"),
      name = "Strata"
    ) +
    scale_fill_manual(
      values = c("black","#B2182B"),
      name = "Strata"
    )
  ggsave("figures/main_coastal.jpeg")
}