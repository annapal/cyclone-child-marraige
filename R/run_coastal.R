
run_coastal <- function() {

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
  
  # # Plot the regions to check
  # plot <- ggplot() +
  #   geom_sf(data = reg_adm1_sf, aes(fill = is_coastal), color = "black", linewidth = 0.05) +  # Admin regions
  #   geom_sf(data = coastline_sf, color = "blue", linewidth = 0.05) +  # Coastline
  #   ggtitle("GADM Regions & Coastline") +
  #   theme_minimal()
  # ggsave("coastline_plots/all_coastal.jpeg", plot=plot, dpi=1000)
  # 
  # plot <- ggplot() +
  #   geom_sf(data = reg_adm2_sf, aes(fill = is_coastal), color = "black", linewidth = 0.05) +  # Admin regions
  #   geom_sf(data = coastline_sf, color = "blue", linewidth = 0.05) +  # Coastline
  #   ggtitle("GADM Regions & Coastline") +
  #   theme_minimal()
  # ggsave("coastline_plots/all_coastal_2.jpeg", plot=plot, dpi=1000)
  
  
  # Run the analysis --------------------------------------------------------
  
  # Read in the data
  all_dat_merged <- readRDS("data/all_dat_merged.rds")
  
  # Dataframe to store results
  coefs_all <- data.frame()
  
  for (iso_cde in meta_dat$iso) {
  
    # Get the data for the country
    dat <- all_dat_merged %>% filter(iso == iso_cde)
    
    # Run main model
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
  
  # Get country name
  coefs_all$country <- countrycode(coefs_all$iso, "iso3c", "country.name")
  coefs_all$region <- countrycode(coefs_all$iso, "iso3c", "region")
  
  ggplot(coefs_all, aes(x = year, y = coefs*10000, color = strata)) +
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

