
# Run the analysis with binary exposure of extreme events

run_analysis_binary <- function(all_dat, wind_dat) {
  # Get meta data
  meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
    filter(exclude==0) %>%
    arrange(iso)
  iso_adm1 <- meta_dat$iso[meta_dat$level == "Adm1"] # Countries at Adm1 level
  iso_adm2 <- meta_dat$iso[meta_dat$level == "Adm2"] # Countries at Adm2 level
  
  # Create binary variable if wind speed is greater than the 75th percentile
  wind_dat <- wind_dat %>%
    group_by(iso) %>%
    mutate(
      wind_thresh = quantile(windsp_ms[windsp_ms > 0], 0.75, na.rm = TRUE),
      wind_bin = ifelse(windsp_ms > wind_thresh, 1, 0)
    ) %>%
    ungroup()
  
  # Make panel plots
  for (iso3 in unique(wind_dat$iso)) {
    # Get country data
    wind_iso <- wind_dat %>% filter(iso==iso3)
    
    # Make the plot
    plot <- ggplot(wind_iso, aes(x = year, y = gid, fill = wind_bin)) +
      geom_tile(color="white") +
      scale_fill_gradientn(
        colors = c("white", "#B2182B"),
        na.value = "grey80",
      ) +
      labs(x = "Year", y = "Subnational Region", 
           title = countrycode(iso3, "iso3c", "country.name"),
           fill = "Avg. windspeed (m/s)") +
      theme_minimal() +
      scale_x_continuous(limits=c(1979, 2016), expand = c(0, 0),
                         breaks = seq(1980, 2015, by=5)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25),
            panel.grid.major = element_blank())
    
    # Save the panel plot
    ggsave(paste0("data/panel_plots_binary/", iso3, "_panel_binary.jpeg"), plot  = plot,
           # height = max(length(unique(wind_iso$gid))/10, 5), 
           height = 5,
           width = 7)
  }
  
  # Add lagged variables to wind_dat (k=3)
  wind_dat2 <- wind_dat %>%
    arrange(gid, year) %>%
    group_by(gid) %>% 
    mutate(
      wind_bin_lag1 = lag(wind_bin, 1),
      wind_bin_lag2 = lag(wind_bin, 2),
      wind_bin_lag3 = lag(wind_bin, 3)
    ) %>%
    ungroup() %>%
    na.omit()
  
  # Merge wind data with DHS data
  all_dat$gid <- ifelse(all_dat$iso3 %in% iso_adm1, all_dat$GID_1, all_dat$GID_2)
  all_dat_merged <- left_join(all_dat, wind_dat2)
  
  # Save merged data
  saveRDS(all_dat_merged, "data/all_dat_merged_bin.rds")
  
  # Main dataframe to store all the results
  results_all <- data.frame()
  
  for (iso_cde in meta_dat$iso) {
    
    # Get the data for the country
    dat <- all_dat_merged %>% filter(iso == iso_cde)

    # Run model
    dat$gid_cde <- as.numeric(factor(dat$gid)) # Give GID a numeric code
    mod <- feols(married ~ wind_bin + wind_bin_lag1 + wind_bin_lag2 + wind_bin_lag3 +
                   i(gid_cde, year, ref=1)|
                   gid + year + age_turned + rural,
                 data=dat, vcov=~gid, weights=~Denorm_Wt)
    
    # Get exposure response curve
    coefficients <- coef(mod)
    ses <- se(mod)
    windsp_vars <- grepl("^wind_bin", names(coefficients))
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
  write_xlsx(results_all, "results/results_all_bin.xlsx")
  
  # Plot the results
  plot <- ggplot(results_all, aes(x = year, y = coefs*10000, color = type)) +
    geom_line(linewidth = 0.5) +                             
    geom_ribbon(aes(ymin = lb*10000, ymax = ub*10000, fill = type), alpha = 0.2, color = NA) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.25) +
    labs(
      x = "Years since tropical cyclone",
      y = "Change in the annual rate of child marriage\n(per 10,000)"
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
    coord_cartesian(ylim = c(-700, 700)) +
    scale_color_manual(
      values = c("#2166AC","#B2182B"),
      name = "Effect Type"
    ) +
    scale_fill_manual(
      values = c("#2166AC","#B2182B"),
      name = "Effect Type"
    )
  
  ggsave("figures/main_bin.jpeg", plot = plot)
}

