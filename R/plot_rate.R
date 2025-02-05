
# Plot the rate of child marriage in each region

plot_rate <- function(all_dat) {
  meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
    filter(exclude==0)
  meta_dat$country <- countrycode(meta_dat$iso, "iso3c", "country.name")
  meta_dat$region <- countrycode(meta_dat$iso, "iso3c", "un.region.name")
  
  # Create variable for region of analysis
  all_dat$Reg <- ifelse(all_dat$level=="Adm1", all_dat$GID_1, all_dat$GID_2)
  
  # Calculate probability
  prob <- all_dat %>%
    group_by(Reg, level) %>%
    summarise(prob = weighted.mean(married, Denorm_Wt),
              rate = prob*10000)
  
  prob_adm1 <- prob %>% filter(level=="Adm1")
  colnames(prob_adm1) <- c("GID_1", "level", "prob", "rate")
  
  prob_adm2 <- prob %>% filter(level=="Adm2")
  colnames(prob_adm2) <- c("GID_2", "level", "prob", "rate")
  
  # Get polygons
  countries <- gadm(country_codes()$ISO3, level=0, path="data", version="3.6") %>%
    st_as_sf()
  adm1_reg <- gadm(meta_dat[meta_dat$level=="Adm1", ]$iso, 
                   level=1, path="data", version="3.6") %>% st_as_sf()
  adm2_reg <- gadm(meta_dat[meta_dat$level=="Adm2", ]$iso, 
                   level=2, path="data", version="3.6") %>% st_as_sf()
  
  # Merge probability data
  adm1_reg <- left_join(adm1_reg, prob_adm1)
  adm2_reg <- left_join(adm2_reg, prob_adm2)
  
  # Plot the annual probabilities in each region
  plot <- 
    ggplot(countries) +
    geom_sf(aes(color="Excluded from study", geometry = geometry), lwd = 0) +
    scale_color_manual(values = c("Excluded from study" = "black")) +
    geom_sf(data = adm1_reg, aes(fill = rate, geometry = geometry), lwd = 0) + # Region fills
    geom_sf(data = adm2_reg, aes(fill = rate, geometry = geometry), lwd = 0) + # Region fills
    scale_fill_gradient(low = "#FBF8FC", high = "#762A83", na.value = "grey80") +
    ggtitle(~bold("a.")) +
    labs(fill = "Rate of child marriage\n(per 10,000)", color = "") +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank()) +
    geom_sf(fill = NA, color = "black", lwd = 0.1) + # Add country borders
    coord_sf(xlim = c(25, 140), ylim = c(-30, 40))
  
  # Plot Americas
  plot_americas <- 
    ggplot(countries) +
    geom_sf(aes(color = "Excluded from study", geometry = geometry), lwd = 0) +
    scale_color_manual(values = c("Excluded from study" = "black")) +
    geom_sf(data = adm1_reg, aes(fill = rate, geometry = geometry), lwd = 0) + # Region fills
    geom_sf(data = adm2_reg, aes(fill = rate, geometry = geometry), lwd = 0) + # Region fills
    scale_fill_gradient(low = "#FBF8FC", high = "#762A83", na.value = "grey80") +
    ggtitle(~bold("a. Americas")) +
    labs(fill = "Rate of child marriage\n(per 10,000)", color = "") +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
    ) +
    geom_sf(fill = NA, color = "black", lwd = 0.1) + # Add country borders
    annotation_scale(location = "bl", style = "ticks") +
    coord_sf(xlim = c(-95, -65), ylim = c(-5, 25))
  
  # Plot Africa
  plot_africa <- 
    ggplot(countries) +
    geom_sf(aes(color = "Excluded from study", geometry = geometry), lwd = 0) +
    scale_color_manual(values = c("Excluded from study" = "black")) +
    geom_sf(data = adm1_reg, aes(fill = rate, geometry = geometry), lwd = 0) + # Region fills
    geom_sf(data = adm2_reg, aes(fill = rate, geometry = geometry), lwd = 0) + # Region fills
    scale_fill_gradient(low = "#FBF8FC", high = "#762A83", na.value = "grey80") +
    ggtitle(~bold("b. Africa")) +
    labs(fill = "Rate of child marriage\n(per 10,000)", color = "") +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
    ) +
    geom_sf(fill = NA, color = "black", lwd = 0.1) + # Add country borders
    annotation_scale(location = "bl", style = "ticks") +
    coord_sf(xlim = c(25, 55), ylim = c(-35, -5))
  
  # Plot Asia
  plot_asia <- 
    ggplot(countries) +
    geom_sf(aes(color = "Excluded from study", geometry = geometry), lwd = 0) +
    scale_color_manual(values = c("Excluded from study" = "black")) +
    geom_sf(data = adm1_reg, aes(fill = rate, geometry = geometry), lwd = 0) + # Region fills
    geom_sf(data = adm2_reg, aes(fill = rate, geometry = geometry), lwd = 0) + # Region fills
    scale_fill_gradient(low = "#FBF8FC", high = "#762A83", na.value = "grey80") +
    ggtitle(~bold("c. Asia")) +
    labs(fill = "Rate of child marriage\n(per 10,000)", color = "") +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
    ) +
    geom_sf(fill = NA, color = "black", lwd = 0.1) + # Add country borders
    annotation_scale(location = "bl", style = "ticks") +
    coord_sf(xlim = c(60, 140), ylim = c(-10, 35))

  # Save the plot
  ggsave("figures/prob_map_americas.jpeg", plot_americas, width = 6, height = 6, dpi= 600)
  ggsave("figures/prob_map_africa.jpeg", plot_africa, width = 6, height = 6, dpi= 600)
  ggsave("figures/prob_map_asia.jpeg", plot_asia, width = 10, height = 6, dpi= 600)
}


