
# Plot the rate of child marriage in each region

plot_rate <- function(all_dat) {
  meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx")
  
  # Create variable for region of analysis
  all_dat$Reg <- ifelse(all_dat$level=="Adm1", all_dat$GID_1, all_dat$GID_2)
  
  # Calculate probability
  prob <- all_dat %>%
    group_by(Reg, level) %>%
    summarise(prob = weighted.mean(married, Denorm_Wt))
  
  prob_adm1 <- prob %>% filter(level=="Adm1")
  colnames(prob_adm1) <- c("GID_1", "level", "prob")
  
  prob_adm2 <- prob %>% filter(level=="Adm2")
  colnames(prob_adm2) <- c("GID_2", "level", "prob")
  
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
    geom_sf(data = adm1_reg, aes(fill = prob, geometry = geometry), lwd = 0) + # Region fills
    geom_sf(data = adm2_reg, aes(fill = prob, geometry = geometry), lwd = 0) + # Region fills
    scale_fill_gradient(low = "#FAF3FC", high = "#2E0854", na.value = "grey80") +
    ggtitle(~bold("a.")) +
    labs(fill = "Average annual probability of marriage", color = "") +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank()) +
    geom_sf(fill = NA, color = "black", lwd = 0.1) + # Add country borders
    coord_sf(xlim = c(-85, 140), ylim = c(-40, 50))
  
  # Save the plot
  ggsave("figures/prob_map.jpeg", plot, width = 9, height = 4, dpi= 600)
}


