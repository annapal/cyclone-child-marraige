
plot_child_marriage <- function(all_dat) {

  # Plot maps ---------------------------------------------------------------
  
  meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
    filter(exclude==0)
  
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
  
  # Base plot
  plot <- ggplot(countries) +
    geom_sf(fill = "grey80", color = NA) +
    geom_sf(data = adm1_reg, aes(fill = rate, geometry = geometry), lwd = 0) + 
    geom_sf(data = adm2_reg, aes(fill = rate, geometry = geometry), lwd = 0) + 
    scale_fill_gradient(low = "#FBF8FC", high = "#762A83", na.value = "grey80") +
    labs(fill = "Rate\n(per 10,000)") +
    theme_minimal() +
    theme(
      title = element_text(size = 10),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank()
    ) +
    geom_sf(fill = NA, color = "black", lwd = 0.1) + 
    annotation_scale(location = "bl", style = "ticks")
  
  # Regional plots
  plot_americas <- plot + 
    coord_sf(xlim = c(-95, -65), ylim = c(-5, 25)) + 
    ggtitle(~bold("b. Americas")) +
    theme(legend.position = "none")
  
  plot_africa <- plot + 
    coord_sf(xlim = c(25, 55), ylim = c(-35, -5)) +
    ggtitle(~bold("c. Africa")) +
    theme(legend.position = "none")
  
  plot_asia <- plot +  
    coord_sf(xlim = c(60, 160), ylim = c(-10, 40)) +
    ggtitle(~bold("a. Asia")) +
    # theme(legend.position = "none")
    theme(
      legend.position = "inside",
      legend.justification.inside = c(1, 0.5)
    )
  
  # Bar plot ----------------------------------------------------------------
  
  prob <- all_dat %>%
    filter(iso3 %in% meta_dat$iso) %>%
    group_by(iso3) %>%
    summarise(prob = weighted.mean(married, Denorm_Wt)) %>%
    mutate(rate = prob*10000,
           country = countrycode(iso3, "iso3c", "country.name"))
  
  p2 <- ggplot(prob, aes(x = fct_rev(country), y = rate)) +
    geom_bar(stat = "identity", fill = "black", width = 0.7) +  # Black bars, narrower width for a polished look
    labs(y = "Rate (per 10,000)", x = NULL, title=~bold("d.")) +  
    scale_y_continuous(expand = c(0, 0)) +  
    coord_flip() +  
    theme_minimal() +
    theme(
      title = element_text(size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(linewidth = 0.5, color = "black"),
      axis.ticks.x = element_line(linewidth = 0.5, color = "black"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 8, color = "black"),
      legend.position = "none"  # Remove legend if unnecessary
    )
  
  # Line plots --------------------------------------------------------------
  
  # Get aggregate data
  agg_data <- all_dat %>%
    filter(iso3 %in% meta_dat$iso) %>%
    group_by(iso3, year) %>%
    summarise(prob = weighted.mean(married, Denorm_Wt),
              rate = prob*10000)
  
  # Plot Bangladesh
  agg_data_bgd <- agg_data %>% filter(iso3=="BGD")
  bgd <- ggplot(agg_data_bgd, aes(x = year, y = rate)) +
    geom_line(linewidth = 0.6, alpha = 0.8, color = "black") + 
    labs(x = "Year", y = "Rate (per 10,000)", title=~bold("e. Bangladesh")) +
    theme_minimal(base_size = 14) +  
    ylim(0, 2500) +
    theme(
      title = element_text(size = 10),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 8, color = "black")
    )
  
  # Plot Madagascar
  agg_data_mdg <- agg_data %>% filter(iso3=="MDG")
  mdg <- ggplot(agg_data_mdg, aes(x = year, y = rate)) +
    geom_line(linewidth = 0.6, alpha = 0.8, color = "black") + 
    labs(x = "Year", y = "Rate (per 10,000)", title=~bold("f. Madagascar")) +
    theme_minimal(base_size = 14) +  
    ylim(0, 2500) +
    theme(
      title = element_text(size = 10),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 8, color = "black")
    )
  
  # Arrange plots -----------------------------------------------------------
  
  plot_asia <- plot_asia + theme(plot.margin = margin(0, 0, 0, 0))
  plot_americas <- plot_americas + theme(plot.margin = margin(0, 0, 0, 0))
  plot_africa <- plot_africa + theme(plot.margin = margin(0, 0, 0, 0))
  
  line_plots <- plot_grid(bgd, mdg, ncol=1, align = "v", vjust = 1.5)
  row_1 <- plot_grid(plot_americas, plot_africa, line_plots, ncol=3, align = "h")
  row_2 <- plot_grid(ggdraw(plot_asia), p2, ncol=2, rel_widths = c(2, 1), align = "h")
  final_plot <- plot_grid(row_2, row_1, ncol=1)
  
  ggsave("figures/child_marriage_main.jpeg", final_plot, width = 10, height = 8)
  
}