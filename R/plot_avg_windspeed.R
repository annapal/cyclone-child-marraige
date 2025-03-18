
plot_avg_windspeed <- function() {
  
  # Get the data
  meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
    filter(exclude==0)
  all_dat_merged <- readRDS("data/all_dat_merged.rds")
  
  # Get aggregate data
  agg_data <- all_dat_merged %>%
    filter(iso3 %in% meta_dat$iso) %>%
    group_by(iso3, year) %>%
    summarise(windsp = weighted.mean(windsp_ms, Denorm_Wt))
  agg_data$country <- countrycode(agg_data$iso3, "iso3c", "country.name")
  
  # Filter out first three years (1980-1982) and reverse country order
  agg_data_filtered <- agg_data %>%
    filter(year > 1982) %>%
    mutate(country = factor(country, levels = rev(unique(country))))
  
  # Add regions
  agg_data_filtered$region <- countrycode(agg_data_filtered$iso3, origin = "iso3c", destination = "region")
  agg_data_filtered <- rbind(agg_data_filtered,
                             data.frame(
                               iso3 = "AAA",
                               year = NA,
                               windsp = NA,
                               country = paste("**", unique(agg_data_filtered$region), "**", sep=""),
                               region = unique(agg_data_filtered$region)))
  
  # Order the data
  agg_data_filtered <- agg_data_filtered %>%
    arrange(region, iso3)
  agg_data_filtered$country <- factor(agg_data_filtered$country, levels = unique(agg_data_filtered$country))
  
  # Create the plot
  ggplot(agg_data_filtered, aes(x = year, y = fct_rev(country), fill = windsp)) +
    geom_tile() +
    scale_fill_viridis_c(name = "Wind speed (m/s)", option = "D") +
    labs(x = "Year", y = "Country") +
    scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0,0)) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, color = "black"),
      axis.text.y = ggtext::element_markdown(size = 12, color = "black"),
      axis.title = element_text(size = 14),
      axis.ticks.x = element_line(color = "black", linewidth = 0.5),
      axis.ticks.y = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  ggsave("figures/avg_windsp.jpeg", height=7, width=8)
}
