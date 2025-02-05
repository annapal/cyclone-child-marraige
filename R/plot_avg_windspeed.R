
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

ggplot(agg_data_filtered, aes(x = year, y = fct_rev(country), fill = windsp)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Wind speed (m/s)", option = "D") +  # Nature-style color scale
  labs(x = "Year", y = "Country") +
  scale_x_continuous(breaks = seq(1985, 2015, by = 5)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major = element_blank(),  # Remove grid lines
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)  # Thin black border
  )
ggsave("figures/avg_windsp.jpeg", height=8, width=6)
