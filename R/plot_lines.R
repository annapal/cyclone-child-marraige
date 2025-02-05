
meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
  filter(exclude==0)
meta_dat$country <- countrycode(meta_dat$iso, "iso3c", "country.name")
meta_dat$region <- countrycode(meta_dat$iso, "iso3c", "un.region.name")

# Get aggregate data
agg_data <- all_dat %>%
  filter(iso3 %in% meta_dat$iso) %>%
  group_by(iso3, year) %>%
  summarise(prob = weighted.mean(married, Denorm_Wt),
            rate = prob*10000)
agg_data$country <- countrycode(agg_data$iso3, "iso3c", "country.name")
agg_data$region <- countrycode(agg_data$iso3, "iso3c", "un.region.name")

label_data <- agg_data %>%
  group_by(country) %>%
  filter(year == max(year))

ggplot(agg_data, aes(x = year, y = rate, group = country)) +
  geom_line(linewidth = 0.6, alpha = 0.8, color = "black") + 
  labs(x = "Year", y = "Rate of Child Marriage (per 10,000)") +
  theme_minimal(base_size = 14) +  
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  facet_wrap(~country, ncol = 4)
ggsave("figures/line_plot.jpeg", height=9, width=8)  


agg_data_bgd <- agg_data %>% filter(iso3 %in% c("BGD", "MDG"))
agg_data_bgd$country2 <- paste(rep(c("e. ", "f. "), each=36), agg_data_bgd$country)
ggplot(agg_data_bgd, aes(x = year, y = rate, group = country)) +
  geom_line(linewidth = 0.6, alpha = 0.8, color = "black") + 
  labs(x = "Year", y = "Rate of Child Marriage\n(per 10,000)") +
  theme_minimal(base_size = 14) +  
  ylim(0, 2500) +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.text = element_text(size = 12, color = "black"),
    strip.text = element_text(size = 14, face = "bold", hjust = 0)
  ) +
  facet_wrap(~country2)
ggsave("figures/line_main.jpeg", width = 9, height = 3, dpi= 600)
