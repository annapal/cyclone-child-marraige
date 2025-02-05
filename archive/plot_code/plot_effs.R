
# Create directory if needed
dir.create("figures", showWarnings = FALSE)

dat <- read_xlsx("results/results_etwfe.xlsx") %>%
  mutate(country = countrycode(iso, "iso3c", "country.name"),
         region = countrycode(iso, "iso3c", "region"))

dat <- dat %>%
  arrange(region, estimate) %>%
  mutate(
    region_change = c(0, as.numeric(region[-1] != region[-n()])),
    region_cumsum = cumsum(region_change),
    ID = row_number() + region_cumsum * 1.5 # Add a space between regions
  ) %>%
  select(-region_change, -region_cumsum)

p <- ggplot(dat, aes(x = estimate, y = ID)) +
  geom_point(size=1.5, shape=18) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.5) +
  geom_vline(xintercept = 0, linewidth = 0.25, linetype="dotted") +
  labs(x = "Change in the probability of marriage (95% CI)",
       y = "") + 
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(linewidth = 0.25),
        axis.ticks.x = element_line(linewidth = 0.25),
        legend.key = element_blank()) +
  # theme_minimal() +
  scale_y_continuous(breaks=dat$ID, labels=dat$country)

# Get the locations for the region labels
locations <- dat %>%
  group_by(region) %>%
  summarize(max_ID = max(ID))

# Add global region sub-headings
p <- p +
  annotate("text",
           x = -0.1130,
           y = locations$max_ID + 1,
           label = locations$region,
           hjust = 1, fontface="bold", size=3) +
  coord_cartesian(xlim = c(-0.1, 0.1), 
                  clip = 'off')

# Save the plot
ggsave(filename = "figures/tes_main.jpeg", plot = p, width = 5, height = 6, dpi = 300)
