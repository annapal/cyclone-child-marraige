
# Create directory if needed
dir.create("figures", showWarnings = FALSE)

# Get country ordering from main effects model
dat_main <- read_xlsx("results/results_etwfe.xlsx") %>%
  mutate(country = countrycode(iso, "iso3c", "country.name"),
         region = countrycode(iso, "iso3c", "region")) %>%
  arrange(region, estimate) %>%
  mutate(
    region_change = c(0, as.numeric(region[-1] != region[-n()])),
    region_cumsum = cumsum(region_change),
    ID = row_number() + region_cumsum * 1.5 # Add a space between regions
  ) %>%
  select(-region_change, -region_cumsum)

# Read in the rural/urban data and arrange it
dat <- read_xlsx("results/results_rural.xlsx") %>%
  mutate(country = countrycode(iso, "iso3c", "country.name"),
         region = countrycode(iso, "iso3c", "region")) %>%
  arrange(match(iso, dat_main$iso))

# Set position of countries on the plot
dat <- dat %>%
  mutate(
    country_change = c(0, as.numeric(country[-1] != country[-n()])),
    country_cumsum = cumsum(country_change),
    region_change = c(0, as.numeric(region[-1] != region[-n()])),
    region_cumsum = cumsum(region_change),
    ID = 1 + country_cumsum + region_cumsum * 1.5 # Add a space between regions
  )

# Add some space between the rural and urban estimates
dat$ID = dat$ID + rep(c(-0.1, 0.1), nrow(dat_main))

# Plot
p <- ggplot(dat, aes(x = estimate, y = ID, color = factor(rural))) +
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
  scale_y_continuous(breaks=dat_main$ID, labels=dat_main$country)

# Get the locations for the region labels
locations <- dat_main %>%
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
ggsave(filename = "figures/tes_rural.jpeg", plot = p, width = 5, height = 6, dpi = 300)

# # Fracet wrap plot
# p <- ggplot(dat, aes(x = estimate, y = factor(rural))) +
#   geom_point(size=1.5, shape=18) +
#   geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
#                  height = 0, linewidth = 0.5) +
#   geom_vline(xintercept = 0, linewidth = 0.25) +
#   labs(x = "Change in the prob. of marriage (95% CI)",
#        y = "") + 
#   theme(panel.grid = element_blank(),
#         panel.background = element_blank(),
#         axis.ticks.y = element_line(linewidth = 0.25),
#         axis.line.x = element_line(linewidth = 0.25),
#         axis.line.y = element_line(linewidth = 0.25),
#         axis.ticks.x = element_line(linewidth = 0.25),
#         legend.key = element_blank(),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   xlim(-0.1, 0.1) +
#   facet_wrap(~country, ncol=7,
#              labeller = labeller(country = label_wrap_gen(width = 15)))
