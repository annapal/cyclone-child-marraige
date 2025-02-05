
# Create directory if needed
dir.create("figures", showWarnings = FALSE)

# Merge all the datafiles
file_list <- list.files(path = "data/merged_dat", pattern = "*.rds", full.names = TRUE)
data_list <- lapply(file_list, readRDS)
merged_data <- bind_rows(data_list)

# Average annual probability of marriage
prob_mar <- merged_data %>%
  group_by(iso3) %>%
  summarise(prob = weighted.mean(married, Denorm_Wt)) %>%
  ungroup() %>%
  mutate(country = countrycode(iso3, "iso3c", "country.name"),
         region = countrycode(iso3, "iso3c", "region")) %>%
  arrange(region, country)

# Plot probability of marriage
plot_a <- ggplot(prob_mar, aes(x=fct_rev(as.factor(country)), y=prob)) +
  geom_bar(stat = "identity", position="stack") +
  coord_flip() + 
  labs(x="", y="Avg. annual prob of marriage") +
  ggtitle(~bold("a.")) +
  theme_minimal()
ggsave("figures/plot_a.jpeg", plot_a, heigh=7, width=5)

# Calculate number of py exposed
prop_exp <- merged_data %>%
  group_by(iso3, cyclone2) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup() %>%
  mutate(country = countrycode(iso3, "iso3c", "country.name"),
         region = countrycode(iso3, "iso3c", "region")) %>%
  arrange(region, country)

# Plot proportion exposed
plot_b <- ggplot(prop_exp, aes(x=fct_rev(as.factor(country)), y=proportion, fill=factor(cyclone2))) +
  geom_bar(stat = "identity", position="stack") +
  coord_flip() + 
  scale_fill_manual(values=c("#E6E6FA", "#4B0082")) +
  labs(x="", y="Proportion of person-years", fill="Exposed") +
  ggtitle(~bold("b.")) +
  theme_minimal()
ggsave("figures/plot_b.jpeg", plot_b, heigh=7, width=5)
