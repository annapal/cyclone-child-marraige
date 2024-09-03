
# Plot linear and logit results
results_etwfe <- read_excel("results/results_etwfe.xlsx")
results_etwfe$mod <- "linear"

results_etwfe_logit <- read_excel("results/results_etwfe_logit.xlsx")
results_etwfe_logit$mod <- "logit"

results_all <- rbind(results_etwfe, results_etwfe_logit)
results_all$country <- countrycode(results_all$iso, "iso3c", "country.name")
results_all$country <- factor(results_all$country, levels = rev(unique(results_all$country)))

ggplot(results_all, aes(x = estimate, y = country, color = mod)) +
  geom_point(position = position_dodge(width = 0.5), shape=18, size=2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  labs(x = "ISO", y = "Estimate") +
  xlim(-0.1, 0.1) +
  theme_minimal()

# Plot pt trends test for linear and logit
results_etwfe_pt <- read_excel("results/results_etwfe_pt.xlsx")

ggplot(results_etwfe_pt, aes(x=coef, y=cohort)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper)) +
  xlim(-0.15, 0.15) +
  facet_wrap(~iso)
