
# Create directory if needed
dir.create("figures", showWarnings = FALSE)

# Plot pt trends test for linear and logit
results_etwfe_pt <- read_excel("results/results_etwfe_pt.xlsx")
ggplot(results_etwfe_pt, aes(x=coef, y=cohort)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper)) +
  xlim(-0.15, 0.15) +
  facet_wrap(~iso)

# Save the plot
ggsave("figures/pt.jpeg")
