
plot_eff_het <- function() {

  coefs_all <- read_excel("results/coefs_all_het.xlsx")
  prop_all <- read_excel('results/prop_all_het.xlsx')
  
  # Add country name
  coefs_all$country <- countrycode(coefs_all$iso, "iso3c", "country.name")
  
  # Subset the coefficients just to the third year
  coefs_all_plot <- coefs_all %>% filter(year==3)
  
  # Change the order of the strata
  coefs_all_plot$strata <- factor(coefs_all_plot$strata)
  coefs_all_plot$strata <- fct_recode(
    coefs_all_plot$strata,
    "coastal" = "coastal",
    "post-2000" = "post-2000",
    "pre-2000" = "pre-2000",
    "high exposure" = "Q2",
    "low exposure" = "Q1",
    "rural" = "rural",
    "overall" = "all"
  )
  coefs_all_plot$strata <- fct_relevel(coefs_all_plot$strata, "coastal",
                                       "post-2000", "pre-2000", "high exposure",
                                       "low exposure", "rural", "overall")
  
  # Remove some of the data
  coefs_all_plot <- coefs_all_plot %>% filter(!(strata%in% c("low exposure", "post-2000")))
  
  plot <- ggplot(coefs_all_plot, aes(x = coefs*10000, y = strata, color = strata)) +
    geom_point(size = 2.5, shape = 18) +
    geom_errorbarh(aes(xmin = lb*10000, xmax = ub*10000), height = 0, linewidth = 0.5) +
    geom_vline(xintercept = 0, linewidth = 0.25, linetype = "dotted") +
    geom_hline(yintercept = 0, linewidth = 0.1, linetype = "solid") +
    labs(x = "Change in the annual rate of child marriage \n (per 10,000 per m/s)", y = "Estimate") +
    theme_minimal() +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(),
      axis.ticks.length = unit(3, "pt"),
      axis.ticks = element_line(color = "black", linewidth = 0.25),
      plot.title = element_text(face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      strip.text = element_text(hjust = 0),
      legend.position = "none"
    ) +
    ggforce::facet_wrap_paginate(~country, scales = "fixed", ncol = 4, labeller = labeller(country = label_wrap_gen(width = 20))) +
    scale_color_manual(values = c("#009E73", "#D55E00",  "#08306B", "#800000", "#000000"))
  ggsave("figures/main_eff_het.jpeg", plot, height=8)
}