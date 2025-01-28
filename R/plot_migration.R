

plot_migration <- function(all_dat) {
  # Get meta data
  meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
    filter(exclude==0) %>%
    arrange(iso)
  
  # Create variable related to moving
  all_dat <- all_dat %>%
    mutate(
      # Create variable indicating if woman has moved region
      moved = case_when(
        is.na(al_lived) ~ "Unknown", # Unknown if woman moved
        al_lived==1 ~ "No", # Woman always lived in same location
        al_lived==0 & age-yrs_lived<13 ~ "No", # Woman moved prior to being at risk of child marriage
        al_lived==0 & Adm1==Prev_Adm1 ~ "No", # Woman has moved, but moved within same region
        al_lived==0 & is.na(Prev_Adm1) ~ "Unknown", # Woman has moved, but to unknown region
        Adm1!=Prev_Adm1 ~ "Yes") # Woman moved to a different region
    )
  
  # Calculate proportion in each country
  proportions <- all_dat %>%
    group_by(iso3, moved) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(iso3) %>%
    mutate(proportion = count / sum(count)) %>%
    mutate(country = countrycode(iso3, "iso3c", "country.name")) %>%
    mutate(country = fct_rev(as.factor(country))) # Reverse the order of countries
  
  # Exclude countries where analysis was done at the Adm2 level
  proportions <- proportions %>% filter(iso3 %in% meta_dat$iso & !(iso3 %in% c("BGD", "MDG", "PAK")))
  
  # Plot the data
  plot <- ggplot(proportions, aes(x = fct_rev(country), y = proportion, fill = moved)) +
    geom_bar(stat = "identity", position = "fill") + # Use "fill" to stack bars proportionally
    coord_flip() + # Flip coordinates for horizontal bars
    labs(x = NULL, y = "Proportion of sample", fill = "Moved") +
    scale_fill_manual(
      values = c(
        "Yes" = "#D55E00",      # Dark orange
        "No" = "#80B1D3",       # Muted blue
        "Unknown" = "#999999"   # Neutral grey
      )
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.y = element_text(face = "bold"),
      legend.position = "right"
    )
  ggsave("figures/migration.jpeg", plot = plot, height = 5, width = 5)
}


