
# Missing birth plot -------------------------------------------------------

# Get meta data
meta_dat <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
  filter(exclude==0) %>%
  arrange(iso)

imp_colors <- c(
  "Month & Year Specified" = "#556B2F",      # Dark Olive Green (subdued olive tone)
  "Month & Age Specified, Year Missing" = "#4682B4",  # Steel Blue (muted blue)
  "Year & Age Specified, Month Missing" = "#5F9EA0",  # Cadet Blue (subtle aquamarine tone)
  "Year Specified, Month & Age Missing" = "#6A5ACD",  # Slate Blue (more natural purple)
  "Age Specified, Month & Year Missing" = "#DAA520",  # Goldenrod (slightly darker)
  "Month Specified, Age & Year Missing" = "#FF8C69",  # Dark Salmon (muted pink-orange)
  "Month, Age & Year Missing" = "#A52A2A"   # Brown (subdued red tone)
)

cross_section <- all_dat %>%
  group_by(case_id) %>%
  slice(1) %>%  # Take the first row for each case_id group
  ungroup()

proportions <- cross_section %>%
  group_by(iso3, birth_imp) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(iso3) %>% 
  mutate(total = sum(count),
         proportion = count / total,
         country = countrycode(iso3, "iso3c", "country.name"))

proportions <- proportions %>%
  mutate(birth_imp_label = case_when(
    birth_imp == 1 ~ "Month & Year Specified",
    birth_imp == 2 ~ "Month & Age Specified, Year Missing",
    birth_imp == 3 ~ "Year & Age Specified, Month Missing",
    birth_imp == 4 ~ "Year & Age Specified, Month Missing",
    birth_imp == 5 ~ "Year Specified, Month & Age Missing",
    birth_imp == 6 ~ "Age Specified, Month & Year Missing",
    birth_imp == 7 ~ "Month Specified, Age & Year Missing",
    birth_imp == 8 ~ "Month, Age & Year Missing",
    TRUE ~ as.character(birth_imp)  # Default case, keep original if not matched
  ))

proportions <- proportions %>% filter(iso3 %in% meta_dat$iso)
ggplot(proportions, aes(x = proportion, y = fct_rev(factor(country)), fill = birth_imp_label)) +
  geom_bar(stat = "identity", position = "stack") +  # Create a stacked bar plot
  labs(x = "Proportion of respondents", y = "", title = "Missing birth dates", 
       fill = "") +
  theme_minimal() +
  scale_fill_manual(values = imp_colors) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none") 
ggsave("figures/missing_birth.jpeg", width = 3.4, height = 5)

# Missing marriage plot -------------------------------------------------------

proportions2 <- cross_section %>%
  filter(!is.na(mar_imp)) %>%
  group_by(iso3, mar_imp) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(iso3) %>% 
  mutate(total = sum(count),
         proportion = count / total,
         country = countrycode(iso3, "iso3c", "country.name"))

proportions2 <- proportions2 %>%
  mutate(mar_imp_label = case_when(
    mar_imp == 1 ~ "Month & Year Specified",
    mar_imp == 2 ~ "Month & Age Specified, Year Missing",
    mar_imp == 3 ~ "Year & Age Specified, Month Missing",
    mar_imp == 5 ~ "Year Specified, Month & Age Missing",
    mar_imp == 6 ~ "Age Specified, Month & Year Missing",
    mar_imp == 7 ~ "Month Specified, Age & Year Missing",
    mar_imp == 8 ~ "Month, Age & Year Missing",
    TRUE ~ as.character(mar_imp)  # Default case, keep original if not matched
  ))

proportions2 <- proportions2 %>% filter(iso3 %in% meta_dat$iso)
ggplot(proportions2, aes(x = proportion, y = fct_rev(factor(country)), fill = mar_imp_label)) +
  geom_bar(stat = "identity", position = "stack") +  # Create a stacked bar plot
  labs(x = "Proportion of respondents", y = "", title = "Missing marriage dates",
       fill = "") +
  theme_minimal() +
  scale_fill_manual(values = imp_colors) +
  theme(panel.grid.minor = element_blank())
ggsave("figures/missing_marriage.jpeg", width = 6, height = 5)
