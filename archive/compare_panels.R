
# Get countries that can be included
iso_inc <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
  filter(level!="NA")

# Load in EM-DAT data
emdat <- read_excel("data/emdat/emdat_updated.xlsx")

for (i in 1:nrow(iso_inc)) {
 
  iso <- iso_inc$iso[i]
  level <- as.numeric(iso_inc$level[i])
  
  # Get region names
  reg_names <- gadm(iso, version="3.6", level=level, path="data")
  if (level==2) {
    reg_names <- reg_names$GID_2
  } else {
    reg_names <- reg_names$GID_1
  }
  
  # Import the panel datasets
  if (file.exists(paste0("data/panel_dat_emdat/", iso, ".rds"))) {
    panel_emdat <- readRDS(paste0("data/panel_dat_emdat/", iso, ".rds")) %>%
      select(-event_no)
  } else {
    panel_emdat <- data.frame(iso=iso, reg=reg_names,
                            year=rep(1980:2015, each=length(reg_names)),
                            cyclone=0)
  }
  
  if (file.exists(paste0("data/panel_dat_64/", iso, ".rds"))) {
    panel_64 <- readRDS(paste0("data/panel_dat_64/", iso, ".rds"))
    if (level==2) {
      panel_64 <- panel_64 %>% select(GID_0, GID_2, year, cyclone)
    } else {
      panel_64 <- panel_64 %>% select(GID_0, GID_1, year, cyclone)
    }
    colnames(panel_64) <- c("iso", "reg", "year", "cyclone_64")
  } else {
    panel_64 <- data.frame(iso=iso, reg=reg_names,
                              year=rep(1980:2015, each=length(reg_names)),
                              cyclone_64=0)
  }
  
  if (file.exists(paste0("data/panel_dat_34/", iso, ".rds"))) {
    panel_34 <- readRDS(paste0("data/panel_dat_34/", iso, ".rds"))
    if (level==2) {
      panel_34 <- panel_34 %>% select(GID_0, GID_2, year, cyclone)
    } else {
      panel_34 <- panel_34 %>% select(GID_0, GID_1, year, cyclone)
    }
    colnames(panel_34) <- c("iso", "reg", "year", "cyclone_34")
  } else {
    panel_34 <- data.frame(iso=iso, reg=reg_names,
                              year=rep(1980:2015, each=length(reg_names)),
                              cyclone_34=0)
  }
  
  # Merge all panel datasets together
  panel_all <- full_join(panel_64, panel_34)
  panel_all <- full_join(panel_emdat, panel_all)
  
  # Create variable showing agreement
  panel_all <- panel_all %>%
    mutate(condition_status = case_when(
      cyclone == 1 & cyclone_34 == 1 & cyclone_64 == 1 ~ "All == 1",
      cyclone == 1 & cyclone_34 == 1 ~ "EMDAT & Cyclone_34 == 1",
      cyclone_34 == 1 & cyclone_64 == 1 ~ "Cyclone_34 & Cyclone_64 == 1",
      cyclone == 1 ~ "EMDAT == 1",
      cyclone_34 == 1 ~ "Cyclone_34 == 1",
      TRUE ~ "No Cyclone"  # Default case when no condition is met
    ))
  table(panel_all$condition_status)
  
  # Add variable showing year where EMDAT drought occurred, but no spatial
  emdat_no_gdis <- emdat %>% filter(ISO==iso & gdis==0)
  panel_all$emdat_missing <- ifelse(panel_all$year %in% emdat_no_gdis$`Start Year`, 1, 0)
  
  # Create factor
  panel_all$condition_status <- factor(panel_all$condition_status, 
                                       levels = c("All == 1", "EMDAT & Cyclone_34 == 1",
                                                  "Cyclone_34 & Cyclone_64 == 1",
                                                  "EMDAT == 1", "Cyclone_34 == 1",
                                                  "No Cyclone"))
  
  # Create panel plot
  p <- ggplot(panel_all, aes(x = year, y = reg, fill = condition_status)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("All == 1" = "darkred", 
                                 "EMDAT & Cyclone_34 == 1" = "pink",
                                 "Cyclone_34 & Cyclone_64 == 1" = "darkgreen", 
                                 "EMDAT == 1" = "red", 
                                 "Cyclone_34 == 1" = "lightgreen", 
                                 "No Cyclone" = "white")) +
    labs(x = "Year", y = "Region", title = paste0(iso, ": Cyclones")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_vline(xintercept = emdat_no_gdis$`Start Year`, linetype = "dashed")
  
  # Create table
  table_data <- data.frame(
    EMDAT= c("EMDAT=1", "EMDAT=0"),
    Cyclone_64 = c(sum(panel_all$cyclone == 1 & panel_all$cyclone_64 == 1)/sum(panel_all$cyclone == 1), 
                   sum(panel_all$cyclone == 0 & panel_all$cyclone_64 == 1)/sum(panel_all$cyclone == 0)),
    Cyclone_34 = c(sum(panel_all$cyclone == 1 & panel_all$cyclone_34 == 1)/sum(panel_all$cyclone == 1), 
                   sum(panel_all$cyclone == 0 & panel_all$cyclone_34 == 1)/sum(panel_all$cyclone == 0))
  )
  
  
  # Create the table using gridExtra
  table_grob <- tableGrob(table_data)
  
  # Convert the table to a ggplot-friendly annotation using ggplotify
  p_with_table <- ggplotify::as.ggplot(table_grob)
  
  # Combine the plot and the table
  plot_combined <- grid.arrange(p, p_with_table, nrow = 2, heights=c(3,1))
  # Save the plot
  ggsave(paste0(getwd(),"/data/panel_plots_comp/", iso, ".jpeg"), plot = plot_combined,
         height = max(length(unique(panel_all$reg))/7, 5), width = 10)
  
}
