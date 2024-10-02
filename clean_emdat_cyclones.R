# Get countries that can be included
iso_inc <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
  filter(level!="NA")

# Load in EM-DAT data
emdat <- read_excel("data/emdat/emdat_public_2023_09_25_full_legacy.xlsx", skip=6) %>%
  filter(`Disaster Type`=="Storm" & `Disaster Subtype`=="Tropical cyclone",
           `Start Year`%in%1980:2015 & ISO%in%iso_inc$iso)

# Load in GDIS data
gdis <- read.csv("data/emdat/pend-gdis-1960-2018-disasterlocations.csv") %>%
  filter(disastertype=="storm" & iso3%in%iso_inc$iso)
gdis$dis_no <- paste(gdis$disasterno, gdis$iso3, sep="-")
gdis <- gdis %>% filter(dis_no %in% emdat$`Dis No`)

# Create variable indicating if cyclone is in GDIS
emdat$gdis <- ifelse(emdat$`Dis No` %in% gdis$dis_no, 1, 0)
table(emdat$gdis, useNA="always")
emdat_no_loc <- subset(emdat, gdis==0)
write_xlsx(emdat_no_loc, "data/emdat_no_loc.xlsx")

# Match Regions to GID_1 codes ---------------------------------
match_regions <- data.frame(GID_0 = gadm(gdis$iso3, level=1, version="3.6", path="data")$GID_0,
                            GID_1 = gadm(gdis$iso3, level=1, version="3.6", path="data")$GID_1,
                            NAME_1 = gadm(gdis$iso3, level=1, version="3.6", path="data")$NAME_1)

# Match GDIS to the GID_1 codes in GADM
isos <- unique(match_regions$GID_0)
gdis_matched <- data.frame()
for (i in 1:length(isos)) {
  iso <- isos[i] # Country code
  dat2 <- gdis %>% filter(iso3==iso) # Get gdis data for specific country
  dat2$NAME_1 <- dat2$adm1
  regions <- match_regions %>% filter(GID_0==iso) # Get region data for specfic country
  
  # Fuzzy join the dataframes
  result <- stringdist_join(dat2, regions, by="NAME_1", mode="left", 
                            distance_col = "dist_1", max_dist = 20)
  result <- result %>%
    group_by(NAME_1.x) %>%
    slice_min(order_by = dist_1, n = 1) %>% # Select only the closest match
    ungroup()
  gdis_matched <- rbind(gdis_matched, result) # Save to dataframe
}

# Match Regions to GID_2 codes ---------------------------------
gdis2 <- subset(gdis_matched, level>1)
match_regions <- data.frame(GID_0 = gadm(gdis2$iso3, level=2, version="3.6", path="data")$GID_0,
                            GID_1 = gadm(gdis2$iso3, level=2, version="3.6", path="data")$GID_1,
                            GID_2 = gadm(gdis2$iso3, level=2, version="3.6", path="data")$GID_2,
                            NAME_2 = gadm(gdis2$iso3, level=2, version="3.6", path="data")$NAME_2)

# Remove the word "city" as this seems to be causing some issues
gdis2$adm2 <- gsub(" City", "", gdis2$adm2)
match_regions$NAME_2 <- gsub(" City", "", match_regions$NAME_2)

# Match GDIS to the GID_2 codes in GADM
gid1s <- unique(match_regions$GID_1)
gdis_matched_2 <- data.frame()
for (i in 1:length(gid1s)) {
  gid <- gid1s[i] # Country code
  dat2 <- gdis2 %>% filter(GID_1==gid) # Get gdis data for specific region
  dat2$NAME_2 <- dat2$adm2
  regions <- match_regions %>% filter(GID_1==gid) # Get region data for specfic country
  
  # Fuzzy join the dataframes
  result <- stringdist_join(dat2, regions, by="NAME_2", mode="left", 
                            distance_col = "dist_2", max_dist = 20)
  result <- result %>%
    group_by(NAME_2.x) %>%
    slice_min(order_by = dist_2, n = 1) %>% # Select only the closest match
    ungroup()
  gdis_matched_2 <- rbind(gdis_matched_2, result) # Save to dataframe
}

# Merge the dataframes back together
gdis_matched <- gdis_matched %>% select(-`NAME_1.x`, -`NAME_1.y`, -`dist_1`)
gdis_matched <- gdis_matched %>% filter(level==1) %>% mutate(GID_2=NA)
gdis_matched_2 <- gdis_matched_2 %>% select(-`NAME_1.x`, -`NAME_1.y`, -`dist_1`,
                                            -`NAME_2.x`, -`NAME_2.y`, -`dist_2`,
                                            -`GID_0.x`, -`GID_1.x`)
gdis_matched_2 <- gdis_matched_2 %>% rename(GID_0 = GID_0.y, GID_1 = GID_1.y)
gdis_all <- rbind(gdis_matched, gdis_matched_2)

# Make panel plots --------------------------------------------------------

for (i in 1:nrow(iso_inc)) {
  iso <- iso_inc$iso[i]
  
  # Get min and max years of data for country
  min_yr <- 1980
  max_yr <- 2015
  level <- as.numeric(iso_inc$level[i])
  gid <- paste0("GID_", iso_inc$level[i])
  
  # Take subset of cyclone events that can be evaluated
  cyc_subs <- subset(emdat, ISO==iso & `Start Year` %in% min_yr:max_yr)
  
  # If there are no cyclone events, skip
  if (nrow(cyc_subs)==0) next
  
  # Get region names
  reg_names <- gadm(iso, version="3.6", level=level, path="data")
  if (level==2) {
    reg_names <- reg_names$GID_2
  } else {
    reg_names <- reg_names$GID_1
  }
  
  # Make dataframe
  panel_dat <- data.frame(iso=iso, reg=reg_names,
                          year=rep(min_yr:max_yr, each=length(reg_names)),
                          cyclone=0, event_no=NA)
  
  for (j in 1:nrow(cyc_subs)) {
    
    # If cyclone event isn't in GDIS, skip
    if (cyc_subs$gdis[j]==0) next
    
    dis_no <- cyc_subs$`Dis No`[j]
    event_no <- substr(cyc_subs$`Dis No`[j], 1, 9) # EMDAT disaster number
    event_start <- cyc_subs$`Start Year`[j] # Year of event start
    event_end <- cyc_subs$`End Year`[j] # Year of event end
    
    # Get regions that are exposed
    gdis_matched_subs <- gdis_all %>% filter(disasterno==event_no & iso3==iso)
    if (level==1) {
      treated <- unique(gdis_matched_subs$GID_1)
    } else {
      treated <- c()
      for (k in 1:nrow(gdis_matched_subs)) {
        if (gdis_matched_subs$level[k]>=2) {
          treated <- c(treated, unique(gdis_matched_subs$GID_2[k]))
        } else {
          # gid1 <- gdis_matched_subs$GID_1[k]
          # treated <- c(treated, match_regions[match_regions$GID_1==gid1, "GID_2"])
          emdat[emdat$`Dis No`==dis_no,"gdis"] <- 0
        }
      }
    }
    
    # Apply indicator for cyclone region
    panel_dat[(panel_dat$reg %in% treated) & (panel_dat$year %in% event_start:event_end),
              "cyclone"] <- 1
    panel_dat[(panel_dat$reg %in% treated) & (panel_dat$year %in% event_start:event_end),
              "event_no"] <- event_no
  }
  
  # Save the panel data
  saveRDS(panel_dat, file=paste0("data/panel_dat_emdat/", iso, ".rds"))
  
  # Get years of events that are not in GDIS
  emdat_no_gdis <- emdat %>% filter(ISO==iso & gdis==0)
  
  # Plot the panel
  panel_dat$cyclone <- factor(panel_dat$cyclone, levels = c(0, 1), labels = c("No Cyclone", "Cyclone"))
  ggplot(panel_dat, aes(x = year, y = reg, fill = cyclone)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("lightblue", "darkblue")) +
    labs(x = "Year", y = "Region", title = paste0(iso, ": Cyclones (EM-DAT)")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_vline(xintercept = emdat_no_gdis$`Start Year`, linetype = "dashed")
  
  # Save the plot
  ggsave(paste0(getwd(),"/data/panel_plots_emdat/", iso, ".jpeg"),
         height = max(length(reg_names)/5, 5), width = 10)
}

# Save a copy of the updated emdat spreadsheet
write_xlsx(emdat, path="data/emdat/emdat_updated.xlsx")
