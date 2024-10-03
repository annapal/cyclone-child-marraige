# Get countries that can be included
iso_inc <- read_excel("data/meta_dhs_mics_updated.xlsx") %>%
  filter(level!="NA")

# Load in GDIS data
gdis <- read.csv("data/emdat/pend-gdis-1960-2018-disasterlocations.csv") %>%
  filter(disastertype%in% c("earthquake", "flood", "lanslide", "storm") & iso3%in%iso_inc$iso)
gdis$dis_no <- paste(gdis$disasterno, gdis$iso3, sep="-")

# Load in EM-DAT data
emdat <- read_excel("data/emdat/emdat_public_2023_09_25_full_legacy.xlsx", skip=6) %>%
  filter(`Disaster Type` %in% c("Earthquake", "Flood", "Lanslide", "Storm"),
         `Start Year`%in%1980:2015 & ISO%in%iso_inc$iso)

# Create variable indicating if event is in GDIS
emdat$gdis <- ifelse(emdat$`Dis No` %in% gdis$dis_no, 1, 0)
table(emdat$gdis, useNA="always")
emdat_no_loc <- subset(emdat, gdis==0)
write_xlsx(emdat_no_loc, "gdis_add/emdat_no_loc_all.xlsx")

dat <- st_as_sf(gadm("MOZ", level=1, version="3.6", path="data"))

