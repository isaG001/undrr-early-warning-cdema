####

library(readr)
library(sf)
library(dplyr)  
library(tidyr)
library(ggplot2) 

# DATA


mhews_C1_gap_long <- read_csv("data/mhews_C1_gap_long.csv")
mhews_coverage_summary_long <- read_csv("data/mhews_coverage_summary_long.csv")
## Country polygon 
countries <- st_read("packages/Natural_Earth_quick_start/10m_cultural/ne_10m_admin_0_countries.shp")

## Join and organize by hazard category
dt <- mhews_coverage_summary_long %>%
  left_join(mhews_C1_gap_long, by = c("country", "iso3", "hazard_category"))%>%
  mutate(n_hazard_types_with_C1 = replace_na(n_hazard_types_with_C1, 0)) %>%
  mutate(C1_coverage_ratio = n_hazard_types_with_C1 / n_hazard_types_covered)%>%
  group_by(hazard_category)

categories <- c("Biological", "Chemical", "Environmental", "Geological", "Hydrometeorological")

## Create country-category pairs
full_grid_cat <- countries %>%
  st_drop_geometry() %>%       
  select(ISO_A3) %>%
  distinct() %>%
  crossing(hazard_category = categories)

## Join 
full_grid_cat <- full_grid_cat %>%
  left_join(dt, by = c("ISO_A3" = "iso3", "hazard_category"))

# PLOT
map_data2 <- countries %>%
  select(ISO_A3) %>%
  left_join(full_grid_cat, by = "ISO_A3")

ggplot(map_data2) +
  geom_sf(aes(fill = C1_coverage_ratio), color = "black", linewidth = 0.15) +
  facet_wrap(~hazard_category) +
  coord_sf(xlim = c(-85, -52), ylim = c(0, 27)) +
  scale_fill_viridis_c(
    option = "C",
    na.value = "grey80",
    limits = c(0, 1),
    name = "C1 coverage\nratio"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold")) +
  labs(title = "Disaster Risk Knowledge (C1) coverage by hazard category - Caribbean")

