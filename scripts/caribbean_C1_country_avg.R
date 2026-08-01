library(readr)
library(sf)
library(dplyr)  
library(tidyr)
library(ggplot2) 

# DATA


mhews_C1_gap_long <- read_csv("/data/mhews_C1_gap_long.csv")

mhews_coverage_summary_long <- read_csv("/data/mhews_coverage_summary_long.csv")

## Country polygon 
countries <- st_read("~/packages/Natural_Earth_quick_start/10m_cultural/ne_10m_admin_0_countries.shp")


## Join and add coverage ratio column
dt <- mhews_coverage_summary_long %>%
  left_join(mhews_C1_gap_long, by = c("country", "iso3", "hazard_category"))%>%
  mutate(n_hazard_types_with_C1 = replace_na(n_hazard_types_with_C1, 0)) %>%
  mutate(C1_coverage_ratio = n_hazard_types_with_C1 / n_hazard_types_covered)

summary(dt$C1_coverage_ratio)
mean(dt$C1_coverage_ratio == 0) 

##  Average by country separately 
country_avg <- dt %>%
  group_by(country, iso3) %>%
  summarise(mean_ratio_country = mean(C1_coverage_ratio), .groups = "drop") %>%
  arrange(desc(mean_ratio_country))

## Join
map_data1 <- countries %>%
  left_join(country_avg, by = c("ISO_A3" = "iso3"))

# PLOT
ggplot(map_data1) +
  geom_sf(aes(fill = mean_ratio_country), color = "grey40") +
  coord_sf(xlim = c(-90, -52), ylim = c(0, 27)) +
  scale_fill_viridis_c(option = "C", na.value = "grey90", limits = c(0,1), name = "C1 coverage\nratio") +
  theme_minimal() +
  labs(title = "Disaster risk knowledge (C1) coverage - Caribbean")

