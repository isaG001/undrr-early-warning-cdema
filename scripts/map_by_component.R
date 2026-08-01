#### by component 

library(readr)
library(sf)
library(dplyr)  
library(tidyr)
library(ggplot2) 
library(stringr)

# DATA
mhews_capacity_matrix_detailed <- read_csv("Desktop/project_caribb/mhews_capacity_matrix_detailed.csv")

mhews_coverage_summary_long <- read_csv("Desktop/project_caribb/mhews_coverage_summary_long.csv")

## Country polygon 
countries <- st_read("~/Desktop/packages/Natural_Earth_quick_start/10m_cultural/ne_10m_admin_0_countries.shp")

# RATIO FUNCTION
component_ratio <- function(detail_df, coverage_df, component) {
  comp_data <- detail_df %>%
    filter(country != "Regional") %>%
    filter(str_detect(components, component)) %>%
    group_by(country, iso3, hazard_category) %>%
    summarise(n_hazard_types_with_comp = n_distinct(hazard_type), .groups = "drop")
  
  coverage_df %>%
    left_join(comp_data, by = c("country", "iso3", "hazard_category")) %>%
    mutate(
      n_hazard_types_with_comp = replace_na(n_hazard_types_with_comp, 0),
      coverage_ratio = n_hazard_types_with_comp / n_hazard_types_covered,
      component = component
    )
}

# COMPONENTS 
components_list <- c("C1", "C2", "C3", "C4")

all_components <- bind_rows(
  component_ratio(mhews_capacity_matrix_detailed, mhews_coverage_summary_long, "C1"),
  component_ratio(mhews_capacity_matrix_detailed, mhews_coverage_summary_long, "C2"),
  component_ratio(mhews_capacity_matrix_detailed, mhews_coverage_summary_long, "C3"),
  component_ratio(mhews_capacity_matrix_detailed, mhews_coverage_summary_long, "C4")
)

country_component_avg <- all_components %>%
  group_by(country, iso3, component) %>%
  summarise(avg_ratio = mean(coverage_ratio), .groups = "drop")

# Create country-component pairs
full_grid_com <- countries %>%
  st_drop_geometry() %>%
  select(ISO_A3) %>%
  distinct() %>%
  crossing(component = components_list)%>%
  left_join(country_component_avg, by = c("ISO_A3" = "iso3", "component"))

# PLOT
map_data3 <- countries %>%
  select(ISO_A3) %>%
  left_join(full_grid_com, by = "ISO_A3")

ggplot(map_data3) +
  geom_sf(aes(fill = avg_ratio), color = "black", linewidth = 0.15) +
  facet_wrap(~component, ncol = 2) +
  coord_sf(xlim = c(-85, -52), ylim = c(0, 27)) +
  scale_fill_viridis_c(option = "C", na.value = "grey80", limits = c(0,1), name = "Coverage\nratio") +
  theme_minimal() +
  labs(title = "EWS Component Coverage by Country - Caribbean")


component_summary <- all_components %>%
  group_by(component) %>%
  summarise(
    mean_ratio = mean(coverage_ratio),
    pct_zero = mean(coverage_ratio == 0)
  )

