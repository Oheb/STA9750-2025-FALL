# Load libraries
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(plotly)
library(ggplot2)

#===========================================
# 1. Load the shapefile (NYC Community Districts)
#===========================================
neighborhoods <- st_read("nycdwi_25d/nycdwi.shp")
names(neighborhoods) <- tolower(names(neighborhoods))

# Inspect columns
names(neighborhoods)
# Should show: "borocd", "shape_leng", "shape_area", "geometry"

# Rename borocd → BoroCD (to match your population CSV)
neighborhoods <- neighborhoods %>%
  rename(BoroCD = borocd)

# Project to WGS84 (safe for spatial joins)
neighborhoods <- st_transform(neighborhoods, 4326)

#===========================================
# 2. Load Facilities CSV (with multipolygon WKT)
#===========================================
facilities <- read_csv("Parks_Properties_20251005.csv")
names(facilities) <- tolower(names(facilities))

# Convert WKT to geometry
facilities_sf <- st_as_sf(
  facilities,
  wkt = "multipolygon",
  crs = 4326
)

# Fix invalid geometries
facilities_sf <- st_make_valid(facilities_sf)

#===========================================
# 3. Spatial join → assign each facility to BoroCD
#===========================================
facilities_joined <- st_join(
  facilities_sf,
  neighborhoods,
  left = TRUE,
  join = st_intersects
)

#===========================================
# 4. Facilities per BoroCD
#===========================================
facility_counts <- facilities_joined %>%
  st_drop_geometry() %>%
  group_by(BoroCD) %>%
  summarise(Facilities = n())

#===========================================
# 5. Load Population CSV (Now includes BoroCD)
#===========================================
population <- read_csv("Total Population.csv")
names(population) <- str_replace_all(names(population), " ", "")

# Ensure BoroCD column exists and is numeric
population <- population %>%
  mutate(BoroCD = as.integer(BoroCD))

#===========================================
# 6. Join population with facility counts
#===========================================
neighborhood_metrics <- population %>%
  left_join(facility_counts, by = "BoroCD") %>%
  mutate(
    Facilities = ifelse(is.na(Facilities), 0, Facilities),
    Facilities_per_1000 = round((Facilities / Population) * 1000, 3)
  )

#===========================================
# 7. Produce ranking results
#===========================================

# Best-served neighborhoods
best_served <- neighborhood_metrics %>%
  arrange(desc(Facilities_per_1000)) %>%
  head(6)

# Underserved neighborhoods
worst_served <- neighborhood_metrics %>%
  arrange(Facilities_per_1000) %>%
  head(6)

print("=== Top 6 Best-Served Neighborhoods (Facilities per 1000 Residents) ===")
print(best_served)

print("=== Top 6 Worst-Served Neighborhoods (Underserved) ===")
print(worst_served)

#===========================================
# 8. Join results back to map
#===========================================
neighborhood_map <- neighborhoods %>%
  left_join(neighborhood_metrics, by = "BoroCD")


#===========================================
# 9. Visualizations
#===========================================


library(sf)
library(dplyr)
library(ggplot2)

#========================================================
# 1. Recalculate Service Tiers (Top 6 / Bottom 6)
#========================================================

neighborhood_metrics <- neighborhood_metrics %>%
  arrange(desc(Facilities_per_1000)) %>% 
  mutate(
    Rank = row_number(),
    Service_Tier = case_when(
      Rank <= 6 ~ "Most Served (Top 6)",
      Rank > (n() - 6) ~ "Least Served (Bottom 6)",
      TRUE ~ "Middle"
    )
  )

#========================================================
# 2. Join metrics to geometries
#========================================================

neighborhood_map <- neighborhoods %>%
  left_join(neighborhood_metrics, by = "BoroCD")

#========================================================
# 3. Compute centroids for only labeled neighborhoods
#========================================================

label_points <- neighborhood_map %>%
  filter(Service_Tier != "Middle") %>%        # only top 6 & bottom 6
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )

#========================================================
# 4. Plot the map
#========================================================

gg_map <- ggplot() +
  geom_sf(
    data = neighborhood_map,
    aes(fill = Service_Tier),
    color = "white",
    size = 0.8
  ) +
  scale_fill_manual(
    values = c(
      "Least Served (Bottom 6)" = "#ff4c4c",
      "Middle" = "grey80",
      "Most Served (Top 6)" = "#2ecc71"
    )
  ) +
  geom_text(
    data = label_points,
    aes(x = lon, y = lat, label = Neighborhood),
    size = 2.6,
    fontface = "bold",
    color = "black"
  ) +
  labs(
    title = "NYC Neighborhood Facility Access",
    subtitle = "Top 5 Most Served (Green) and Bottom 5 Least Served (Red)",
    fill = "Service Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  )

print(gg_map)



# Scatter plot:


library(ggplot2)
library(dplyr)
library(ggrepel)

# Filter only the neighborhoods we want to label
label_points <- neighborhood_metrics %>%
  filter(Service_Tier != "Middle")   # keep only top 6 + bottom 6

scatter_plot <- ggplot(
  neighborhood_metrics,
  aes(
    x = Population,
    y = Facilities_per_1000,
    color = Service_Tier
  )
) +
  geom_point(size = 3, alpha = 0.85) +
  
  # Label only selected neighborhoods
  geom_text_repel(
    data = label_points,
    aes(label = Neighborhood),
    size = 3.4,
    fontface = "bold",
    max.overlaps = Inf,
    box.padding = 0.5,
    point.padding = 0.3
  ) +
  
  scale_color_manual(
    values = c(
      "Most Served (Top 6)" = "#2ecc71",
      "Middle" = "grey60",
      "Least Served (Bottom 6)" = "#ff4c4c"
    )
  ) +
  labs(
    title = "Park Access vs Total Population",
    subtitle = "Labels shown for Top 6 Most Served and Bottom 6 Least Served Neighborhoods",
    x = "Total Population",
    y = "Facilities per 1000 Residents",
    color = "Service Tier"
  ) +
  theme_minimal(base_size = 13)

print(scatter_plot)



