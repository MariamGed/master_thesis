

data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")
weights <- read.csv("data/multi_outcomes_donor_weightsV1.csv")
# add one row, whith weights[treated, treated] = 
weights <- rbind(weights, c(0, "treated"))
weights$weights <- as.numeric(weights$weights)

# merge the weights with the data
data <- merge(data, weights, by = "geometry_name")
# keep only: geometry_name, centroid_lat, centroid_lon, weights
data <- data[, c("geometry_name", "centroid_lat", "centroid_lon", "weights")]
# keep only unique geometry_name
data <- data[!duplicated(data$geometry_name), ]
View(data)

# histogram of weights
hist(data$weights, breaks = 100, main = "Histogram of Weights", xlab = "Weights", ylab = "Frequency")

# Drop bottom 70% of weights from data
threshold <- quantile(data$weights, 0.80, na.rm = TRUE)  # 70th percentile
df_filtered <- data[data$weights > threshold, ]
hist(df_filtered$weights, breaks = 100, main = "Histogram of Weights", xlab = "Weights", ylab = "Frequency")
# if df_filtered < 0.00001, multiply by 10^7
df_filtered$weights <- ifelse(df_filtered$weights < 0.001, df_filtered$weights * 10, df_filtered$weights)

# Drop those with weights < 0.0001
data_sub$weights <- as.numeric(data_sub$weights)
data_sub <- data[data$weights > 0.000001, ]

hist(data_sub$weights, breaks = 100, main = "Histogram of Weights", xlab = "Weights", ylab = "Frequency")

# ---- Visualisations ----
library(sf)
library(ggplot2)
library(viridis)
library(dplyr)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(geobr) # For brazilian borders


df <- data_sub
df <- df_filtered
df$weights <- as.numeric(df$weights)
# Convert to sf object
df_sf <- st_as_sf(df, coords = c("centroid_lon", "centroid_lat"), crs = 4326)

# Extract treated point
treated_point <- df_sf %>% 
  filter(geometry_name == "treated") %>% 
  st_geometry() #%>% .[[1]]

# Create lines from treated to each other point
lines_list <- df_sf %>%
  filter(geometry_name != "treated") %>%
  rowwise() %>%
  mutate(
    line = st_sfc(st_linestring(rbind(st_coordinates(treated_point), st_coordinates(geometry))), crs = 4326)
  )

# Create an sf object of lines with associated weights
lines_sf <- st_sf(weight = lines_list$weights, geometry = st_sfc(lines_list$line))

# Get a basemap using rnaturalearth (world map)
world <- ne_countries(scale = "medium", returnclass = "sf")
#  Load Pará state polygon
para <- read_state(code_state = "PA", year = 2020)
belem <- read_municipality(code_muni = "Belém", year = 2020)
# Plotting
ggplot() +
  #geom_sf(data = para, fill = "gray95", color = "gray80") +
  geom_sf(data = lines_sf, aes(color = weight), size = 2) +
  geom_sf(data = df_sf %>% filter(geometry_name != "treated"), color = "black", size = 2) +
  geom_sf(data = df_sf %>% filter(geometry_name == "treated"), color = "black", size = 2) +
  scale_color_gradientn(colors = c("orange", "black"), name = "Weights")
  #scale_color_viridis(name = "Weights", option = "D") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_minimal()) +
  coord_sf(xlim = st_bbox(para)[c("xmin", "xmax")], ylim = st_bbox(para)[c("ymin", "ymax")]) +
  theme_minimal() +
  ggtitle("Connections to Treated Point in Pará, Brazil")


# ---- Adding a basemap ----

data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")
final_placebo_results <- read.csv("results/area_placebo_test_ATT_SC_vs_SCMO_v01.csv")

# Libraries

library(dplyr)
library(ggplot2)
library(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(dplyr)
library(geobr) # For brazilian borders
library(osmdata)
library(ggspatial)
library(basemaps)

# convert to long format for plotting
final_placebo_results_long <- final_placebo_results %>%
  pivot_longer(cols = c(ATT_SC, ATT_SCMO), names_to = "Model", values_to = "ATT")

# make $ATT values to numeric
final_placebo_results_long$ATT <- as.numeric(final_placebo_results_long$ATT)

# overall_winner: SCMO 46% of the time, rest, SC wins.
# Add a column indicating when SCMO wins
final_placebo_results <- final_placebo_results %>%
  mutate(SCMO_wins = abs(ATT_SCMO) < abs(ATT_SC))

placebo_coordinates <- data %>% 
    select(geometry_name, centroid_lat, centroid_lon) %>% 
    distinct()

final_placebo_results <- final_placebo_results %>%
  left_join(placebo_coordinates, by = "geometry_name")

# Visualisation of placebo results on a map
# Points
points_sf <- st_as_sf(final_placebo_results,
                     coords = c("centroid_lon", "centroid_lat"),
                     crs = 4326)

# Get a basemap using rnaturalearth (world map)
world <- ne_countries(scale = "medium", returnclass = "sf")
#  Load Pará state polygon
para_border <- read_state(code_state = "PA", year = 2020)
# Transform to Web Mercator (required by basemaps)
para_3857 <- st_transform(para_border, crs = 3857)

# Ensure para is in WGS84 (required for OSM queries)
para_wgs <- st_transform(para, 4326)

st_crs(points_sf)
st_crs(para_wgs)

# get a bounding box (not full for testing)
bb_small <- c(
  xmin = -60,
  ymin = -10,
  xmax = -45,
  ymax = 5
)

# data(ext)
para <- draw_ext()
# install.packages("mapedit")
library(mapedit)
# Get osm basemap
map_osm <- basemap(para, map_service = "osm", map_type = "streets")
# esri
map_esri <- basemap(para, map_service = "esri", map_type = "natgeo_world_map")


# points in sf
final_placebo_results <- st_as_sf(final_placebo_results,
                                 coords = c("centroid_lon", "centroid_lat"),
                                 crs = 4326)

final_placebo_results$longitude <- st_coordinates(final_placebo_results)[, 1]
final_placebo_results$latitude <- st_coordinates(final_placebo_results)[, 2]

library(ggplot2)
library(ggspatial)
# Make sure your sf object is transformed to match the basemap CRS
final_placebo_results <- st_transform(final_placebo_results, crs = 3857)

# Plot basemap + points
basemap_ggplot(para, map_service = "osm", map_type = "streets") +
  geom_sf(data = final_placebo_results, 
          color = "red", 
          size = 2, 
          alpha = 0.7) +
  theme_minimal()

# Plot basemap + points
basemap_ggplot(para_3857, map_service = "esri", map_type = "natgeo_world_map") +
  geom_sf(data = final_placebo_results, 
          color = "red", 
          size = 2, 
          alpha = 0.7) +
  theme_minimal()


# adding protected areas
# install.packages("wdpar")
library(wdpar)
library(sf)
library(ggplot2)
library(basemaps)

# Download WDPA data for Brazil
wdpa_brazil <- wdpa_fetch("BRA", wait = TRUE, datatype = "shp")

# Filter to Pará using spatial intersection with your border
wdpa_para <- st_filter(wdpa_brazil, st_transform(para_border, st_crs(wdpa_brazil)))

# Clean the data (removes invalid geometries, fixes issues)
wdpa_para <- wdpa_clean(wdpa_para)

# Transform all layers to 3857
para_3857       <- st_transform(para_border, 3857)
wdpa_3857       <- st_transform(wdpa_para,   3857)
points_3857     <- st_transform(final_placebo_results, 3857)

points_3857$SCMO_wins <- factor(points_3857$SCMO_wins, levels = c(TRUE, FALSE))


basemap_ggplot(para_3857, map_service = "osm", map_type = "streets") +
  geom_sf(data = wdpa_3857,   fill = "blue", color = NA,    alpha = 0.4) +
  geom_sf(data = para_3857,   fill = NA,      color = "black", linewidth = 0.8) +
  geom_sf(data = points_3857, aes(color = SCMO_wins),  size = 2,      alpha = 0.7) +
scale_color_manual(values = c("TRUE" = "green", "FALSE" = "blue"),
                   labels = c("TRUE" = "Significant", "FALSE" = "Not significant"),
                   name = "Result") +
  theme_minimal()

# save the plot
ggsave("figures/area_placebo_results_SCMO_SC_datav2.png", width = 10, height = 8, dpi = 300)
