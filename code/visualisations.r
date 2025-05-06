

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
treated_point <- df_sf %>% filter(geometry_name == "treated") %>% st_geometry() %>% .[[1]]

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

# ready to commit