

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

# LOAD DATA
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_V4.csv")
final_placebo_results <- read.csv("results/area_placebo_test_ATT_SC_vs_SCMO_v01.csv")

sc_res <- read.csv("results/area_placebo_original_synth_datav4_v01.csv")
scmo_res <- read.csv("results/area_placebo_scmo_datav4_v01.csv")

final_placebo_results <- scmo_res %>%
  left_join(sc_res, by = "geometry_name")

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

# write.csv(final_placebo_results, "results/area_placebo_test_ATT_SC_vs_SCMO_with_coords_v01.csv", row.names = FALSE)
# Visualisation of placebo results on a map

# # data(ext)

final_placebo_results_sf <- st_as_sf(final_placebo_results,
                                 coords = c("centroid_lon", "centroid_lat"),
                                 crs = 4326)

final_placebo_results_transformed <- st_transform(final_placebo_results_sf, crs = 3857)

# final_placebo_results$longitude <- st_coordinates(final_placebo_results)[, 1]
# final_placebo_results$latitude <- st_coordinates(final_placebo_results)[, 2]

library(ggplot2)
library(ggspatial)
# Make sure your sf object is transformed to match the basemap CRS


# final_placebo_results <- final_placebo_results %>% select(-geometry)

# #  Load Pará state polygon
para <- read_state(code_state = "PA", year = 2020)
# # Transform to Web Mercator (required by basemaps)
para_3857 <- st_transform(para, crs = 3857)

# Plot basemap + points
basemap_ggplot(para_3857, map_service = "osm", map_type = "streets") +
  geom_sf(data = final_placebo_results_transformed, 
          color = "red", 
          size = 2, 
          alpha = 0.7) +
  theme_minimal()

# Plot basemap + points
basemap_ggplot(para_3857, map_service = "esri", map_type = "natgeo_world_map") +
  geom_sf(data = final_placebo_results_transformed, 
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
library(ggnewscale)

# Download WDPA data for Brazil
wdpa_brazil <- wdpa_fetch("BRA", wait = TRUE, datatype = "shp")

# Filter to Pará using spatial intersection with your border
wdpa_para <- st_filter(wdpa_brazil, st_transform(para, st_crs(wdpa_brazil)))

# Clean the data (removes invalid geometries, fixes issues)
wdpa_para <- wdpa_clean(wdpa_para)

# Transform all layers to 3857
para_3857       <- st_transform(para, 3857)
wdpa_3857       <- st_transform(wdpa_para,   3857)
points_3857     <- st_transform(final_placebo_results_transformed, 3857)

points_3857$SCMO_wins <- factor(points_3857$SCMO_wins, levels = c(TRUE, FALSE))

# Buffer by x km (e.g. 50 km = 50000 metres)
points_buffered <- st_buffer(points_3857, dist = 8000)

wdpa_3857   <- st_make_valid(wdpa_3857)
para_3857   <- st_make_valid(para_3857)

wdpa_clipped <- st_intersection(wdpa_3857, para_3857)

basemap_ggplot(para_3857, map_service = "esri", map_type = "natgeo_world_map", alpha = 0.8) +
  geom_sf(data = wdpa_clipped, fill = "gray1", color = NA, alpha = 0.3) +
  geom_sf(data = para_3857, fill = NA, color = "black", linewidth = 0.8) +

  ggnewscale::new_scale_fill() +

  geom_sf(data = points_buffered, aes(fill = SCMO_wins), alpha = 1) +
  scale_fill_manual(
    values = c("TRUE" = "dark green", "FALSE" = "red"),
    labels = c("TRUE" = "SCMO wins", "FALSE" = "SC wins"),
    name = "Result"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())


library(ggplot2)
library(sf)
library(basemaps)
library(cowplot)  # for inset map
library(rnaturalearth)  # for South America outline

# -----  Whole para map ----
# --- Main map ---
para_bbox <- st_bbox(para_3857)
# Create a large bounding box that covers the whole map extent
big_bbox <- st_as_sfc(para_bbox + c(-1e5, -1e5, 1e5, 1e5))  # expand by 1000 km in all directions

# Subtract Para border from it — this gives you everything OUTSIDE Para
outside_para <- st_difference(big_bbox, para_3857)

main_map <- basemap_ggplot(para_3857, map_service = "esri", map_type = "natgeo_world_map") + # natgeo_world_map
  geom_sf(data = para_3857,       fill = NA, color = "black", linewidth = 1.4) +
  geom_sf(data = outside_para,    fill = "white", color = NA) +  # <-- mask
  annotation_scale(location = "bl", pad_x = unit(1.5, "cm") ,pad_y = unit(1.5, "cm") ) +  # scale bar (ggspatial) , pad_x = unit(1.5, "cm") 
  theme_void()#+
  # theme(legend.position = "bottom", legend.key.spacing.x = unit(1, "cm"))

# --- Inset minimap ---
south_america <- ne_countries(continent = "South America", returnclass = "sf")
brazil        <- ne_countries(country = "Brazil", returnclass = "sf")
para_4326     <- st_transform(para_3857, 4326)  # back to 4326 for inset

inset_map <- ggplot() +
  geom_sf(data = south_america, fill = "grey80", color = "white", linewidth = 0.3) +
  geom_sf(data = brazil,        fill = "grey50", color = "white", linewidth = 0.3) +
  geom_sf(data = para_4326,     fill = "black",  color = "black") +
  theme_void()

# --- Combine with cowplot ---
ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset_map, 
            x = 0.71, y = 0.08,   # position: bottom-right
            width = 0.18, height = 0.25)

# save the plot
ggsave("figures/basic_para_mapv01.png", width = 10, height = 8, dpi = 300)



# Plotting for presentation
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_V4.csv")
View(head(data))

library(ggplot2)
data_treat <- data %>% filter(geometry_name == "treated")

ggplot(data_treat, aes(x = year, y = deforestation*100)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 3) +
  # vertical line at 2012
  geom_vline(xintercept = 2012, linetype = "dashed", color = "blue", size = 1) +
  # add limits on y axis
  ylim(0, 10) +
  scale_color_manual(labels = c("Treated"), name = "") + # removes legend title 
  theme_minimal(base_size = 22) +
  labs(title = "Deforestation in conserved project is close to zero",
       x = "Year",
       y = "Deforestation (% of total area)") +
  theme(legend.position = "top")  +
  theme(panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank())

# save the plot
ggsave("figures/deforestaion_treated_lineplot_scale10_V02.png", width = 10, height = 13, dpi = 300)



treatment_year <- 2012

# Add a column treatment, called "treatment"
data$treatment <- 0

# If geometry_name=="treated", then treatment=1, else treatment=0
data$treatment[data$geometry_name == "treated" & data$year >= treatment_year] <- 1
data$treatment <- data$treatment %>% as.numeric()
data$geometry_name <- data$geometry_name %>% as.factor()
# Sort the data by geometry_name and year
data <- data[order(data$geometry_name, data$year), ]

# Drop column .geo and .index
data$.geo <- NULL
data$system.index <- NULL

# draw 10 random geometry_names
set.seed(124)
random_geometries <- sample(unique(data$geometry_name), 10)
data_random <- data %>% filter(geometry_name %in% random_geometries)

donors <- data %>% 
    filter(geometry_name != "treated") %>% 
    select(geometry_name) %>% 
    distinct() %>% 
    pull()

years <- data %>% 
    select(year) %>% 
    distinct() %>% 
    pull()

find_lows <- function(data) {
  candidates <- c() 
  for (i in donors) {
    temp <- data %>% filter(geometry_name == i) %>% arrange(year)
    deforestation <- temp$deforestation
    mean_def <- mean(deforestation)
    if (mean_def < 0.002) {
      candidates <- rbind(candidates, c(i, mean_def))
    }
  }
  return(candidates)
}

results <- find_lows(data)
results

length(data$geometry_name %>% unique())
# Subset geometry_names in results[,1]
data_subset <- data %>% filter((geometry_name %in% results[,1]) | geometry_name == "treated") 
length(data_subset$geometry_name %>% unique()) 

# panelview(deforestation ~ treatment, data = data_subset,  index = c("geometry_name","year"), type = "outcome",
#           cex.axis = 16, cex.lab = 20, cex.main = 20, main = " Deforestation proportion in treated and donor units")

# make lineplots of each raw in data_subset
# color treated in red, rest in black
ggplot(data_subset, aes(x = year, y = deforestation*100, group = geometry_name, color = geometry_name == "treated")) + #, size = 1
  # geom_line() +
  geom_line(data = subset(data_subset, geometry_name != "treated"), 
          color = "grey", alpha = 0.5) +
geom_line(data = subset(data_subset, geometry_name == "treated"), 
          color = "red", size = 1.2) +
  geom_point() +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "blue", size = 1) +
  # add limits on y axis
  ylim(0, 100) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "grey"),
                    labels = c("TRUE" = "Treated", "FALSE" = "Donor"),
                    name = "") +  # removes legend title 
  theme_minimal(base_size = 24) +
  labs(title = "Many donor units with low deforestation",
       x = "Year",
       y = "Deforestation (% of total area)") +
    theme(legend.position = "top")  +
  theme(panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_blank()) 
# + theme(legend.key.width = unit(5, "cm"))

ggsave("figures/deforestaion_donors_lineplot_scale100_V01.png", width = 10, height = 13, dpi = 300)



 
# ----- Treated point coordinates:  ----

coords <- matrix(c(
  -49.400300753503466, -3.3826227517416867,
  -49.399614107806286, -3.3564039613484815,
  -49.397469946830725, -3.351892357196979,
  -49.3893160300592,   -3.343580986585731,
  -49.385367817723846, -3.33664050972247,
  -49.383565372945775, -3.32515862560933,
  -49.379531329908545, -3.3155617243728597,
  -49.37187141991771,  -3.302986592830798,
  -49.363275715225,    -3.2934020904386574,
  -49.35323352303559,  -3.2791776236668153,
  -49.34961604761821,  -3.273106168545384,
  -49.34335040634778,  -3.2581101859274546,
  -49.34162318167869,  -3.252968654688277,
  -49.339992398337294, -3.2414001135883,
  -49.33511151340897,  -3.2335005911485717,
  -49.331206205543886, -3.2306726736482716,
  -49.32807338007226,  -3.226216565816031,
  -49.325455559425755, -3.2226173877301663,
  -49.32714678668693,  -3.2197514182170384,
  -49.32660641110333,  -3.2144859809442825,
  -49.325662273383436, -3.210886756974755,
  -49.32549061198482,  -3.204031057196402,
  -49.32417579440322,  -3.1974870317265114,
  -49.319412190447714, -3.192602286436185,
  -49.31471827713939,  -3.188440569792171,
  -49.31208433459771,  -3.183079070080389,
  -49.30784650339187,  -3.1713276648226554,
  -49.307587215683554, -3.1682204517080153,
  -49.308057488307824, -3.165113238583877,
  -49.30599397058223,  -3.1590702110545434,
  -49.30511104523514,  -3.155152564441504,
  -49.30517225938133,  -3.1527346727473105,
  -49.30589550545946,  -3.1494843515845807,
  -49.305294690563116, -3.140485701186776,
  -49.30357807654607,  -3.130887055300114,
  -49.30110354728277,  -3.109023485829136,
  -49.30179019290913,  -3.102220823143291,
  -49.394658895325605, -3.1259773275983456,
  -49.48348829641461,  -3.371914914894332
), ncol = 2, byrow = TRUE)
coords <- rbind(coords, coords[1, ])
poly <- st_polygon(list(coords)) |> 
  st_sfc(crs = 4326) |>   # WGS84 (same as GEE)
  st_sf()

treated_3857 <- st_transform(poly, 3857)


main_map <- basemap_ggplot(para_3857, map_service = "esri", map_type = "natgeo_world_map") + # natgeo_world_map
  geom_sf(data = para_3857,       fill = NA, color = "black", linewidth = 1.4) +
  geom_sf(data = treated_3857,    fill = "red", color = "black") + #, linewidth = 1.4
  geom_sf(data = outside_para,    fill = "white", color = NA) +  # <-- mask
  annotation_scale(location = "bl", pad_x = unit(1.5, "cm") ,pad_y = unit(1.5, "cm") ) +  # scale bar (ggspatial) , pad_x = unit(1.5, "cm") 
  theme_void()

ggdraw() +
  draw_plot(main_map)

# save the plot
ggsave("figures/basic_para_with_treated_mapv01.png", width = 10, height = 8, dpi = 300)

