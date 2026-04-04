
# Load libraries

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
library(readr)
library(wdpar) # for protected areas data
library(ggnewscale)


# Load data
data <- read_csv("results/area_placebo_test_ATT_SC_vs_SCMO_with_coords_v01.csv")

data_st <- st_as_sf(data,
                    coords = c("centroid_lon", "centroid_lat"),
                    crs = 4326)


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

# Definition of treated unit's boundary polygon
treated_3857 <- st_transform(poly, 3857)

# #  Load Pará state polygon
para <- read_state(code_state = "PA", year = 2020)
# # Transform to Web Mercator (required by basemaps)
para_3857 <- st_transform(para, crs = 3857)

# download protected areas data for Brazil using wdpar
wdpa_brazil <- wdpa_fetch("BRA", wait = TRUE, datatype = "shp")

# Filter to Pará using spatial intersection with your border
wdpa_para <- st_filter(wdpa_brazil, st_transform(para_3857, st_crs(wdpa_brazil)))

# Clean the data (removes invalid geometries, fixes issues)
wdpa_para <- wdpa_clean(wdpa_para)

# Transform all layers to 3857
para_3857       <- st_transform(para, 3857)
wdpa_3857       <- st_transform(wdpa_para,   3857)
data_3857     <- st_transform(data_st, 3857)

# Buffer by x km (e.g. 50 km = 50000 metres)
points_buffered <- st_buffer(data_3857, dist = 8000)

wdpa_3857   <- st_make_valid(wdpa_3857)
para_3857   <- st_make_valid(para_3857)

wdpa_clipped <- st_intersection(wdpa_3857, para_3857)
para_bbox <- st_bbox(para_3857)
# Create a large bounding box that covers the whole map extent
big_bbox <- st_as_sfc(para_bbox + c(-1e5, -1e5, 1e5, 1e5))  # expand by 1000 km in all directions

# Subtract Para border from it — this gives you everything OUTSIDE Para
outside_para <- st_difference(big_bbox, para_3857)

# Make the map with basemap
basemap_ggplot(para_3857) + # natgeo_world_map
  geom_sf(data = wdpa_clipped,    fill = "grey", color = NA) +  # protected areas
  geom_sf(data = points_buffered, fill = "red", color = NA, alpha = 1) +  # buffered points
  geom_sf(data = treated_3857,   fill = "blue", color = NA, alpha = 1) +  # treated unit
  geom_sf(data = outside_para,    fill = "white", color = NA) +  # <-- mask
  geom_sf(data = para_3857,       fill = NA, color = "black", linewidth = 1.4) +
  annotation_scale(location = "bl", pad_x = unit(1.5, "cm") ,pad_y = unit(1.5, "cm") ) +  # scale bar (ggspatial) , pad_x = unit(1.5, "cm") 
  theme_void()


# Without basemap
ggplot() +
    geom_sf(data = wdpa_clipped,    fill = "grey", color = NA) +  # protected areas
    geom_sf(data = points_buffered, fill = "red", color = NA, alpha = 1) +  # buffered points
    geom_sf(data = treated_3857,   fill = "blue", color = NA, alpha = 1) +  # treated unit
    geom_sf(data = outside_para,    fill = "white", color = NA) +  # <-- mask
    geom_sf(data = para_3857,       fill = NA, color = "black", linewidth = 1.4) +
    # annotation_scale(location = "bl", pad_x = unit(1.5, "cm") ,pad_y = unit(1.5, "cm") ) +  # scale bar (ggspatial) , pad_x = unit(1.5, "cm") 
    theme_void()

# save the plot
ggsave("figures/sc_animations/para_border_donors_mapv01.png", width = 10, height = 8, dpi = 300)

# read data with weights
weights <- read_csv("results/scmo_weights_datav04_v02.csv")
View(weights)

hist(weights$weight[weights$weight > 0.001])

# significant weights
weights[weights$weight > 0.001, ]

# add coordinates to the weights data
weights_with_coords <- weights %>%
    left_join(data %>% select(geometry_name, centroid_lon, centroid_lat), by = "geometry_name") %>%
    unique()

unique(weights_with_coords[weights_with_coords$weight > 0.001, ])

weights_significant <- weights_with_coords[weights_with_coords$weight > 0.001, ]

# plot those points on the map
weights_with_coords_sf <- st_as_sf(weights_significant,
                                    coords = c("centroid_lon", "centroid_lat"),
                                    crs = 4326)

weights_3857 <- st_transform(weights_with_coords_sf, 3857)

weights_3857_buffered <- st_buffer(weights_3857, dist = 10000)
# map
# Without basemap
ggplot() +
    geom_sf(data = wdpa_clipped,    fill = "grey", color = NA) +  # protected areas
    # geom_sf(data = points_buffered, fill = "red", color = NA, alpha = 1) +  # buffered points
    geom_sf(data = treated_3857,   fill = "blue", color = NA, alpha = 1) +  # treated unit
    geom_sf(data = outside_para,    fill = "white", color = NA) +  # <-- mask
    geom_sf(data = para_3857,       fill = NA, color = "black", linewidth = 1.4) +
    geom_sf(data = weights_3857_buffered,  fill = 'red', color = "red",  alpha = 1) +  # <-- weights
    # annotation_scale(location = "bl", pad_x = unit(1.5, "cm") ,pad_y = unit(1.5, "cm") ) +  # scale bar (ggspatial) , pad_x = unit(1.5, "cm") 
    theme_void()

# save the plot
ggsave("figures/sc_animations/significant_weights_v01.png", width = 10, height = 8, dpi = 300)



# Add lines and weights between treated point and donors

# treated point:
treated_centroid <- st_centroid(treated_3857)

# Create lines from treated to each other point
lines_list <- weights_3857 %>%
  rowwise() %>%
  mutate(
    line = st_sfc(st_linestring(rbind(st_coordinates(treated_centroid), st_coordinates(geometry))), crs = 3857)
  )

# Create an sf object of lines with associated weights
lines_sf <- st_sf(weight = lines_list$weight, geometry = st_sfc(lines_list$line))

lines_sf <- st_transform(lines_sf, 3857)

# Connecting with straight lines
ggplot() +
    geom_sf(data = wdpa_clipped,    fill = "grey", color = NA) +  # protected areas
    # geom_sf(data = points_buffered, fill = "red", color = NA, alpha = 1) +  # buffered points
    geom_sf(data = treated_3857,   fill = "blue", color = NA, alpha = 1) +  # treated unit
    geom_sf(data = outside_para,    fill = "white", color = NA) +  # <-- mask
    geom_sf(data = para_3857,       fill = NA, color = "black", linewidth = 1.4) +
    geom_sf(data = weights_3857_buffered,  fill = 'red', color = "red",  alpha = 1) +  # <-- weights
    geom_sf(data = lines_sf, aes(color = weight), size = 5) +  # <-- lines with weights
    scale_color_gradient(low = "green", high = "red") +
    # annotation_scale(location = "bl", pad_x = unit(1.5, "cm") ,pad_y = unit(1.5, "cm") ) +  # scale bar (ggspatial) , pad_x = unit(1.5, "cm") 
    theme_void()

ggplot() + geom_sf(data = lines_sf, color = 'black', size = 5) 

ggsave("figures/sc_animations/connected_weights_v01.png", width = 10, height = 8, dpi = 300)

library(dplyr)

# Extract start and end coordinates
lines_df <- weights_3857 %>%
  mutate(
    x_end = st_coordinates(geometry)[, 1],
    y_end = st_coordinates(geometry)[, 2],
    x_start = st_coordinates(treated_centroid)[1],
    y_start = st_coordinates(treated_centroid)[2]
  ) %>%
  st_drop_geometry()  # geom_curve doesn't need sf geometry

# Connecting with curved lines
ggplot() +
    geom_sf(data = wdpa_clipped,    fill = "grey", color = NA) +  # protected areas
    # geom_sf(data = points_buffered, fill = "red", color = NA, alpha = 1) +  # buffered points
    geom_sf(data = treated_3857,   fill = "blue", color = NA, alpha = 1) +  # treated unit
    geom_sf(data = outside_para,    fill = "white", color = NA) +  # <-- mask
    geom_sf(data = para_3857,       fill = NA, color = "black", linewidth = 1.4) +
    geom_sf(data = weights_3857_buffered,  fill = 'black', color = "black",  alpha = 1) +  # <-- weights
    # geom_sf(data = lines_sf, aes(color = weight), size = 5) +  # <-- lines with weights
    # scale_color_gradient(low = "green", high = "red") +
    # annotation_scale(location = "bl", pad_x = unit(1.5, "cm") ,pad_y = unit(1.5, "cm") ) +  # scale bar (ggspatial) , pad_x = unit(1.5, "cm") 
    theme_void() +
    geom_curve(data = lines_df,
           aes(x = x_start, y = y_start,
               xend = x_end, yend = y_end,
                color = weight),
           curvature = 0.5,          # positive = curves right, negative = curves left
           alpha = 1,
           arrow = arrow(length = unit(0.1, "cm"))) +  # optional arrow tip
    scale_color_gradient(low = "blue", high = "red")
    # theme(legend.position = "br")  # hide legend if not needed


ggsave("figures/sc_animations/connected_weights_curved_v01.png", width = 10, height = 8, dpi = 300)
