
# Libraries
library(sf)
library(dplyr)
library(tidyr)

data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_V4.csv")
scmo_result <- read.csv("results/area_placebo_scmo_datav4_v01.csv")
sc_result <- read.csv("results/area_placebo_original_synth_datav4_v01.csv")

View(head(scmo_result))
View(head(sc_result))

# Merge the two results
merged_results <- sc_result %>%
  left_join(scmo_result, by = "geometry_name")
View(head(merged_results))

final_placebo_results <- merged_results

final_placebo_results <- final_placebo_results %>%
  mutate(SCMO_wins_SC = abs(ATT_SCMO) < abs(ATT_SC))

placebo_coordinates <- data %>% 
    select(geometry_name, centroid_lat, centroid_lon) %>% 
    distinct()

final_placebo_results <- final_placebo_results %>%
  left_join(placebo_coordinates, by = "geometry_name")

View(head(final_placebo_results))

points_sf <- st_as_sf(final_placebo_results,
                     coords = c("centroid_lon", "centroid_lat"),
                     crs = 4326)


st_write(points_sf, "results/placebo_att_SC_SCMO_datav4_v01.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)
sum(final_placebo_results$SCMO_wins_SC)  #60
View(points_sf)