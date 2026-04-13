
# Libraries
library(sf)
library(dplyr)
library(tidyr)

data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_V4.csv")
scmo_result <- read.csv("results/area_placebo_scmo_concat_datav4_v02.csv")
sc_result <- read.csv("results/area_placebo_original_synth_datav4_v02.csv")

View(head(scmo_result))
View(head(sc_result))

sc_result <- sc_result %>%
  rename(ATT_SC = ATT, Pre.RMSE_SC = Pre.RMSE, Post.RMSE_SC = Post.RMSE, Ratio.Post.Pre_SC = Ratio.Post.Pre)

scmo_result <- scmo_result %>%
  rename(ATT_SCMO = ATT, Pre.RMSE_SCMO = Pre.RMSE, Post.RMSE_SCMO = Post.RMSE, Ratio.Post.Pre_SCMO = Ratio.Post.Pre)
  
# 36th donor point is not in sc, so I remove from scmo for comparison
scmo_result <- scmo_result %>%
  filter(treated_point != "donor36.0")
  
# Merge the two results
merged_results <- sc_result %>%
  left_join(scmo_result, by = "treated_point")
# View(head(merged_results))


final_placebo_results <- merged_results

# drop the treated point
final_placebo_results <- final_placebo_results %>%
  filter(treated_point != "treated")

final_placebo_results <- final_placebo_results %>%
  mutate(SCMO_wins_SC = abs(ATT_SCMO) < abs(ATT_SC))

final_placebo_results <- final_placebo_results %>%
  rename(geometry_name = treated_point) 


sum(final_placebo_results$SCMO_wins_SC) #56 out of 116

placebo_coordinates <- data %>% 
    select(geometry_name, centroid_lat, centroid_lon) %>% 
    distinct()

final_placebo_results <- final_placebo_results %>%
  left_join(placebo_coordinates, by = "geometry_name")

View(head(final_placebo_results))

write.csv(final_placebo_results, "results/placebo_att_SC_SCMO_datav4_v02.csv", row.names = FALSE)

points_sf <- st_as_sf(final_placebo_results,
                     coords = c("centroid_lon", "centroid_lat"),
                     crs = 4326)


# st_write(points_sf, "results/placebo_att_SC_SCMO_datav4_v01.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)
sum(final_placebo_results$SCMO_wins_SC)  #60
View(points_sf)