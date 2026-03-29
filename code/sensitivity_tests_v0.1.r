'
Sensitivity tests

Test one: Treatment area placebo 

Choose a donor area instance as a treated area and run the model
Expected result: ATT should be close to zero
Compare Synth with multiple outcomes vs Synth with single outcome.

'''

# Libraries
library(readr)
library(magrittr)
library(dplyr)
library(panelView)
library(augsynth) # multiple outcome synth 
library(fect) # single outcome gsynth


#setwd("/Users/mariamgedenidze/Desktop/YSE Thesis/master_thesis")
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")

datav4 <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_V4.csv")
#View(head(data,30))

# check the distribution of deforestation per year
#hist(data$deforestation[data$year==2009])

# Modelling parameters:
# Variables - SR_B1: Blue, SR_B2: Green, SR_B3: Red, SR_B4: NIR, SR_B5: SWIR1, SR_B7: SWIR2
# Outcomes - deforestation, NDVI, SR_B1, SR_B2, SR_B5

# Assumption check
# Low rank assumption
# Evaluate this empirically by examining whether a few singular vectors capture the majority of the total variation across outcomes.



# Prepping data for modelling
# Step 1: Standardise the outcome variables by pre_treatment standard deviation

data <- data %>%
    group_by(geometry_name) %>%
    mutate(deforestation_standardised = deforestation/(sd(deforestation[year<treatment_year], na.rm = FALSE))) %>%
    mutate(NDVI_standardised = NDVI/(sd(NDVI[year<treatment_year], na.rm = FALSE))) %>%
    mutate(SR_B1_standardised = SR_B1/(sd(SR_B1[year<treatment_year], na.rm = FALSE))) %>%
    mutate(SR_B2_standardised = SR_B2/(sd(SR_B2[year<treatment_year], na.rm = FALSE))) %>%
    mutate(SR_B5_standardised = SR_B5/(sd(SR_B5[year<treatment_year], na.rm = FALSE))) %>%
    ungroup() # drop the groups 


# Seeing correlations between outcome variables
treated_NDVI <- data$NDVI[data$geometry_name=="treated"]
treated_deforestation <- data$deforestation[data$geometry_name=="treated"]
treated_SR_B5 <- data$SR_B5[data$geometry_name=="treated"]
treated_SR_B2 <- data$SR_B2[data$geometry_name=="treated"]
treated_SR_B1 <- data$SR_B1[data$geometry_name=="treated"]

cor(treated_SR_B1,treated_SR_B2)

# ----- End of data prep -----

# -------- Modelling ---------
# ------ Single outcome gsynthetic control, fect package ----
panelview(deforestation ~ treatment, data = data,  index = c("geometry_name","year")) 
panelview(deforestation_standardised ~ treatment, data = data,  index = c("geometry_name","year"), type = "outcome") 
# NOTE: huge spike in 2016 in deforestation, reason? should i remove that year

single_outcome <- fect(deforestation ~ treatment + SR_B1_standardised + SR_B2_standardised + SR_B5_standardised, data = data, index = c("geometry_name","year"), 
            method = "gsynth", force = "two-way", CV = FALSE, r = c(0, 5), 
            se = TRUE,  vartype = 'parametric', 
            parallel = FALSE) # + X1 + X2 # nboots = 1000,
print(single_outcome)

a <- plot(single_outcome)
print(a)


# ------ Multiple outcomes with augsynth package ------
multi_outcomes2_std <- augsynth(deforestation_standardised + NDVI_standardised ~ treatment, geometry_name, year, data, progfunc = 'Ridge', scm=T) # | SR_B1 + SR_B2 + SR_B3 # t_int = treatment_year,
plot(multi_outcomes2_std, grid_size = 2)
summary(multi_outcomes2_std, grid_size = 2) 

multi_outcomes2 <- augsynth(deforestation + NDVI ~ treatment, geometry_name, year, data, progfunc = 'Ridge', scm=T)
plot(multi_outcomes2, grid_size = 2)
summary(multi_outcomes2, grid_size = 2) 

# This causes r to abort
# Seems like it's a non-convex problem
#single_outcome_augsynth <- augsynth(deforestation_standardised ~ treatment | SR_B1 + SR_B2 + SR_B3, geometry_name, year, data, progfunc = 'Ridge', scm=T) 
#summary(single_outcome_augsynth, grid_size = 2)


# ---- Test 1: Treatment Placebo test ----

# a) Reassign treatment status to a non-treated area and reproduce ATT = 0

data1 <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")
#View(head(data,30))

# Constants
treatment_year <- 2012
treated_test_point <- sample(unique(data1$geometry_name), size = 1)

# Add a column treatment, called 'treatment'
data1$treatment <- 0

# If geometry_name=='treated', then treatment=1, else treatment=0
data1$treatment[data1$geometry_name == treated_test_point & data1$year >= treatment_year] <- 1
data1$treatment <- data1$treatment %>% as.numeric()
data1$geometry_name <- data1$geometry_name %>% as.factor()
# Sort the data by geometry_name and year
data1 <- data1[order(data1$geometry_name, data1$year), ]

# Drop column .geo and .index
data1$.geo <- NULL
data1$system.index <- NULL

# Y2013 missing for most donor pool
data1 <- data1 %>% filter(year != 2013)

# 2 missing values left for NDVI(&rest of the vars), removing the rows with missing values
data1 <- data1 %>% filter(!is.na(NDVI))

# standardising
data1 <- data1 %>%
  group_by(geometry_name) %>%
  mutate(deforestation_standardised = deforestation/(sd(deforestation[year<treatment_year], na.rm = FALSE))) %>%
  mutate(NDVI_standardised = NDVI/(sd(NDVI[year<treatment_year], na.rm = FALSE))) %>%
  mutate(SR_B1_standardised = SR_B1/(sd(SR_B1[year<treatment_year], na.rm = FALSE))) %>%
  mutate(SR_B2_standardised = SR_B2/(sd(SR_B2[year<treatment_year], na.rm = FALSE))) %>%
  mutate(SR_B5_standardised = SR_B5/(sd(SR_B5[year<treatment_year], na.rm = FALSE))) %>%
  ungroup() # drop the groups 

final_data <- data1
# ------ Multiple outcomes with augsynth package ------
multi_outcomes2_std <- augsynth(deforestation_standardised + NDVI_standardised ~ treatment| SR_B1 + SR_B2 + SR_B5, geometry_name, year, final_data, progfunc = 'Ridge', scm=T) # | SR_B1 + SR_B2 + SR_B3 # t_int = treatment_year,
a <- summary(multi_outcomes2_std, grid_size = 2) 
View(a[["att"]])


multi_outcomes2 <- augsynth(deforestation + NDVI ~ treatment | SR_B1 + SR_B2 + SR_B5, geometry_name, year, final_data, progfunc = 'Ridge', scm=T)
summary(multi_outcomes2, grid_size = 2) 
#plot(multi_outcomes2, grid_size = 2)
#plot(multi_outcomes2[["weights"]])
b <- summary(multi_outcomes2, grid_size = 2) 
View(b[["att"]])

# single outcome
single_outcome2 <- fect(deforestation ~ treatment + SR_B1 + SR_B2 + SR_B5, data = final_data, index = c("geometry_name","year"), 
                       method = "gsynth", force = "two-way", CV = FALSE, r = c(0, 5), 
                       se = TRUE,  vartype = 'parametric', 
                       parallel = FALSE) # + X1 + X2 # nboots = 1000,
print(single_outcome2)

# ----- Test 1b -----
# Permute among the donor pool, run both models on each area placebo
# outcome:plot ATT for each placebo with CI, 1 with gsynth, 1 with augsynth.


# Libraries
library(readr)
library(magrittr)
library(dplyr)
library(panelView)
library(augsynth) # multiple outcome synth 
library(fect) # single outcome gsynth
library(ggplot2)
library(tidyr)

# load data
# data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_V4.csv")

#View(head(data,30))

att_synth_methods <- function(data, treatment_point, treatment_year, model = c("SC", "SCMO")){
    # Data prep
    data$treatment <- 0
    data$treatment[data$geometry_name == treatment_point & data$year >= treatment_year] <- 1
    data$treatment <- data$treatment %>% as.numeric()
    data$geometry_name <- data$geometry_name %>% as.factor()
    # Sort the data by geometry_name and year
    data <- data[order(data$geometry_name, data$year), ]
    
    # Drop column .geo and .index
    data$.geo <- NULL
    data$system.index <- NULL
    
    if (model == "SC") {
      outcome <- augsynth(deforestation ~ treatment| NDVI + SR_B1 + SR_B2 + SR_B5, geometry_name, year, data, progfunc = 'Ridge', scm=T)
      a <- summary(outcome)
      att <- a$average_att$Estimate
    }  
    else if(model == "SCMO"){
      # clean data
      data <- data %>% 
          filter(year != 2013) %>% 
          filter(!is.na(NDVI))
  
      outcome <- augsynth(deforestation + NDVI + SR_B1 + SR_B2 + SR_B5 ~ treatment, geometry_name, year, data, t_int=treatment_year, progfunc = 'Ridge', scm=T)
      
      b <- summary(outcome)
      bdf <- as.data.frame(b$average_att)
      att <- bdf %>%
        filter(Outcome == "deforestation") %>%
        select(Estimate)
    }
    return(list(treated_point = treatment_point, ATT =as.numeric(att))) #full_outcome = outcome, 
}

sc <- att_synth_methods(data = data, treatment_point = "treated", treatment_year = 2012, model = "SC")
scmo <- att_synth_methods(data = data, treatment_point = "treated", treatment_year = 2012, model = "SCMO")


# apply att_synth_methods to all donor areas
sc_placebo_results <- sapply(unique(data$geometry_name), function(x) att_synth_methods(data, x, 2012, model = "SC"))
sc_placebo_results <- as.data.frame(sc_placebo_results)

scmo_placebo_results <- sapply(unique(data$geometry_name), function(x) att_synth_methods(data, x, 2012, model = "SCMO"))
scmo_placebo_results <- as.data.frame(scmo_placebo_results)

# Merge the results

# Make df long
sc_placebo_results <- sc_placebo_results %>%
  filter(rownames(sc_placebo_results) == "ATT") %>% # only select raw ATT
  pivot_longer(cols = everything(), names_to = "geometry_name", values_to = "ATT") %>%
  rename(ATT_SC = ATT)

# sc_placebo_results1$ATT_SC <- as.numeric(sc_placebo_results1$ATT_SC) 
# ggplot(sc_placebo_results1, aes(x = geometry_name, y = ATT_SC)) +
#   geom_line() +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black")

# st_sc <- sd(sc_placebo_results1$ATT_SC)
# range_sc <- range(sc_placebo_results1$ATT_SC)
# a <- hist(sc_placebo_results1$ATT_SC)
# # make a ggplot histogram of the ATT_SC values
# ggplot(sc_placebo_results1, aes(x = ATT_SC)) +
#   geom_histogram(binwidth = 0.007, fill = "blue", color = "black") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
#   labs(title = "Distribution of ATT_SC values across placebo tests", x = "ATT_SC", y = "Frequency") +
#   theme_minimal()

# # save the histogram
# ggsave("figures/area_placebo_comparisons/area_placebo_test_ATT_SC_scatterplot.png", width = 10, height = 6)
# ggsave("figures/area_placebo_comparisons/area_placebo_test_ATT_SC_histogram.png", width = 10, height = 6)

scmo_placebo_results <- scmo_placebo_results %>%
  filter(rownames(scmo_placebo_results) == "ATT") %>% # only select raw ATT
  pivot_longer(cols = everything(), names_to = "geometry_name", values_to = "ATT") %>%
  rename(ATT_SCMO = ATT)

final_placebo_results <- sc_placebo_results %>%
  left_join(scmo_placebo_results, by = "geometry_name")
# Save the final placebo results
# final_placebo_results$ATT_SC <- as.numeric(final_placebo_results$ATT_SC)
# final_placebo_results$ATT_SCMO <- as.numeric(final_placebo_results$ATT_SCMO)

# write.csv(final_placebo_results, "results/area_placebo_test_ATT_SC_vs_SCMO_v01.csv", row.names = FALSE)

# read the final placebo results
final_placebo_results <- read.csv("results/area_placebo_test_ATT_SC_vs_SCMO_v01.csv")

# convert to long format for plotting
final_placebo_results_long <- final_placebo_results %>%
  pivot_longer(cols = c(ATT_SC, ATT_SCMO), names_to = "Model", values_to = "ATT")


# make $ATT values to numeric
final_placebo_results_long$ATT <- as.numeric(final_placebo_results_long$ATT)


# Plot ATTs on y-axis, geometry_name on x-axis, with two lines for SC and SCMO
# x-axis should be discrete
ggplot(final_placebo_results_long, aes(x = geometry_name, y = ATT, color = Model, group = Model)) +
  geom_line() +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

# save the plot
# ggsave("figures/area_placebo_comparisons/area_placebo_test_ATT_comparison_scatterplot_V01.png", width = 10, height = 6)

# make a ggplot histograms of the ATT values for SC and SCMO with 2 facets, and two differeent colors
ggplot(final_placebo_results_long, aes(x = ATT, fill = Model)) +
  geom_histogram(position = "identity", alpha = 0.8, binwidth = 0.007, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Distribution of ATT values across placebo tests", x = "ATT", y = "Frequency") +
  theme_minimal() +
  facet_wrap(~Model)

# save the histogram
# ggsave("figures/area_placebo_comparisons/area_placebo_test_ATT_comparison_histogram_V01.png", width = 10, height = 6)

# histogram of histograms overlapping
ggplot(final_placebo_results_long, aes(x = ATT, fill = Model)) +
  geom_histogram(position = "identity", alpha = 0.8, binwidth = 0.007, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Distribution of ATT values across placebo tests", x = "ATT", y = "Frequency") +
  theme_minimal()

# Make a density plot of the ATT values for SC and SCMO
ggplot(final_placebo_results_long, aes(x = ATT, fill = Model)) +
  geom_density(alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Density of ATT values across placebo tests", x = "ATT", y = "Density") +
  theme_minimal()

# save the density plot
# ggsave("figures/area_placebo_comparisons/area_placebo_test_ATT_comparison_density_V01.png", width = 10, height = 6)

# How many times is ATT closer to zero among SC vs SCMO (filter geometry_name != "treated")
overall_winner <- final_placebo_results %>%
  filter(geometry_name != "treated") %>%
  summarise(SCMO_wins = sum(abs(ATT_SCMO) < abs(ATT_SC))/n())

# overall_winner: SCMO 46% of the time, rest, SC wins.
# Add a column indicating when SCMO wins
final_placebo_results <- final_placebo_results %>%
  mutate(SCMO_wins = abs(ATT_SCMO) < abs(ATT_SC))

placebo_coordinates <- data %>% 
    select(geometry_name, centroid_lat, centroid_lon) %>% 
    distinct()

final_placebo_results <- final_placebo_results %>%
  left_join(placebo_coordinates, by = "geometry_name")

# Plot the map of state of Para, Brazil
# add road and river layers
# add a layer of points for the geometry_name, colored by SCMO_wins
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(dplyr)
library(geobr) # For brazilian borders
library(osmdata)
library(ggspatial)
# library(prettymapr)
# install.packages("prettymapr")


# Points
points_sf <- st_as_sf(final_placebo_results,
                     coords = c("centroid_lon", "centroid_lat"),
                     crs = 4326)

# Get a basemap using rnaturalearth (world map)
world <- ne_countries(scale = "medium", returnclass = "sf")
#  Load Pará state polygon
para <- read_state(code_state = "PA", year = 2020)

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

roads_osm <- opq(bbox = bb_small) %>% 
  add_osm_feature(
    key = "highway",
    value = c("motorway", "trunk", "primary", "secondary", "tertiary")
  ) %>%
  osmdata_sf()

# Extract line geometries (roads)
roads_sf <- roads_osm$osm_lines

# ggmap(basemap) +
#   geom_sf(data = para_wgs, inherit.aes = FALSE, fill = NA, color = "black") +
#   geom_sf(data = points_sf, aes(color = SCMO_wins), inherit.aes = FALSE)

ggplot() +
  annotation_map_tile(type = "osm") +
  geom_sf(data = para_wgs, fill = NA, color = "black") +
  geom_sf(data = points_sf, aes(color = SCMO_wins), size = 3) +
  scale_color_manual(values = c("red", "green"),
                     labels = c("SC wins", "SCMO wins")) +
  labs(title = "Placebo Test Results: SC vs SCMO",
       color = "Model Performance") +
  theme_minimal() 

# plotting
# ggplot() +
#   geom_sf(data = para, fill = "gray95", color = "gray80") +
#   geom_sf(data = points_sf, aes(color = SCMO_wins), size = 3) +
#   scale_color_manual(values = c("red", "green"), labels = c("SC wins", "SCMO wins")) +
#   labs(title = "Placebo Test Results: SC vs SCMO", color = "Model Performance") +
#   theme_minimal()


# --- try reinstalling basemaps 
# use: https://cran.r-project.org/web/packages/basemaps/readme/README.html#supported-services-and-maps

# install.packages("basemaps")
library(basemaps)
data(ext)
# view all available maps
get_maptypes()

# draw extent with draw_ext()
ext <- draw_ext()



basemap(ext, map_service = "osm", map_type = "streets")


# install.packages("maptiles")
# library(maptiles)

# tiles <- get_tiles(
#   para,
#   provider = "OpenStreetMap",
#   zoom = 6
# )

# ggplot() +
#   geom_spatraster(data = tiles) +
#   geom_sf(data = para, fill = NA, color = "black") +
#   geom_sf(data = points_sf, aes(color = SCMO_wins), size = 3)

# library(ggmap)



st_write(points_sf, "results/donor_points_att.geojson", delete_dsn = TRUE)
# st_write(para, "results/para.geojson", delete_dsn = TRUE)

library(sf)

st_write(points_sf, "results/donor_points_att.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)

# ---- Merging this result with original synth method ---

scmo_result <- read.csv("results/area_placebo_test_ATT_SC_vs_SCMO_v01.csv")
sc_result <- read.csv("results/area_placebo_original_synth_datav2_v01.csv")
sc_result <- sc_result %>%
  rename(ATT_SC_original = ATT_SC )

View(head(scmo_result))
View(head(sc_result))

# Merge the two results
merged_results <- sc_result %>%
  left_join(scmo_result, by = "geometry_name")
View(head(merged_results))

final_placebo_results <- merged_results

final_placebo_results <- final_placebo_results %>%
  mutate(SCMO_wins_original = abs(ATT_SCMO) < abs(ATT_SC_original))

placebo_coordinates <- data %>% 
    select(geometry_name, centroid_lat, centroid_lon) %>% 
    distinct()

final_placebo_results <- final_placebo_results %>%
  left_join(placebo_coordinates, by = "geometry_name")

View(head(final_placebo_results))

points_sf <- st_as_sf(final_placebo_results,
                     coords = c("centroid_lon", "centroid_lat"),
                     crs = 4326)


st_write(points_sf, "results/donor_points_att_datav2_v01.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)
sum(final_placebo_results$SCMO_wins_original)  #60
View(points_sf)
