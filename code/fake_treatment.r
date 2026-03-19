# Find an area that WILL have some deforestation, and compare the two methods. 
# normal synth cant work well in this because the deforestation =0 in the pre-treat period. 
# Potentially look backwards in time. Take an area close to the road/intersecting a road to 
# capture some deforestation risk.

# Libraries
library(readr)
library(magrittr)
library(dplyr)
library(panelView)
library(augsynth) # multiple outcome synth 
library(fect) # single outcome gsynth
library(ggplot2)
library(tidyr)

''' Finding  a "fake" treatment point
# Load data
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")

# plot deforestation variable for the donor pool to identify a "fake" treatment area, that
# we know will experience deforestation
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
panelview(deforestation ~ treatment, data = data,  index = c("geometry_name","year"), type = "outcome") 
# View(data)


# Finding an area which has a spike in deforestation

#list of donor points
donors <- data %>% 
    filter(geometry_name != "treated") %>% 
    select(geometry_name) %>% 
    distinct() %>% 
    pull()

years <- data %>% 
    select(year) %>% 
    distinct() %>% 
    pull()

find_spikes <- function(data) {
  candidates <- c() 
  for (i in donors) {
    temp <- data %>% filter(geometry_name == i) %>% arrange(year)
    deforestation <- temp$deforestation
    n <- length(deforestation)
    for (t in 5:(n-1)) {   # require some baseline years
        pre_mean <- mean(deforestation[1:(t-1)])
        spike <- deforestation[t]

        if(pre_mean < 0.001 & spike > 5 * (pre_mean + 1e-6)) {
        candidates <- rbind(candidates, c(i, t))
        }
    }
  }
  return(candidates)
}

results <- find_spikes(data)
print(results)

# plot deforestation time series for "donor19.0", vertical line at year  "10"
ggplot(data %>% filter(geometry_name == "donor19.0"), aes(x = year, y = deforestation)) +
  geom_line() +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "red") +
  labs(title = "Deforestation Time Series for donor19.0", x = "Year", y = "Deforestation") +
  theme_minimal()

# donor 19 seems like a good fit with very low deforestation until 2010 and a spike afterwards.

coordinates_donor19 <- data %>% 
    filter(geometry_name == "donor19.0") %>% 
    select(centroid_lat, centroid_lon) %>% 
    distinct()
coordinates_donor19 # close to river, not road
'''

# ---- Test -----

# I will create a face treatment point from donor19.0, with treatment starting in 2010, and then compare the two methods.

# Load data
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")
panelview(NDVI ~ treatment, data = data,  index = c("geometry_name","year")) 

# plot deforestation variable for the donor pool to identify a "fake" treatment area, that
# we know will experience deforestation
treatment_year <- 2010
fake_treatment_point <- "donor19.0"

# Add a column treatment, called "treatment"
data$treatment <- 0

# If geometry_name=='treated', then treatment=1, else treatment=0
data$treatment[data$geometry_name == fake_treatment_point & data$year >= treatment_year] <- 1
data$treatment <- data$treatment %>% as.numeric()
data$geometry_name <- data$geometry_name %>% as.factor()
# Sort the data by geometry_name and year
data <- data[order(data$geometry_name, data$year), ]

# Drop column .geo and .index
data$.geo <- NULL
data$system.index <- NULL


final_data <- data
# Run the models

# ------ Multiple outcomes with augsynth package ------
single_outcome_aug <- augsynth(deforestation ~ treatment, geometry_name, year, final_data, t_int=2010, progfunc = 'Ridge', scm=T)
# | SR_B1 + SR_B2 + SR_B5

# what are weights vs synw? --> 
# from R/augsynth.R it could be that synw are when progfunc = 'Ridge' and weights are when progfunc = else

# hist(single_outcome_aug$weights)

sc_weights <- data.frame(
  geometry_name = rownames(single_outcome_aug$weights),
  weight = single_outcome_aug$weights[,1]
)

data_subset <- data %>%
  select(deforestation, geometry_name, year)

# data_subset to: geometry_name in rows, year in columns, and deforestation as matrix values
real_deforestation <- data_subset %>%
  pivot_wider(names_from = year, values_from = deforestation)

sc_deforestation <- real_deforestation %>%
  left_join(sc_weights, by = "geometry_name") %>%
  filter(geometry_name != fake_treatment_point) %>%
  mutate(across(-c(geometry_name, weight), ~ . * weight))

sc_deforestation_agg <- sc_deforestation %>%
  select(-weight) %>%
  summarise(across(-geometry_name, sum)) %>%
  mutate(geometry_name = "synthetic_control")

true_deforestation <- real_deforestation %>%
  filter(geometry_name == fake_treatment_point) 

df_comparison_sc <- bind_rows(true_deforestation, sc_deforestation_agg)
difference_2010_sc <- df_comparison_sc[df_comparison_sc$geometry_name == "synthetic_control", "2010"] - df_comparison_sc[df_comparison_sc$geometry_name == fake_treatment_point, "2010"]
# make this long: columns to year, rows to two columns, treated and synthetic control.

df_comparison_sc <- df_comparison_sc %>%
  pivot_longer(cols = -geometry_name, names_to = "year", values_to = "deforestation") %>%
  pivot_wider(names_from = geometry_name, values_from = deforestation)

df_comparison_sc <- df_comparison_sc %>%
  mutate(pred_diff = synthetic_control - donor19.0)


# Difference in 2010
difference_2010_sc <- df_comparison_sc[df_comparison_sc$year == "2010", "pred_diff"]
# calculate rmse prior to 2010
rmse_sc_pre2010 <- sqrt(mean((df_comparison_sc %>% filter(year < 2010))$pred_diff^2, na.rm = TRUE))
# calculate rmse post 2010
rmse_sc_post2010 <- sqrt(mean((df_comparison_sc %>% filter(year >= 2010))$pred_diff^2, na.rm = TRUE))

print(paste("SC, rmse pre 2010:", round(rmse_sc_pre2010, 5), "SC, rmse post 2010:", round(rmse_sc_post2010, 5)))


# Prediction using synthetic control with multipe outcomes (SCMO)

# This model does not like NAs

# Y2013 missing for most donor pool
data <- data %>% filter(year != 2013)
# 2 missing values left for NDVI(&rest of the vars), removing the rows with missing values
data <- data %>% filter(!is.na(NDVI))
sum(is.na(data))

final_data <- data
# Run  SCMO
multi_outcomes <- augsynth(deforestation + NDVI + SR_B1 + SR_B2 + SR_B5 ~ treatment, geometry_name, year, final_data, t_int=2010, progfunc = 'Ridge', scm=T)


scmo_weights <- data.frame(
  geometry_name = rownames(multi_outcomes$weights),
  weight = multi_outcomes$weights[,1]
)

data_subset <- final_data %>%
  select(deforestation, geometry_name, year)

# data_subset to: geometry_name in rows, year in columns, and deforestation as matrix values
real_deforestation <- data_subset %>%
  pivot_wider(names_from = year, values_from = deforestation)

scmo_deforestation <- real_deforestation %>%
  left_join(scmo_weights, by = "geometry_name") %>%
  filter(geometry_name != fake_treatment_point) %>%
  mutate(across(-c(geometry_name, weight), ~ . * weight))

scmo_deforestation_agg <- scmo_deforestation %>%
  select(-weight) %>%
  summarise(across(-geometry_name, sum)) %>%
  mutate(geometry_name = "synthetic_control")

true_deforestation <- real_deforestation %>%
  filter(geometry_name == fake_treatment_point) 

df_comparison_scmo <- bind_rows(true_deforestation, scmo_deforestation_agg)


df_comparison_scmo <- df_comparison_scmo %>%
  pivot_longer(cols = -geometry_name, names_to = "year", values_to = "deforestation") %>%
  pivot_wider(names_from = geometry_name, values_from = deforestation)

df_comparison_scmo <- df_comparison_scmo %>%
  mutate(pred_diff = synthetic_control - donor19.0)

# Difference in 2010
difference_2010_scmo <- df_comparison_scmo[df_comparison_scmo$year == "2010", "pred_diff"]

# calculate rmse prior to 2010
rmse_scmo_pre2010 <- sqrt(mean((df_comparison_scmo %>% filter(year < 2010))$pred_diff^2, na.rm = TRUE))
# calculate rmse post 2010
rmse_scmo_post2010 <- sqrt(mean((df_comparison_scmo %>% filter(year >= 2010))$pred_diff^2, na.rm = TRUE))

print(paste("SCMO, difference in 2010:", round(difference_2010_scmo,5), " rmse pre 2010:", round(rmse_scmo_pre2010, 5), "SCMO, rmse post 2010:", round(rmse_scmo_post2010, 5)))
print(paste("SC difference in 2010:", round(difference_2010_sc,5), "rmse pre 2010:", round(rmse_sc_pre2010, 5), "SC, rmse post 2010:", round(rmse_sc_post2010, 5)))



# I define a function that computes all steps above:

scmo <- function(data, treatment_point, treatment_year, model = c("SC", "SCMO")){
    # Data prep
    # Add a column treatment, called "treatment"
    data$treatment <- 0

    # If geometry_name==treatment_point, then treatment=1, else treatment=0
    data$treatment[data$geometry_name == treatment_point & data$year >= treatment_year] <- 1
    data$treatment <- data$treatment %>% as.numeric()
    data$geometry_name <- data$geometry_name %>% as.factor()
    # Sort the data by geometry_name and year
    data <- data[order(data$geometry_name, data$year), ]

    if (model == "SC") {
      outcome <- augsynth(deforestation ~ treatment, geometry_name, year, data, t_int=2010, progfunc = 'Ridge', scm=T)
    }
    else if (model == "SCMO"){
      # Y2013 missing for most donor pool
      data <- data %>% filter(year != 2013)
      # 2 missing values left for NDVI(&rest of the vars), removing the rows with missing values
      data <- data %>% filter(!is.na(NDVI))
      outcome <- augsynth(deforestation + NDVI + SR_B1 + SR_B2 + SR_B5 ~ treatment, geometry_name, year, data, t_int=2010, progfunc = 'Ridge', scm=T)
    }
    # run scmo
    sc_weights_df <- data.frame(
      geometry_name = rownames(outcome$weights),
      weight = outcome$weights[,1]
    )

    data <- data %>%
      select(deforestation, geometry_name, year)

    # data_subset to: geometry_name in rows, year in columns, and deforestation as matrix values
    real_deforestation <- data %>%
      pivot_wider(names_from = year, values_from = deforestation)

    sc_deforestation <- real_deforestation %>%
      left_join(sc_weights_df, by = "geometry_name") %>%
      filter(geometry_name != treatment_point) %>%
      mutate(across(-c(geometry_name, weight), ~ . * weight))

    sc_deforestation <- sc_deforestation %>%
      select(-weight) %>%
      summarise(across(-geometry_name, sum)) %>%
      mutate(geometry_name = "synthetic_control")
    #View(head(sc_deforestation))
    true_deforestation <- real_deforestation %>%
      filter(geometry_name == treatment_point) 
    #View(head(true_deforestation))
    df_comparison_sc <- bind_rows(true_deforestation, sc_deforestation)
    
    df_comparison_sc <- df_comparison_sc %>%
      pivot_longer(cols = -geometry_name, names_to = "year", values_to = "deforestation") %>%
      pivot_wider(names_from = geometry_name, values_from = deforestation) %>%
      rename(treated = treatment_point)

    df_comparison_sc <- df_comparison_sc %>%
      mutate(pred_diff = synthetic_control - treated)
    View(df_comparison_sc)
    return (df_comparison_sc)
}


try <- scmo(data = data, treatment_point = "donor19.0", treatment_year = 2010, model = "SC")
View(try)
