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

''' Finding  a "fake" treatment point '
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

# save plot
ggsave("figures/deforestation_whole_sample_data_V02.png", width = 10, height = 6)

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

        if(pre_mean < 0.002 & spike > 10 * (pre_mean + 1e-6)) {
        candidates <- rbind(candidates, c(i, t))
        }
    }
  }
  return(candidates)
}

results <- find_spikes(data)
print(results)

# plot deforestation time series for "donor17.0", vertical line at year  "10"
ggplot(data %>% filter(geometry_name == "donor17.0"), aes(x = year, y = deforestation)) +
  geom_line() +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "red") +
  labs(title = "Deforestation Time Series for donor17.0", x = "Year", y = "Deforestation") +
  theme_minimal()

# previous try: donor19.0
# donor 17 seems like a good fit with very low deforestation until 2010 and a spike afterwards.

coordinates_donor17 <- data %>% 
    filter(geometry_name == "donor17.0") %>% 
    select(centroid_lat, centroid_lon) %>% 
    distinct()
coordinates_donor17 # close to a road
''

2006_deforestations <- data %>%
    filter(year == 2006) %>% 
    select(geometry_name, deforestation) %>% 
    arrange(deforestation)

View(data[data$year==2006, ])

# ---- Test -----
# I will create a face treatment point from donor17.0, with treatment starting in 2007, and then compare the two methods.

# Load data
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")
panelview(NDVI ~ treatment, data = data,  index = c("geometry_name","year")) 

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
      outcome <- augsynth(deforestation ~ treatment, geometry_name, year, data, t_int=treatment_year, progfunc = 'Ridge', scm=T)
    } #| NDVI + SR_B1 + SR_B2 + SR_B5
    else if (model == "SCMO"){
      # Y2013 missing for most donor pool
      data <- data %>% filter(year != 2013)
      # 2 missing values left for NDVI(&rest of the vars), removing the rows with missing values
      data <- data %>% filter(!is.na(NDVI))
      outcome <- augsynth(deforestation + NDVI + SR_B1 + SR_B2 + SR_B5 ~ treatment, geometry_name, year, data, t_int=treatment_year, progfunc = 'Ridge', scm=T)
    }
    # run sc
    sc_weights_df <- data.frame(
      geometry_name = rownames(outcome$weights),
      weight = outcome$weights[,1]
    )
    #View(sc_weights_df)
    data <- data %>%
      select(deforestation, geometry_name, year)

    # data_subset to: geometry_name in rows, year in columns, and deforestation as matrix values
    real_deforestation <- data %>%
      pivot_wider(names_from = year, values_from = deforestation)

    sc_deforestation <- real_deforestation %>%
      left_join(sc_weights_df, by = "geometry_name") %>%
      filter(geometry_name != treatment_point) %>%
      mutate(across(-c(geometry_name, weight), ~ . * weight))
    #View(sc_deforestation)

    sc_deforestation <- sc_deforestation %>%
      select(-weight) %>%
      summarise(across(-geometry_name, ~ sum(.x, na.rm = TRUE))) %>%
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
    #View(df_comparison_sc)

    treat_year_error <- df_comparison_sc %>% filter(year == treatment_year) %>% pull(pred_diff)
    rmse_pre_treat <- sqrt(mean((df_comparison_sc %>% filter(year < treatment_year))$pred_diff^2, na.rm = TRUE))
    # calculate rmse post "treatment" year
    rmse_post_treat <- sqrt(mean((df_comparison_sc %>% filter(year >= treatment_year))$pred_diff^2, na.rm = TRUE))
    return (list(outcome_df = df_comparison_sc, treatment_year_error = treat_year_error, rmse_pre_treat = rmse_pre_treat, rmse_post_treat = rmse_post_treat))
}


sc_donor17 <- scmo(data = data, treatment_point = "donor17.0", treatment_year = 2007, model = "SC")
scmo_donor17 <- scmo(data = data, treatment_point = "donor17.0", treatment_year = 2007, model = "SCMO")


sc_donor17
scmo_donor17

View(scmo_donor17$outcome_df)
View(sc_donor17$outcome_df)
