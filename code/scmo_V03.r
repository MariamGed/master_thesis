# Created March 28, 2026

# Evaluate impact of conservation on 'treated' area using SCMO

# Libraries
library(readr)
library(magrittr)
library(dplyr)
library(panelView)
library(augsynth)
library(tidyverse)

# Data
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_V4.csv")

# Drop column .geo and .index
data$.geo <- NULL
data$system.index <- NULL

data$geometry_name <- data$geometry_name %>% as.factor()
# drop rows with at least one NA value across all columns
data <- data %>%
  group_by(geometry_name) %>%
  filter(!any(is.na(NDVI))) %>%
  ungroup() %>%
  as.data.frame()

run_synth <- function(data, treatment_point, treatment_year){
    # Data prep
    data$treatment <- 0
    data$treatment[data$geometry_name == treatment_point & data$year >= treatment_year] <- 1
    data$treatment <- as.numeric(data$treatment)
    data$geometry_name <- as.factor(data$geometry_name)
    # Sort the data by geometry_name and year
    data <- data[order(data$geometry_name, data$year), ]

    outcome <- augsynth(deforestation + NDVI + SR_B1 + SR_B2 + SR_B5 ~ treatment, geometry_name, year, data, t_int=treatment_year, progfunc = 'Ridge', scm=T)
    
    b <- summary(outcome)
    bdf <- as.data.frame(b$average_att)
    att <- bdf %>%
        filter(Outcome == "deforestation") %>%
        select(Estimate)

    return(list(treated_point = treatment_point, ATT =as.numeric(att), full_outcome = outcome))
}


try1 <- run_synth(data, "treated", 2012) 
scmo_outcome <- try1[["full_outcome"]]
plot(scmo_outcome, grid_size = 1)

# make a gaps plot: treated vs synthetic control
weights <- scmo_outcome[["weights"]]
# make weights into a df
weights_df <- as.data.frame(weights)
weights_df$geometry_name <- rownames(weights_df)
weights_df <- weights_df %>%
  rename(weight = V1)

# add weights to data
data <- data %>%
    left_join(weights_df, by = "geometry_name")

# Calculate synthetic deforestation
data_with_synthetic <- data %>%
    group_by(year) %>%
    mutate(synthetic_deforestation = sum(deforestation * weight, na.rm = TRUE)) %>%
    ungroup() 


# plot treated vs synthetic deforestation
treated_deforestation <- data_with_synthetic %>%
    filter(geometry_name == "treated") %>%
    select(year, deforestation) 

synth_deforestation <- data_with_synthetic %>%
    group_by(year) %>%
    summarise(synthetic_deforestation = unique(synthetic_deforestation))

deforestation_comparison <- treated_deforestation %>%
    left_join(synth_deforestation, by = "year")

# plot 
plt <- ggplot(deforestation_comparison, aes(x = year)) +
    geom_line(aes(y = deforestation, color = "Treated")) +
    geom_line(aes(y = synthetic_deforestation, color = "Synthetic Control"), linetype = "dashed") +
    labs(title = "Deforestation: Treated vs Synthetic Control", y = "Deforestation", color = "Legend") +
    theme_minimal()

print(plt)

# save deforestation_comparison
write.csv(deforestation_comparison, "results/scmo_treated_deforestation_gaps.csv", row.names = FALSE)


# apply run_synth to all donor areas
# Remove treated from the list of geometry_name
data_donors <- data %>%
  filter(geometry_name != "treated")


sc_placebo_results <- sapply(unique(data_donors$geometry_name), function(x) run_synth(data_donors, x, 2012))
sc_placebo_results <- as.data.frame(sc_placebo_results)
View(sc_placebo_results)

# drop the row full outcome from the placebo results
sc_placebo_results <- sc_placebo_results[-which(rownames(sc_placebo_results) == "full_outcome"), ]


# Make df long
sc_placebo_results <- sc_placebo_results %>%
  filter(rownames(sc_placebo_results) == "ATT") %>% # only select raw ATT
  pivot_longer(cols = everything(), names_to = "geometry_name", values_to = "ATT") %>%
  rename(ATT_SCMO = ATT)


sc_placebo_results$ATT_SCMO <- as.numeric(sc_placebo_results$ATT_SCMO)

# Save the placebo results
write.csv(sc_placebo_results, "results/area_placebo_scmo_datav4_v01.csv", row.names = FALSE)

