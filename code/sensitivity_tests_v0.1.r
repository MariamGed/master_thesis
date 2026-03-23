'
Sensitivity tests

Test one: Treatment area placibo 

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
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")
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
sc_placebo_results <- sapply(head(unique(data$geometry_name)), function(x) att_synth_methods(data, x, 2012, model = "SC"))
sc_placebo_results <- as.data.frame(sc_placebo_results)

scmo_placebo_results <- sapply(head(unique(data$geometry_name)), function(x) att_synth_methods(data, x, 2012, model = "SCMO"))
scmo_placebo_results <- as.data.frame(scmo_placebo_results)

# Merge the results

# Make df long
sc_placebo_results <- sc_placebo_results %>%
  filter(rownames(sc_placebo_results) == "ATT") %>% # only select raw ATT
  pivot_longer(cols = everything(), names_to = "geometry_name", values_to = "ATT") %>%
  rename(ATT_SC = ATT)

scmo_placebo_results <- scmo_placebo_results %>%
  filter(rownames(scmo_placebo_results) == "ATT") %>% # only select raw ATT
  pivot_longer(cols = everything(), names_to = "geometry_name", values_to = "ATT") %>%
  rename(ATT_SCMO = ATT)

final_placebo_results <- sc_placebo_results %>%
  left_join(scmo_placebo_results, by = "geometry_name")

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
ggsave("figures/area_placebo_test_ATT_comparison_v01.png", width = 10, height = 6)

# # single outcome
# single_outcome2 <- fect(deforestation ~ treatment + SR_B1 + SR_B2 + SR_B5, data = final_data, index = c("geometry_name","year"), 
#                         method = "gsynth", force = "two-way", CV = FALSE, r = c(0, 5), 
#                         se = TRUE,  vartype = 'parametric', 
#                         parallel = FALSE) # + X1 + X2 # nboots = 1000,
# print(single_outcome2)

# print(single_outcome2[["est.att"]])
# single_att <- single_outcome2[["est.att"]]
# plot(single_att$ATT)

