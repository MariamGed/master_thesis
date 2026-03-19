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


setwd("/Users/mariamgedenidze/Desktop/YSE Thesis/master_thesis")
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")
#View(head(data,30))

# Constants
treatment_year <- 2012

# Add a column treatment, called 'treatment'
data$treatment <- 0

# If geometry_name=='treated', then treatment=1, else treatment=0
data$treatment[data$geometry_name == "treated" & data$year >= treatment_year] <- 1
data$treatment <- data$treatment %>% as.numeric()
data$geometry_name <- data$geometry_name %>% as.factor()
# Sort the data by geometry_name and year
data <- data[order(data$geometry_name, data$year), ]

# Drop column .geo and .index
data$.geo <- NULL
data$system.index <- NULL


# Cleaning Data
# Check for missing values
sum(is.na(data))
# View where the missing values are
panelview(NDVI ~ treatment, data = data, index = c("geometry_name", "year"), pre.post = TRUE, ylab = "Controls")

# Y2013 missing for most donor pool
data <- data %>% filter(year != 2013)

# 2 missing values left for NDVI(&rest of the vars), removing the rows with missing values
data <- data %>% filter(!is.na(NDVI))

# final check
 
sum(is.na(data))==0

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
treated_deforestation <- data$deforestation[data1$geometry_name=="treated"]
treated_SR_B5 <- data$SR_B5[data$geometry_name=="treated"]
treated_SR_B2 <- data$SR_B2[data$geometry_name=="treated"]
treated_SR_B1 <- data$SR_B1[data$geometry_name=="treated"]

cor(treated_SR_B1,treated_SR_B2)

# ----- End of data prep -----

# -------- Modelling ---------
# ------ Single outcome gsynthetic control, fect package ----

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


#  ---- Test 2: Treatment time placebo  -----
# Change treatment time from 2012 to 2010
# Result: see which model can better reproduce 0 ATT in year 2010 and 2011.

data1 <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")
#View(head(data,30))

# Constants
treatment_year <- 2010
treated_test_point <- "treated" #sample(unique(data1$geometry_name), size = 1)

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

final_data <- data1

# ------ Multiple outcomes with augsynth package ------
multi_outcomes3 <- augsynth(deforestation + NDVI ~ treatment, geometry_name, year, final_data, t_int=2010, progfunc = 'Ridge', scm=T)
# | SR_B1 + SR_B2 + SR_B5
summary(multi_outcomes3, grid_size = 3) 
#plot(multi_outcomes2, grid_size = 2)
#plot(multi_outcomes2[["weights"]])
c <- summary(multi_outcomes3, grid_size = 3) 
#View(c[["att"]])
att <- c[["att"]]

# plot treatment effect across time
plot(att$Time[att$Outcome=="deforestation"], att$Estimate[att$Outcome=="deforestation"])


plot(data1$year[data1$geometry_name=="treated"], data1$NDVI[data1$geometry_name=="treated"] )


# single outcome
single_outcome2 <- fect(deforestation ~ treatment + SR_B1 + SR_B2 + SR_B5, data = final_data, index = c("geometry_name","year"), 
                        method = "gsynth", force = "two-way", CV = FALSE, r = c(0, 5), 
                        se = TRUE,  vartype = 'parametric', 
                        parallel = FALSE) # + X1 + X2 # nboots = 1000,
print(single_outcome2)

print(single_outcome2[["est.att"]])
single_att <- single_outcome2[["est.att"]]
plot(single_att$ATT)

typeof(single_att)

single_att_df <- matrix(single_att, 19, 6)
colnames(single_att_df) <- c("ATT", 'S.E', 'CI.lower', 'CI.upper', 'p.value', 'count')
single_att_df <- as.data.frame(single_att_df)
single_att_df <- single_att_df %>%
  mutate(year = seq.int(2001, 2019)) # this is wrong, it should skip y2013 and add y2020

plot(single_att_df$year, single_att_df$ATT)

