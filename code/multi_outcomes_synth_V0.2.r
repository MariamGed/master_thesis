# Load libraries
library(magrittr)
library(dplyr)
library(augsynth)

# Load the data
setwd("/Users/mariamgedenidze/Desktop/YSE Thesis/master_thesis/")
#data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2019_mean_annual_V3.csv")
#data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_V4.csv")
View(data)
# ---- Data cleaning and formatting ----

# Add a column treatment, called 'treatment'
data$treatment <- 0

# If geometry_name=='treated', then treatment=1, else treatment=0
data$treatment[data$geometry_name == "treated" & data$year >= 2012] <- 1
data$treatment <- data$treatment %>% as.numeric()
data$geometry_name <- data$geometry_name %>% as.factor()
# Sort the data by geometry_name and year
data <- data[order(data$geometry_name, data$year), ]

# Drop column .geo and .index
data$.geo <- NULL
data$system.index <- NULL

# Check for missing values
sum(is.na(data))
# View where the missing values are
panelview(NDVI ~ treatment, data = data, index = c("geometry_name", "year"), pre.post = TRUE, ylab = "Controls")


# --- cleaning for V2 data, hardcoded --- 
# Donor20.0, 2013 is missing: donor data in 2013 is missing for all outcomes but deforestation
# Drop the year 2013 for all observations
#data[data$year == 2013, ]

# This is hardcoded for the full 100 donor pool dataset
#data <- data %>% filter(year != 2013)

# Donor 5, 60 in 2007 are NA
# Drop donor 5 and 60 all together
#View(data[data$geometry_name =="donor60.0",])

#data <- data %>% filter(!(geometry_name %in% c("donor5.0", "donor60.0")))

# ----- End of cleaning V2 hardcoded cleanup -----


# Drop outliers, 1.5 std deviation from the mean (Introduces missing values )
#data <- data %>% filter(deforestation < mean(data$deforestation, na.rm = TRUE) + 1.5 * sd(data$deforestation, na.rm = TRUE))
#data <- data %>% filter(deforestation > mean(data$deforestation, na.rm = TRUE) - 1.5 * sd(data$deforestation, na.rm = TRUE))

# --- Standardise the data ----
# Standardise the outcome variables
#data$deforestation <- (data$deforestation - mean(data$deforestation, na.rm = TRUE)) / sd(data$deforestation, na.rm = TRUE)
#data$NDVI <- (data$NDVI - mean(data$NDVI, na.rm = TRUE)) / sd(data$NDVI, na.rm = TRUE)
#mean(data$NDVI, na.rm = TRUE) # Checking that the mean is ~0

# Standardise the covariates
#data$SR_B1 <- (data$SR_B1 - mean(data$SR_B1, na.rm = TRUE)) / sd(data$SR_B1, na.rm = TRUE)
#data$SR_B2 <- (data$SR_B2 - mean(data$SR_B2, na.rm = TRUE)) / sd(data$SR_B2, na.rm = TRUE)
#data$SR_B3 <- (data$SR_B3 - mean(data$SR_B3, na.rm = TRUE)) / sd(data$SR_B3, na.rm = TRUE)
#data$SR_B5 <- (data$SR_B5 - mean(data$SR_B5, na.rm = TRUE)) / sd(data$SR_B5, na.rm = TRUE)
#data$SR_B7 <- (data$SR_B7 - mean(data$SR_B7, na.rm = TRUE)) / sd(data$SR_B7, na.rm = TRUE)
#data$mean_agri_proba <- (data$mean_agri_proba - mean(data$mean_agri_proba, na.rm = TRUE)) / sd(data$mean_agri_proba, na.rm = TRUE)
#data$mean_logging_proba <- (data$mean_logging_proba - mean(data$mean_logging_proba, na.rm = TRUE)) / sd(data$mean_logging_proba, na.rm = TRUE)
#data$mean_elevation <- (data$mean_elevation - mean(data$mean_elevation, na.rm = TRUE)) / sd(data$mean_elevation, na.rm = TRUE)
#data$mean_slope <- (data$mean_slope - mean(data$mean_slope, na.rm = TRUE)) / sd(data$mean_slope, na.rm = TRUE)
# View the data


# ----- Demeaning data -------
#  de-meaning outcomes across pre-treatment periods within each unit’s outcome series.
'''
# Havent yet done this
data <- data %>%
     group_by(geometry_name) %>%
     mutate(deforestation = deforestation - mean(deforestation[year < 2012], na.rm = TRUE),
            SR_B1 = SR_B1 - mean(SR_B1[year < 2012], na.rm = TRUE),
            SR_B2 = SR_B2 - mean(SR_B2[year < 2012], na.rm = TRUE),
            SR_B3 = SR_B3 - mean(SR_B3[year < 2012], na.rm = TRUE)
            ) %>%
     ungroup()
'''

# ---- Running single outcome synth control ----
# Synth control augmented with Ridge regression
syn_deforestation <- augsynth(deforestation ~ treatment | SR_B1 + SR_B2 + SR_B3 + mean_agri_proba + mean_logging_proba + mean_elevation + mean_slope, geometry_name, year, data, progfunc = 'Ridge', scm=T) 
plot(syn_deforestation, inf_type = "jackknife+") # Pointwise confidence interval)
plot(syn_deforestation, cv = T)
summary(syn_deforestation) # Two-tail hypothesis test
summary(syn_deforestation, stat_func = function(x) -sum(x)) # One-tail hypothesis test against positive effects)
summary(syn_deforestation, stat_func = function(x) abs(x)) # One-tail test for the average post-treatment effect

syn_deforestation <- augsynth(deforestation ~ treatment, geometry_name, year, data, progfunc = 'None', scm=T) 
summary(syn_deforestation)
plot(syn_deforestation, inf_type = "jackknife+") # Pointwise confidence interval)

View(syn_deforestation)
sum(syn_deforestation$synw) # sum of weights should be 1

# ---- understanding how balance was improved ---
# Elevation for treated unit
mean_treated_elevation <- data$mean_elevation[1]
# Elevation for synthetic control
weights <- syn_deforestation$synw
data_donor_only <- data %>% filter(geometry_name != "treated")
mean_synth_elevation <- data_donor_only$mean_elevation * weights
#sum(mean_synth_elevation)
#mean_treated_elevation
hist(weights, breaks = 30, main = "Histogram of Donor Weights", xlab = "Weights", ylab = "Frequency")
# Histogram shows that only 6 donors have non-zero weights

# ---- Running multiple outcome synth control ---- 
syn_multi <- augsynth(deforestation + SR_B1 + SR_B2 + SR_B3 ~ treatment, geometry_name, year, data, progfunc = 'None', scm=T)
syn_deforestation <- augsynth(deforestation ~ treatment | SR_B1 + SR_B2 + SR_B3, geometry_name, year, data, progfunc = 'None', scm=T)
summary(syn_multi)
summary(syn_deforestation)
plot(syn_deforestation)

syn_multi[1] # = weights for each donor
syn_multi[2] # l2 balance

# View the summary
# This unpacks info about:
# Estimated treatment effects for each outcome var/year
# Average outcome estimate across every year.
# Average_att in 2012 for each outcome variable.
View(summary(syn_multi))

# This is to get the av treatment effect for each outcome var.
# I need to construct CIs on these manually via bootstrapping
summary(syn_multi)$average_att$Estimate # [1] is for the first outcome.

a <- summary(syn_multi)$average_att$Estimate[1]

# export the weights
weights <- syn_multi$w
weights <- as.data.frame(weights)
colnames(weights) <- c("weights")
weights$geometry_name <- rownames(weights)
# save the weights
write.csv(weights, "data/multi_outcomes_donor_weightsV2.csv", row.names = FALSE)

weights <- read.csv("data/multi_outcomes_donor_weightsV1.csv")
hist(weights$weights, breaks = 30, main = "Histogram of Donor Weights", xlab = "Weights", ylab = "Frequency")

# ----- Bootsrapping for multiple outcome synth control -----

library(augsynth)
library(dplyr)
library(purrr)
library(tidyr)

# Set number of bootstrap replications
B <- 100
set.seed(123)

# Store treatment effects from each bootstrap
boot_taus <- numeric(B)

# Get only control units for resampling
controls <- data %>% filter(geometry_name != "treated") 
# Extract treated unit names (no resampling of treated)
boot_treated <- data %>% filter(geometry_name == "treated") %>%
     mutate(new_name = geometry_name) # keep the name same

# Resample control units (with replacement)
for (b in 1:B) {
     boot_controls <- controls %>%
          group_by(geometry_name) %>%
          nest() %>% #takes all the other columns and wraps them into a single list-column
          sample_frac(size = 0.8, replace = TRUE) %>%
          # Add a unique bootstrap ID to handle duplicates
          mutate(boot_id = row_number(), # boot_id is always 1?
               new_name = paste0(geometry_name, "_boot", boot_id)) %>%
          unnest(cols = c(data)) %>% 
          ungroup() %>%
          select(-boot_id) # remove the boot_id column

     boot_data <- bind_rows(
          boot_treated,
          boot_controls
          )
     # Refit model
     fit_b <- augsynth(deforestation + SR_B1 + SR_B2 + SR_B3 ~ treatment, 
                         new_name, year, boot_data,
                         progfunc = "None", scm = TRUE)
     #print(summary(fit_b)$average_att$Estimate[1])
     # Store average treatment effect if successful (specifically for the 1st outcome, deforestation in this case)
     att <- summary(fit_b)$average_att$Estimate[1]
     boot_taus[b] <- att
     print(att)
}

# Compute CI
quantile(boot_taus, probs = c(0.025, 0.975))


# ---- using gsynth for single outcome model + panel synth control ----
library(gsynth) # For generalised synth control 
library(panelView) # For visualising panel data

# Take subset of 10 donors + treatment
#data_subset <- data %>% filter(geometry_name %in% c("donor1.0", "donor2.0", "donor3.0", "donor4.0", "donor5.0", "donor6.0", "donor7.0", "donor8.0", "donor9.0", "donor10.0", "treated"))

#panelview(deforestation ~ treatment, data = data_subset, index = c("geometry_name", "year"), pre.post = TRUE, ylab = "Controls", collapse.history = "TRUE")
panelview(NDVI ~ treatment, data = data, index = c("geometry_name", "year"), type = "outcome")

# full data panel_view
panelview(deforestation ~ treatment, data = data, index = c("geometry_name", "year"), pre.post = TRUE, ylab = "Controls")
panelview(deforestation ~ treatment, data = data, index = c("geometry_name", "year"), type = "outcome")


# Run gsynth
system.time(
    out <- gsynth(deforestation + 'SR_B1' + 'SR_B2' + 'SR_B3' ~ treatment, data = data, # + 'SR_B1' + 'SR_B2' + 'SR_B3'
                  index = c("geometry_name","year"), force = "two-way",
                  CV = TRUE, r = c(0, 5), se = TRUE, 
                  inference = "parametric", nboots = 1000, 
                  parallel = TRUE)
)

print(out)
plot(out)
out$est.avg
out$est.beta
plot(out, type = "counterfactual", raw = "none", main="")
plot(out, type = "counterfactual", raw = "all")
plot(out, type = "counterfactual", raw = "band", 
     xlab = "Time")#, ylim = c(-30,25))

# Plot the distribution of $deforestation of non-treated units
library(ggplot2)

ggplot(data, aes(x = deforestation)) +
    geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Distribution of Deforestation in Non-Treated Units",
         x = "Deforestation",
         y = "Frequency") +
    theme_minimal()

# Find outliers of deforestation
outliers <- boxplot.stats(data$deforestation)$out
outliers
# plot a boxplot to visualise the outliers
boxplot(data$deforestation, main = "Boxplot of Deforestation", ylab = "Deforestation")



# -------- Placebo tests --------

# TEST 1: Unit placebo: Assign treatment to a control unit and see if we find an effect

# Assign one of the control units as treated in 2012
data_placebo <- data
data_placebo <- data_placebo %>%
     mutate(treatment = ifelse(geometry_name == "donor2.0" & year >= 2012, 1, 0)) %>%
     mutate(geometry_name = ifelse(geometry_name == "donor2.0", "treated_placebo", geometry_name))
View(data_placebo)

#sum(data_placebo$treatment==1)

# Run both synth control models on the placebo data
syn_multi <- augsynth(deforestation + SR_B1 + SR_B2 + SR_B3 ~ treatment, geometry_name, year, data_placebo, progfunc = 'None', scm=T)
syn_deforestation <- augsynth(deforestation ~ treatment | SR_B1 + SR_B2 + SR_B3, geometry_name, year, data_placebo, progfunc = 'None', scm=T)

View(summary(syn_multi))
summary(syn_deforestation)
View(syn_multi)

typeof(summary(syn_multi))

# --- Randomisation inference for placebo test 1 ----

# pseudocode
# generate datasets with placebo treatment assigned to each control unit
# run synth control on each dataset and store the att
# compute rmspe pre and post for each control unit, standardise by #time periods pre and post
# (???) I cannot get rmse from augsynth object

# TEST 2: Time placebo: Assign treatment to pre-treatment period and see if we find an effect

data_time_placebo <- data
data_time_placebo <- data_time_placebo %>%
     mutate(treatment = ifelse(geometry_name == "treated" & year >= 2008, 1, 0)) # Placebo treatment starts in 2008 instead of 2012

syn_multi <- augsynth(deforestation + SR_B1 + SR_B2 + SR_B3 ~ treatment, geometry_name, year, data_placebo, progfunc = 'None', scm=T)
summary(syn_multi)


#
syn_multi <- augsynth(deforestation ~ treatment|SR_B1 + SR_B2 + SR_B3, geometry_name, year, data, progfunc = 'None', scm=T)
summary(syn_multi)
syn_multi
colnames(data)
