# Load libraries
library(magrittr)
library(dplyr)
library(augsynth)

# Load the data
setwd("/Users/mariamgedenidze/Desktop/YSE Thesis/master_thesis/")
#data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2019_mean_annual_V3.csv")
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")

# ---- Data cleaning and formatting ----

# Add a column treatment, called 'treatment'
data$treatment <- 0

# If geometry_name=='treated', then treatment=1, else treatment=0
data$treatment[data$geometry_name == "treated" & data$year >= 2012] <- 1
data$treatment <- data$treatment %>% as.numeric()
data$geometry_name <- data$geometry_name %>% as.factor()
# Sort the data by geometry_name and year
data <- data[order(data$geometry_name, data$year), ]

# Drop column .geo 
data$.geo <- NULL
data$system.index <- NULL
#View(data)
# Check for missing values
sum(is.na(data))
# See where are the missing values
data[is.na(data$deforestation), ]
# Donor20.0, 2013 is missing: bunch of donors in 2013 are missing.
# Drop the year 2013 for all observations

# This is hardcoded for the full 100 donor pool dataset
data <- data %>% filter(year != 2013)

# Donor 5, 60 in 2007 are NA
# Drop donor 5 and 60 all together
data <- data %>% filter(!(geometry_name %in% c("donor5.0", "donor60.0")))

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
#data$SR_B5 <- (data$SR_B5 - mean(data$SR_B5, na.rm = TRUE)) / sd(data$SR_B5, na.rm = TRUE)
#data$SR_B7 <- (data$SR_B7 - mean(data$SR_B7, na.rm = TRUE)) / sd(data$SR_B7, na.rm = TRUE)
# View the data

# ---- Running single outcome synth control ----
# Synth control augmented with Ridge regression
syn_deforestation <- augsynth(deforestation ~ treatment | SR_B1 + SR_B2 + SR_B7, geometry_name, year, data, progfunc = 'Ridge', scm=T) # multiple treated points, so running multisynth
plot(syn_deforestation, inf_type = "jackknife+") # Pointwise confidence interval)
summary(syn_deforestation) # Two-tail hypothesis test
summary(syn_deforestation, stat_func = function(x) -sum(x)) # One-tail hypothesis test against positive effects)
summary(syn_deforestation, stat_func = function(x) abs(x)) # One-tail test for the average post-treatment effect

plot(syn_deforestation, cv = T)


syn_deforestation <- augsynth(deforestation ~ treatment, geometry_name, year, data, progfunc = 'None', scm=T) # multiple treated points, so running multisynth
summary(syn_deforestation)
plot(syn_deforestation, inf_type = "jackknife+") # Pointwise confidence interval)


# ---- Running multiple outcome synth control ---- 
syn_multi <- augsynth(deforestation + SR_B1 + SR_B2 + SR_B3 ~ treatment, geometry_name, year, data, progfunc = 'None', scm=T)
syn_deforestation <- augsynth(deforestation ~ treatment | SR_B1 + SR_B2 + SR_B3, geometry_name, year, data, progfunc = 'None', scm=T)
summary(syn_multi)
summary(syn_deforestation)
plot(syn_deforestation)

# export the weights
weights <- syn_multi$w
weights <- as.data.frame(weights)
colnames(weights) <- c("weights")
weights$geometry_name <- rownames(weights)
# save the weights
write.csv(weights, "data/multi_outcomes_donor_weightsV2.csv", row.names = FALSE)

# ---- using gsynth instead ----
library(gsynth) # For generalised synth control 
library(panelView) # For visualising panel data

# Take subset of 10 donors + treatment
data_subset <- data %>% filter(geometry_name %in% c("donor1.0", "donor2.0", "donor3.0", "donor4.0", "donor5.0", "donor6.0", "donor7.0", "donor8.0", "donor9.0", "donor10.0", "treated"))

panelview(deforestation ~ treatment, data = data_subset, index = c("geometry_name", "year"), pre.post = TRUE, ylab = "Controls", collapse.history = "TRUE")
panelview(NDVI ~ treatment, data = data, index = c("geometry_name", "year"), type = "outcome")

# full data panel_view
panelview(deforestation ~ treatment, data = data, index = c("geometry_name", "year"), pre.post = TRUE, ylab = "Controls")
panelview(deforestation ~ treatment, data = data, index = c("geometry_name", "year"), type = "outcome")


# Run gsynth
system.time(
    out <- gsynth(deforestation ~ treatment, data = data,
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