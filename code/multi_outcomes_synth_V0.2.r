# Load libraries
library(magrittr)
library(dplyr)
library(augsynth)

# Load the data
setwd("/Users/mariamgedenidze/Desktop/YSE Thesis/master_thesis/")
#data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2019_mean_annual_V3.csv")
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V1.csv")

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


syn_NDVI <- augsynth(deforestation + NDVI ~ treatment, geometry_name, year, data, progfunc = 'None', scm=T) # multiple treated points, so running multisynth
summary(syn_NDVI)
plot(syn_NDVI)

# using gsynth instead
library(gsynth) # For generalised synth control 
library(panelView) # For visualising panel data

panelview(deforestation ~ treatment, data = data, index = c("geometry_name", "year"), pre.post = TRUE)
panelview(NDVI ~ treatment, data = data, index = c("geometry_name", "year"), type = "outcome")


# Run gsynth
system.time(
    out <- gsynth(deforestation ~ treatment + SR_B1 + SR_B2 + SR_B5 + SR_B7, data = data,
                  index = c("geometry_name","year"), force = "two-way",
                  CV = TRUE, r = c(0, 3), se = TRUE, 
                  inference = "parametric", nboots = 1000, 
                  parallel = TRUE)
)

print(out)
out$est.avg
out$est.beta
plot(out, type = "counterfactual", raw = "none", main="")
plot(out, type = "counterfactual", raw = "all")

