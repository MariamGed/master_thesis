# Developing multiple outcomes control for conservation data

# Load libraries
library(magrittr)
library(dplyr)
library(augsynth)

# Load the data
setwd("/Users/mariamgedenidze/Desktop/YSE Thesis/master_thesis/")
data <- read.csv("data/combined_dataV0.2.csv")
View(data)

# Outcome measures: NDVI, yearly_loss, B3.

# Run single outcome synth control for each outcome variable we want to model later
syn_NDVI <- multisynth(NDVI ~ treatment, new_index, time, data, scm=T) # multiple treated points, so running multisynth 
summary(syn_NDVI)
plot(syn_NDVI)
syn_NDVI

# Run single outcome synth control for each outcome variable we want to model later
syn_yearly_loss <- multisynth(yearly_loss ~ treatment, new_index, time, data, scm=T) # multiple treated points, so running multisynth
summary(syn_yearly_loss)

# Run single outcome synth control for each outcome variable we want to model later
syn_B3 <- multisynth(B3 ~ treatment, new_index, time, data, scm=T) # multiple treated points, so running multisynth
summary(syn_B3)
# Run multiple outcomes synth control - not currently implemented for more than one outcome and more than one treated unit.
# Just to test the function, I will run it with one treated point and multiple outcomes.

# I will reload separate treated and donor pool data

# ----- loading and cleaning the data -----
folder = "data/treated_pointsV2"
files = list.files(
  path = folder,
  pattern = "Sampled_points_.*csv$",
  ignore.case = T,
  full.names = T
)
# data is a list of data frames, with each dataframe corresponding to a csv file (each year of data)
# Each df has 30 rows and 15 columns, for 30 random points and 15 attributes.
treatment_data = lapply(files, read.csv)
treatment_data <- lapply(seq_along(treatment_data), function(i) {
  df <- treatment_data[[i]]
  df$time <- 2000 + i
  return(df)
})
combined_treatment_data <- do.call(rbind, treatment_data)
combined_treatment_data <- combined_treatment_data[combined_treatment_data$system.index != "98_0", ]
# Order the dataframe by system.index and time
combined_treatment_data <- combined_treatment_data[order(combined_treatment_data$system.index, combined_treatment_data$time), ]
combined_treatment_data$treatment <- "treatment"
combined_treatment_data$treatment[combined_treatment_data$time < 2012] <- 0
combined_treatment_data$treatment[combined_treatment_data$time >= 2012] <- 1


folder2 = "data/donor_pointsV2"
files2 = list.files(
  path = folder2,
  pattern = "donor_points_.*csv$",
  ignore.case = T,
  full.names = T
)
donor_data = lapply(files2, read.csv)

# combining dataframes into one long-format dataframe
# Assume your list is called 'list_df'
# Add a time column to each dataframe in the list
donor_data <- lapply(seq_along(donor_data), function(i) {
  df <- donor_data[[i]]
  df$time <- 2000 + i
  return(df)
})

# Combine all dataframes into one
combined_donor_data <- do.call(rbind, donor_data)
# Order the dataframe by system.index and time
combined_donor_data <- combined_donor_data[order(combined_donor_data$system.index, combined_donor_data$time), ]
# Some points are missing information from one date.
# How to deal with these missing numbers?
# Drop the rows with combined_donor_data$system.index such that the number of dates is less than 6
# Get the indices of the rows to drop
rows_to_drop <- which(table(combined_donor_data$system.index) < 6)
combined_donor_data <- combined_donor_data[!combined_donor_data$system.index %in% names(rows_to_drop), ]
table(combined_donor_data$system.index)

# Add a column for treatment, called 'treatment'
combined_donor_data$treatment <- "treatment"
# All the points in the combined_donor_data are control points, so never treated
combined_donor_data$treatment <- 0

#View(combined_donor_data)
#View(combined_treatment_data) 

# Create new index for each system_index. Group the data by system.index and create a new index for each group
combined_treatment_data$new_index <- paste0(combined_treatment_data$system.index, "_treated")
combined_donor_data$new_index <- paste0(combined_donor_data$system.index, "_donor")

View(combined_treatment_data)
# Keep only one treatment point: system.index == 11_0 and system.index == 15_0
#combined_treatment_data <- combined_treatment_data[combined_treatment_data$system.index %in% c("11_0", "15_0"), ]
# keep only one treated unit with new_index == "11_0_treated"
#combined_treatment_data_1treated <- combined_treatment_data[combined_treatment_data$new_index == "11_0_treated", ]
combined_treatment_data_1treated <- combined_treatment_data %>%
  filter(new_index == "15_0_treated")
#View(combined_treatment_data_1treated)
# Combine the treatment and donor data

combined_data <- rbind(combined_treatment_data_1treated, combined_donor_data)

# Make treatment as.numeric
combined_data$treatment <- as.numeric(combined_data$treatment) 

combined_data$yearly_loss <- 0
# combined_data$yearly_loss = 1, if as.numeric(paste0("20",combined_data$lossyear)) <= combined_data$time, else 0
combined_data$yearly_loss[(combined_data$lossyear + 2000) <= combined_data$time] <- 1
View(combined_data)

# Drop combined_data rows 13 and 14, just trying to have one treatment date=1
#combined_data <- combined_data[-c(13, 14), ]

table(combined_data$system.index)
# Run single outcome synth control for each outcome variable we want to model later
syn_NDVI <- augsynth(NDVI ~ treatment, new_index, time, combined_data, progfunc = 'None', scm=T) # multiple treated points, so running multisynth

# Run single outcome synth control for each outcome variable we want to model later
syn_yearly_loss <- augsynth(yearly_loss ~ treatment, new_index, time, combined_data, progfunc = 'None', scm=T) # multiple treated points, so running multisynth

# Run multiple outcomes synth control
syn_multi <- augsynth(NDVI + yearly_loss + B3 ~ treatment, new_index, time, combined_data, progfunc = 'None', scm=T)
