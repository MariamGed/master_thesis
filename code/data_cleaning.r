# Data preparation code from GEE csv files to gsynth required dataframe
# Set working directory
setwd("/Users/mariamgedenidze/Desktop/YSE Thesis/master_thesis/")

# Load libraries
library(dplyr)
library(tidyr)
library=(ggplot2)

# Loading the data
treated_points2001 <- read.csv('data/treated_pointsV2/Sampled_points_2001.csv')
# Read all the csv files in the treated_points2 folder. 
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

# Left_join each treatment_points df with each other
#treatment_data = Reduce(function(x, y) left_join(x, y, by = "system.index"), treatment_data)
# Check if there are null values
sum(is.na(treatment_data))

# combining dataframes into one long-format dataframe
# Assume your list is called 'list_df'
# Add a time column to each dataframe in the list
treatment_data <- lapply(seq_along(treatment_data), function(i) {
  df <- treatment_data[[i]]
  df$time <- 2000 + i
  return(df)
})

# Combine all dataframes into one
combined_treatment_data <- do.call(rbind, treatment_data)
# Drop the rows with system.index 98_0, it has only 2 dates information
combined_treatment_data <- combined_treatment_data[combined_treatment_data$system.index != "98_0", ]
# Order the dataframe by system.index and time
combined_treatment_data <- combined_treatment_data[order(combined_treatment_data$system.index, combined_treatment_data$time), ]

sum(is.na(combined_treatment_data))

# Check the number of dates for each index
table(combined_treatment_data$system.index)
# Some points are missing information from one date.
# How to deal with these missing numbers?

# Add a column for treatment, called 'treatment'
combined_treatment_data$treatment <- "treatment"
# All the points in the combined_treatment_data are treated starting 2012.
# Fill in the 'treatment' column with 0 for the years before 2012, and 1 for the years after 2012
combined_treatment_data$treatment[combined_treatment_data$time < 2012] <- 0
combined_treatment_data$treatment[combined_treatment_data$time >= 2012] <- 1

# ----- Repeat the same for the donor data -----
# Read in the donor data

folder2 = "data/donor_pointsV2"
files2 = list.files(
  path = folder2,
  pattern = "donor_points_.*csv$",
  ignore.case = T,
  full.names = T
)
donor_data = lapply(files2, read.csv)
sum(is.na(donor_data))

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

sum(is.na(combined_treatment_data))

# Check the number of dates for each index
View(table(combined_donor_data$system.index))
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

View(combined_donor_data)
View(combined_treatment_data) 

# Create new index for each system_index. Group the data by system.index and create a new index for each group
combined_treatment_data$new_index <- paste0(combined_treatment_data$system.index, "_treated")
combined_donor_data$new_index <- paste0(combined_donor_data$system.index, "_donor")

# These two dfs have the same columns, so we can combine them
combined_data <- rbind(combined_treatment_data, combined_donor_data)

# save the combined data
write.csv(combined_data, "data/combined_dataV0.1.csv", row.names = FALSE)

# --- End of data merging ---

# --- Start of data cleaning ---

# The Hansen's deforestation data variables in the combined _data are the following:
# treecover2000, gain, loss, lossyear.

# Variable descriptions:
# treecover2000: Tree canopy cover for year 2000, defined as canopy closure for all vegetation taller than 5m in height.
# loss: Forest loss during the study period, defined as a stand-replacement disturbance (a change from a forest to non-forest state), 0: No loss, 1: Loss
# gain: Forest gain during the period 2000-2012, defined as the inverse of loss (a non-forest to forest change entirely within the study period). Note that this has not been updated in subsequent versions. 0: No gain, 1: Gain
# lossyear: Year of gross forest cover loss event. Forest loss during the study period, defined as a stand-replacement disturbance, or a change from a forest to non-forest state. 
# Encoded as either 0 (no loss) or else a value in the range 1-23, representing loss detected primarily in the year 2001-2023, respectively.

# I will add a column yearly_loss, which is binary, indicating if there was a loss in that year or before

combined_data$yearly_loss <- 0

# combined_data$yearly_loss = 1, if as.numeric(paste0("20",combined_data$lossyear)) <= combined_data$time, else 0
combined_data$yearly_loss[(combined_data$lossyear + 2000) <= combined_data$time] <- 1
#View(combined_data)

# Save the data
write.csv(combined_data, "data/combined_dataV0.2.csv", row.names = FALSE)

# --- End of data cleaning ---