# Testing synth package for original synthetic control
library(dplyr)
library(augsynth)
library(panelView)
library(Synth)
library(tidyr)
library(pensynth)

# setwd("/Users/mariamgedenidze/Desktop/YSE Thesis/master_thesis/")
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_V4.csv")

data <- data %>%
  group_by(geometry_name) %>%
  filter(!any(is.na(NDVI))) %>%
  ungroup() %>%
  as.data.frame()

# data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")
# ---- my data ----
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

# add a unique identifier to each donor and treatment
data$unit.variable <- as.numeric(as.factor(data$geometry_name))
treated_point <- data[data$geometry_name == "treated", "unit.variable"] %>% unique()

controls_identifier <- unique(data[data$geometry_name!="treated","unit.variable"])
data$geometry_name <- as.character(data$geometry_name)

dataprep.out <- dataprep(foo = data, 
        predictors = c("NDVI", "SR_B1", "SR_B2", "SR_B5"), 
        dependent = "deforestation", 
        unit.variable = "unit.variable", 
        time.variable = "year",
        treatment.identifier = treated_point, 
        controls.identifier = controls_identifier,
        time.predictors.prior = c(2001:2011), 
        time.optimize.ssr = c(2001:2011),
        unit.names.variable = "geometry_name",
        time.plot = 2001:2020)

synth.out <- synth(dataprep.out)

weights <- synth.out$solution.w

## the period by period 
## discrepancies between the 
## treated unit and its synthetic control unit
## can be computed by typing
gaps<- dataprep.out$Y1plot-(
  dataprep.out$Y0plot%*%synth.out$solution.w
)
gaps

path.plot(dataprep.res = dataprep.out,synth.res = synth.out)

#save the plot
png( "figures/synth_control_original/synth_gaps_datav4_v01.png", width = 800, height = 600)
path.plot(dataprep.res = dataprep.out,synth.res = synth.out,  
          Main = "Original Synthetic Control", Xlab = "Year", Ylab = "Deforestation Proportion")

out <- synth.tab(synth.res = synth.out, 
          dataprep.res = dataprep.out)


# how do i get ATT?
# Calculate average ATT over the post-treatment period (2012-2020)
# assign columname years to the rownames
gaps <- as.data.frame(gaps)
# add rownames as a column
gaps$year <- rownames(gaps)
gaps <- gaps %>%
  dplyr::rename(ATT_original_SC = as.character(treated_point))
View(gaps)

att <- gaps %>%
 filter(year >= 2012) %>%
 summarise(ATT = mean(ATT_original_SC, na.rm = TRUE)) %>%
 pull(ATT)


# # Using pensynth package
# result <- pensynth(X1 = dataprep.out$X1,
#          X0 = dataprep.out$X0,
#          lambda = 0)

# result_placebo <- placebo_test(result, dataprep.out$Y1, dataprep.out$Y0)

# dataprep.out
# ----- funtionalise ----
# Write a function to complete the placebo test on all donors

# dataprep to do:
# add a unique identifier to each donor and treatment 
# data$unit.variable <- # add here

  # Drop column .geo and .index
data$.geo <- NULL
data$system.index <- NULL


run_synth <- function(data, treatment_point, treatment_year){
  # Data prep
  data$treatment <- 0
  data$treatment[data$geometry_name == treatment_point & data$year >= treatment_year] <- 1
  data$treatment <- data$treatment %>% as.numeric()
  data$geometry_name <- data$geometry_name %>% as.factor()
  # Sort the data by geometry_name and year
  data <- data[order(data$geometry_name, data$year), ]
  data$geometry_name <- as.character(data$geometry_name)

  data$unit.variable <- as.numeric(as.factor(data$geometry_name))
  controls_identifier <- unique(data[data$geometry_name!=treatment_point,"unit.variable"]) 
  treatment.identifier <- unique(data[data$geometry_name==treatment_point,"unit.variable"])

  dataprep.out <- dataprep(foo = data, predictors = c("NDVI", "SR_B1", "SR_B2", "SR_B5"), dependent = "deforestation", unit.variable = "unit.variable", time.variable = "year",
                           treatment.identifier = treatment.identifier, controls.identifier = controls_identifier,
                           time.predictors.prior = c(2001:2011), 
                           time.optimize.ssr = c(2001:2011),
                           unit.names.variable = "geometry_name",
                           time.plot = 2001:2020)
  
  synth.out <- synth(dataprep.out)
  gaps<- dataprep.out$Y1plot-(dataprep.out$Y0plot%*%synth.out$solution.w)
  gaps <- as.data.frame(gaps)
  gaps$year <- rownames(gaps)
  gaps <- gaps %>%
    dplyr::rename(ATT_original_SC = as.character(treatment.identifier))

  att <- gaps %>%
    filter(year >= treatment_year) %>%
    summarise(ATT = mean(ATT_original_SC, na.rm = TRUE)) %>%
    pull(ATT)
  
  return(list(treated_point = treatment_point, ATT =as.numeric(att))) #full_outcome = outcome, 
}


try1 <- run_synth(data, "treated", 2012) #run_synth <- function(data, treatment_point, treatment_year)


# apply run_synth to all donor areas
# Remove treated from the list of geometry_name
data_donors <- data %>%
  filter(geometry_name != "treated")

sc_placebo_results <- sapply(unique(data_donors$geometry_name), function(x) run_synth(data_donors, x, 2012))
sc_placebo_results <- as.data.frame(sc_placebo_results)

# Make df long
sc_placebo_results <- sc_placebo_results %>%
  filter(rownames(sc_placebo_results) == "ATT") %>% # only select raw ATT
  pivot_longer(cols = everything(), names_to = "geometry_name", values_to = "ATT") %>%
  rename(ATT_SC = ATT)

sc_placebo_results$ATT_SC <- as.numeric(sc_placebo_results$ATT_SC)
View(sc_placebo_results)

hist(sc_placebo_results$ATT_SC, breaks = 20, main = "Distribution of ATT from Placebo Tests", xlab = "ATT (Synthetic Control)")
plot(sc_placebo_results$geometry_name, sc_placebo_results$ATT_SC, main = "Placebo Test Results", xlab = "Geometry Name", ylab = "ATT (Synthetic Control)", las = 2)
range(sc_placebo_results$ATT_SC)
sd(sc_placebo_results$ATT_SC, na.rm = TRUE)

# save the placebo results
write.csv(sc_placebo_results, "results/area_placebo_original_synth_datav4_v01.csv", row.names = FALSE)
