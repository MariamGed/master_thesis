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

# Ensuring we use the same donors as in the scmo analysis
data <- data %>%
    filter(geometry_name != "donor35.0")

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
png("figures/synth_control_original/synth_gaps_datav4_v02.png", width = 800, height = 600)

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
  # data$treatment <- 0
  # data$treatment[data$geometry_name == treatment_point & data$year >= treatment_year] <- 1
  # data$treatment <- data$treatment %>% as.numeric()
  # data$geometry_name <- data$geometry_name %>% as.factor()
  # # Sort the data by geometry_name and year
  # data <- data[order(data$geometry_name, data$year), ]
  # data$geometry_name <- as.character(data$geometry_name)

  # data$unit.variable <- as.numeric(as.factor(data$geometry_name))
  # controls_identifier <- unique(data[data$geometry_name!=treatment_point,"unit.variable"]) 
  # treatment.identifier <- unique(data[data$geometry_name==treatment_point,"unit.variable"])


  # ---- Stable unit encoding FIRST, before any factor conversion ----
  data$geometry_name <- as.character(data$geometry_name)  # ensure clean character
  data <- data[order(data$geometry_name, data$year), ]    # sort before encoding
  
  # Encode based on sorted unique names — deterministic across all calls
  all_units <- sort(unique(data$geometry_name))
  data$unit.variable <- as.integer(factor(data$geometry_name, levels = all_units))
  
  # Treatment flag
  data$treatment <- as.numeric(
    data$geometry_name == treatment_point & data$year >= treatment_year
  )
  
  # Identifiers — derived from the stable encoding above
  controls_identifier  <- unique(data$unit.variable[data$geometry_name != treatment_point])
  treatment.identifier <- unique(data$unit.variable[data$geometry_name == treatment_point])
  
  # Sanity check — catches the bug explicitly if it ever recurs
  if (treatment.identifier %in% controls_identifier) {
    stop(paste("Treatment unit leaked into controls for:", treatment_point))
  }

  dataprep.out <- dataprep(foo = data, predictors = c("NDVI", "SR_B1", "SR_B2", "SR_B5"), dependent = "deforestation", unit.variable = "unit.variable", time.variable = "year",
                           treatment.identifier = treatment.identifier, controls.identifier = controls_identifier,
                           time.predictors.prior = c(2001:2011), 
                           time.optimize.ssr = c(2001:2011),
                           unit.names.variable = "geometry_name",
                           time.plot = 2001:2020)
  print(paste("dataprep pass", treatment_point))
  synth.out <- synth(dataprep.out)
  print(paste("synth pass", treatment_point))
  gaps<- dataprep.out$Y1plot-(dataprep.out$Y0plot%*%synth.out$solution.w)
  gaps <- as.data.frame(gaps)
  gaps$year <- rownames(gaps)
  gaps <- gaps %>%
    dplyr::rename(ATT_original_SC = as.character(treatment.identifier))
  rmse_pre <- sqrt(mean(gaps$ATT_original_SC[gaps$year < treatment_year]^2, na.rm = TRUE))
  rmse_post <- sqrt(mean(gaps$ATT_original_SC[gaps$year >= treatment_year]^2, na.rm = TRUE))
  # ratio_post_pre <- rmse_post / rmse_pre
  
  att <- gaps %>%
    filter(year >= treatment_year) %>%
    summarise(ATT = mean(ATT_original_SC, na.rm = TRUE)) %>%
    pull(ATT)
  
  return(list(treated_point = treatment_point, 
              ATT =as.numeric(att), 
              Pre.RMSE = rmse_pre, 
              Post.RMSE = rmse_post)) #full_outcome = outcome, , Pre.RMSE = rmse_pre, Post.RMSE = rmse_post, Ratio.Post.Pre = ratio_post_pre
}


try1 <- run_synth(data, "donor36.0", 2012) #run_synth <- function(data, treatment_point, treatment_year)


# apply run_synth to all donor areas
# Remove treated from the list of geometry_name
data_donors <- data %>%
  filter(geometry_name != "treated")

# sc_placebo_results <- lapply(unique(data_donors$geometry_name), function(x) run_synth(data_donors, x, 2012))
sc_placebo_results <- lapply(
  unique(data_donors$geometry_name),
  function(x) tryCatch(
    run_synth(data_donors, x, 2012),
    error = function(e) list(treated_point = x, error = conditionMessage(e))
  )
)

sc_placebo_results_archive <- sc_placebo_results
sc_placebo_results <- bind_rows(sc_placebo_results_archive)

sc_placebo_results <- sc_placebo_results %>%
  select(-error)

# # Make df long
# sc_placebo_results <- sc_placebo_results %>%
#   filter(rownames(sc_placebo_results) == "ATT") %>% # only select raw ATT
#   pivot_longer(cols = everything(), names_to = "geometry_name", values_to = "ATT") %>%
#   rename(ATT_SC = ATT)

# sc_placebo_results$ATT_SC <- as.numeric(sc_placebo_results$ATT_SC)


# sc_placebo_results_t <- sc_placebo_results %>%
#   # Remove the redundant treated_point row
#   filter(rownames(.) != "treated_point") %>%
#   # Convert rownames to a column
#   rownames_to_column(var = "metric") %>%
#   # Pivot so each donor becomes a row
#   pivot_longer(
#     cols = -metric,
#     names_to = "treated_point",
#     values_to = "value"
#   ) %>%
#   # Pivot wider to make metrics the columns
#   pivot_wider(
#     names_from = metric,
#     values_from = value
#   ) %>%
#   # Convert numeric columns
#   mutate(across(c(ATT, Pre.RMSE, Post.RMSE, Ratio.Post.Pre), as.numeric))

rmse_pre <- sqrt(mean(gaps$ATT_original_SC[gaps$year < 2012]^2, na.rm = TRUE))
rmse_post <- sqrt(mean(gaps$ATT_original_SC[gaps$year >= 2012]^2, na.rm = TRUE))
ratio_post_pre <- rmse_post / rmse_pre

treated_point_row <- data.frame(treated_point = "treated", ATT = att, Pre.RMSE = rmse_pre, Post.RMSE = rmse_post) #, Ratio.Post.Pre = ratio_post_pre
sc_placebo_results_t <- rbind(sc_placebo_results, treated_point_row)

# drop donor 36.0 from results due to issue with estimation
sc_placebo_results_t <- sc_placebo_results_t %>%
  filter(treated_point != "donor36.0")

sc_placebo_results_t <- sc_placebo_results_t %>%
  mutate(Ratio.Post.Pre = Post.RMSE / Pre.RMSE)

# sort in descending order, and add a column specifying order of each point in the dataframe
sc_placebo_results_t <- sc_placebo_results_t %>%
    arrange(desc(Ratio.Post.Pre)) %>%
    mutate(order = row_number()) %>%
    mutate(rank = order/n())

library(ggplot2)
library(dplyr)

sc_placebo_results_t <- read.csv("results/area_placebo_original_synth_datav4_v02.csv")

hist(sc_placebo_results_t$ATT, breaks = 20, main = "Distribution of ATT from Placebo Tests", xlab = "ATT (Synthetic Control)")
plot(sc_placebo_results_t$treated_point, sc_placebo_results_t$ATT, main = "Placebo Test Results", xlab = "Geometry Name", ylab = "ATT (Synthetic Control)", las = 2)
range(sc_placebo_results_t$ATT)
sd(sc_placebo_results_t$ATT, na.rm = TRUE)

library(forcats)
ggplot(sc_placebo_results_t, aes(x = fct_reorder(treated_point, Ratio.Post.Pre, .desc = TRUE), y = Ratio.Post.Pre)) + #
    geom_point(size = 3) +
    labs(title = "Post/Pre-treatment RMSE for Placebo Tests, Original Synthetic Control", x = "Placebo point", y = "Ratio of Post-treatment RMSE to Pre-treatment RMSE") +
    theme_minimal(base_size = 15) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    # when treated_point = treated, color the point red
    geom_point(data = sc_placebo_results_t %>% filter(treated_point == "treated"), aes(x = treated_point, y = Ratio.Post.Pre), color = "red", size = 4) +
    theme(legend.position = "none")

# save the plot
ggsave("figures/synth_control_original/placebo_test_post_pre_ratio_datav4_v01.png", width = 8, height = 8)

# save the placebo results
write.csv(sc_placebo_results_t, "results/area_placebo_original_synth_datav4_v02.csv", row.names = FALSE)


