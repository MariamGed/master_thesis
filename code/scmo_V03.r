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
# data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_V6.csv")
# data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_V7.csv")


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

# keep years only from 2002
# data <- data %>%
#   filter(year >= 2002)

# remove geometry with name donor148.0 -> too close to maisa
# data <- data %>%
#   filter(geometry_name != "donor148.0")

run_synth <- function(data, treatment_point, treatment_year){
    # Data prep
    data$treatment <- 0
    data$treatment[data$geometry_name == treatment_point & data$year >= treatment_year] <- 1
    data$treatment <- as.numeric(data$treatment)
    data$geometry_name <- as.factor(data$geometry_name)
    # Sort the data by geometry_name and year
    data <- data[order(data$geometry_name, data$year), ]

    outcome <- augsynth(deforestation + NDVI + SR_B1 + SR_B2 + SR_B5 ~ treatment, geometry_name, year, data, t_int=treatment_year, progfunc = 'Ridge', combine_method = "concat", scm=T)
    
    b <- summary(outcome)
    bdf <- as.data.frame(b$average_att)
    att <- bdf %>%
        filter(Outcome == "deforestation") %>%
        select(Estimate)

    att_est <- b$att$Estimate
        ## get pre-treatment fit by outcome
    imbal_pre <- b$att %>% 
        filter(Time < b$t_int) %>%
        group_by(Outcome) %>%
        summarise(Pre.RMSE = sqrt(mean(Estimate ^ 2, na.rm = TRUE)))
    pre.rmse <- imbal_pre$Pre.RMSE[imbal_pre$Outcome == "deforestation"]

    # post-treatment fit by outcome
    imbal_post <- b$att %>% 
        filter(Time >= b$t_int) %>%
        group_by(Outcome) %>%
        summarise(Post.RMSE = sqrt(mean(Estimate ^ 2, na.rm = TRUE)))
    post.rmse <- imbal_post$Post.RMSE[imbal_post$Outcome == "deforestation"]
    ratio_post_pre <- post.rmse / pre.rmse

    return(list(treated_point = treatment_point, ATT =as.numeric(att), Pre.RMSE = pre.rmse, Post.RMSE = post.rmse, Ratio.Post.Pre = ratio_post_pre))
}

try1 <- run_synth(data, "treated", 2012) 
scmo_outcome <- try1[["full_outcome"]]
plot(scmo_outcome, grid_size = 2)

# make a gaps plot: treated vs synthetic control
weights <- scmo_outcome[["weights"]]
# make weights into a df
weights_df <- as.data.frame(weights)
weights_df$geometry_name <- rownames(weights_df)
weights_df <- weights_df %>%
  rename(weight = V1)

# sort weights in decreasing order
View(weights_df %>%
    arrange(desc(weight)))

# ------ Temp code -> without function ----
# Data prep
data$treatment <- 0
data$treatment[data$geometry_name == 'treated' & data$year >= 2012] <- 1
data$treatment <- as.numeric(data$treatment)
data$geometry_name <- as.factor(data$geometry_name)
# Sort the data by geometry_name and year
data <- data[order(data$geometry_name, data$year), ]

# drop geometry_name 35 --> Drawn from the protected area & shouldn't be used
# For data version 4
data <- data %>%
    filter(geometry_name != "donor35.0")

outcome <- augsynth(deforestation + NDVI + SR_B1 + SR_B2 + SR_B5 ~ treatment, 
                    geometry_name, 
                    year, 
                    data = data, 
                    progfunc = 'Ridge', 
                    combine_method = "concat",
                    scm=T)

weights <- outcome[["weights"]]
# make weights into a df
weights_df <- as.data.frame(weights)
weights_df$geometry_name <- rownames(weights_df)
weights_df <- weights_df %>%
  rename(weight = V1)
# sort weights in decreasing order
View(weights_df %>%
    arrange(desc(weight)))

summary(outcome, grid_size = 2)
plot(outcome, grid_size = 2)

# save the outcome plot
ggsave("figures/scmo_all_outcome_ATTs_dataV4_v01.png", width = 8, height = 8)

a <- summary(outcome)
att_est <- a$att$Estimate
## get pre-treatment fit by outcome
imbal_pre <- a$att %>% 
    filter(Time < a$t_int) %>%
    group_by(Outcome) %>%
    summarise(Pre.RMSE = sqrt(mean(Estimate ^ 2, na.rm = TRUE)))

# post-treatment fit by outcome
imbal_post <- a$att %>% 
    filter(Time >= a$t_int) %>%
    group_by(Outcome) %>%
    summarise(Post.RMSE = sqrt(mean(Estimate ^ 2, na.rm = TRUE)))

pre.rmse <- imbal_pre$Pre.RMSE[imbal_pre$Outcome == "deforestation"]
post.rmse <- imbal_post$Post.RMSE[imbal_post$Outcome == "deforestation"]
ratio_post_pre <- post.rmse / pre.rmse

# plot a histogram of weights
ggplot(weights_df, aes(x = weight)) +
    geom_histogram(binwidth = 0.01, fill = "grey", color = "black") +
    labs(title = "Distribution of donor weights", x = "Weight", y = "Frequency") +
    theme_minimal(base_size = 20)
# save the weights histogram
ggsave("figures/scmo_concat_weights_histogram_dataV4_v01.png", width = 8, height = 8)

# save the weights df
write.csv(weights_df, "results/scmo_concat_weights_datav04_v02.csv", row.names = FALSE)
# --- End of Temp code ----

# add weights to data
data <- data %>%
    left_join(weights_df, by = "geometry_name")

# Calculate synthetic deforestation
data_with_synthetic <- data %>%
    group_by(year) %>%
    mutate(synthetic_deforestation = sum(deforestation * weight, na.rm = TRUE)) %>%
    mutate(synthetic_NDVI = sum(NDVI * weight, na.rm = TRUE)) %>%
    mutate(synthetic_SR_B1 = sum(SR_B1 * weight, na.rm = TRUE)) %>%
    mutate(synthetic_SR_B2 = sum(SR_B2 * weight, na.rm = TRUE)) %>%
    mutate(synthetic_SR_B5 = sum(SR_B5 * weight, na.rm = TRUE)) %>%
    ungroup() 

# plot treated vs synthetic deforestation
treated_deforestation <- data_with_synthetic %>%
    filter(geometry_name == "treated") %>%
    select(year, deforestation, NDVI, SR_B1, SR_B2, SR_B5) 

synth_deforestation <- data_with_synthetic %>%
    group_by(year) %>%
    summarise(synthetic_deforestation = unique(synthetic_deforestation),
              synthetic_NDVI = unique(synthetic_NDVI),
              synthetic_SR_B1 = unique(synthetic_SR_B1),
              synthetic_SR_B2 = unique(synthetic_SR_B2),
              synthetic_SR_B5 = unique(synthetic_SR_B5))

deforestation_comparison <- treated_deforestation %>%
    left_join(synth_deforestation, by = "year")

# rename columns to be more descriptive
deforestation_comparison <- deforestation_comparison %>%
    rename(Deforestation = deforestation,
              NDVI = NDVI,
              Blue = SR_B1,
              Green = SR_B2,
              SWIR1 = SR_B5,
              Synthetic_Deforestation = synthetic_deforestation,
              Synthetic_NDVI = synthetic_NDVI,
              Synthetic_Blue = synthetic_SR_B1,
              Synthetic_Green = synthetic_SR_B2,
              Synthetic_SWIR1 = synthetic_SR_B5)


library(tidyverse)

# Reshape data
df_long <- deforestation_comparison %>%
  pivot_longer(
    cols = -year,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    type = ifelse(grepl("Synthetic", variable), "Synthetic Control", "Treated"),
    variable = gsub("Synthetic_", "", variable)
  )
ggplot(df_long, aes(x = year, y = value, color = type, linetype = type)) +
  geom_line() +
  geom_vline(xintercept = 2012, linetype = "dotted", color = "red", linewidth = 1) +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Treated vs Synthetic Control Across Variables",
    y = ""
  ) +
  theme_minimal(base_size = 15) +
  scale_linetype_manual(values = c(
    "Treated" = "solid",
    "Synthetic Control" = "dashed"
  )) +
  scale_color_manual(values = c(
  "Treated" = "black",
  "Synthetic Control" = "blue"
  )) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )


# save the treated vs synthetic control plot
ggsave("figures/scmo_concat_treated_vs_synthetic_comparison_dataV4_v01.png", width = 8, height = 8)

# save deforestation_comparison
# write.csv(deforestation_comparison, "results/scmo_treated_deforestation_gaps.csv", row.names = FALSE)


# apply run_synth to all donor areas
# Remove treated from the list of geometry_name
data_donors <- data %>%
  filter(geometry_name != "treated")


sc_placebo_results <- sapply(unique(data_donors$geometry_name), function(x) run_synth(data_donors, x, 2012))
sc_placebo_results_archive <- sc_placebo_results
sc_placebo_results <- as.data.frame(sc_placebo_results)
View(sc_placebo_results)

# drop the row full outcome from the placebo results
# sc_placebo_results <- sc_placebo_results[-which(rownames(sc_placebo_results) == "full_outcome"), ]


# Make df long
# sc_placebo_results <- sc_placebo_results %>%
#   filter(rownames(sc_placebo_results) == "ATT") %>% # only select raw ATT
#   pivot_longer(cols = everything(), names_to = "geometry_name", values_to = "ATT") %>%
#   rename(ATT_SCMO = ATT)

library(tidyverse)

sc_placebo_results_t <- sc_placebo_results %>%
  # Remove the redundant treated_point row
  filter(rownames(.) != "treated_point") %>%
  # Convert rownames to a column
  rownames_to_column(var = "metric") %>%
  # Pivot so each donor becomes a row
  pivot_longer(
    cols = -metric,
    names_to = "treated_point",
    values_to = "value"
  ) %>%
  # Pivot wider to make metrics the columns
  pivot_wider(
    names_from = metric,
    values_from = value
  ) %>%
  # Convert numeric columns
  mutate(across(c(ATT, Pre.RMSE, Post.RMSE, Ratio.Post.Pre), as.numeric))

# sc_placebo_results$ATT_SCMO <- as.numeric(sc_placebo_results$ATT_SCMO)

# Save the placebo results
write.csv(sc_placebo_results_t, "results/area_placebo_scmo_concat_datav4_v02.csv", row.names = FALSE)


# sort in descending order of the ratio.post.pre
sc_placebo_results_t <- sc_placebo_results_t %>%
    arrange(desc(Ratio.Post.Pre))

# add treated point to the placebo results
treated_point_row <- data.frame(treated_point = "treated", ATT = try1[["ATT"]], Pre.RMSE = try1[["Pre.RMSE"]], Post.RMSE = try1[["Post.RMSE"]], Ratio.Post.Pre = try1[["Ratio.Post.Pre"]])
sc_placebo_results_t <- rbind(sc_placebo_results_t, treated_point_row)

# sort in descending order, and add a column specifying order of each point in the dataframe
sc_placebo_results_t <- sc_placebo_results_t %>%
    arrange(desc(Ratio.Post.Pre)) %>%
    mutate(order = row_number()) %>%
    mutate(rank = order/n())


# give scatter plot of the ratios
ggplot(sc_placebo_results_t, aes(x = fct_reorder(treated_point, Ratio.Post.Pre, .desc = TRUE), y = Ratio.Post.Pre)) +
    geom_point(size = 3) +
    labs(title = "Ratio of Post-treatment RMSE to Pre-treatment RMSE for Placebo Tests", x = "Placebo point", y = "Ratio of Post-treatment RMSE to Pre-treatment RMSE") +
    theme_minimal(base_size = 15) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    # when treated_point = treated, color the point red
    geom_point(data = sc_placebo_results_t %>% filter(treated_point == "treated"), aes(x = treated_point, y = Ratio.Post.Pre), color = "red", size = 4) +
    theme(legend.position = "none")

# save the plot of the ratios
ggsave("figures/scmo_concat_placebo_ratios_plot_dataV4_v01.png", width = 8, height = 8)

View(head(sc_placebo_results_t,15)) # the treated point is ranked 15th out of 117 donors
