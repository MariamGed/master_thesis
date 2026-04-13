library(readr)

sc <- read_csv("results/area_placebo_original_synth_datav4_v02.csv")
scmo <- read_csv("results/area_placebo_scmo_concat_datav4_v02.csv")
deforestation <- read_csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_V4.csv")

sc <- sc %>%
  rename(geometry_name = treated_point) %>%
  rename(ATT_SC = ATT)

scmo <- scmo %>%
  rename(geometry_name = treated_point) %>%
  rename(ATT_SCMO = ATT)

deforestation_post_treatment <- deforestation %>%
   filter(year >= 2012) %>%
  group_by(geometry_name) %>%
  summarise(mean_deforestation_post = mean(deforestation, na.rm = TRUE))

deforestation_pre_treatment <- deforestation %>%
   filter(year < 2012) %>%
  group_by(geometry_name) %>%
  summarise(mean_deforestation_pre = mean(deforestation, na.rm = TRUE))

merged_data <- sc %>%
  select(geometry_name, ATT_SC) %>%
  left_join(scmo, by = "geometry_name") %>%
  left_join(deforestation_post_treatment, by = "geometry_name") %>%
  left_join(deforestation_pre_treatment, by = "geometry_name")

# View(head(merged_data))


# plot att_sc, att_scmo, and mean deforestation
ggplot(merged_data, aes(x = mean_deforestation_pre)) +
    geom_point(aes(y = ATT_SC, color = "SC"), size = 3) +
    geom_point(aes(y = ATT_SCMO, color = "SCMO"), size = 3) +
    labs(title = "ATT Estimates vs Mean Deforestation", x = "Mean Deforestation (2012-2020)", y = "ATT Estimate", color = "Method") +
    theme_minimal()


library(tidyr)
library(dplyr)

merged_data_long <- merged_data %>%
  mutate(id = geometry_name) %>%   # unique ID for clustering
  pivot_longer(
    cols = c(ATT_SC, ATT_SCMO),
    names_to = "model",
    values_to = "ATT"
  ) %>%
  mutate(
    model = ifelse(model == "ATT_SCMO", 1, 0),  # dummy for scmo =1, sc=0
    error = abs(ATT - 0)
  )

# plot the errors for both models
ggplot(merged_data_long, aes(x = mean_deforestation_pre, y = error, color = as.factor(model))) +
    geom_point(size = 3) +
    labs(title = "Absolute Error of ATT Estimates vs Mean Deforestation", x = "Mean Deforestation (2012-2020)", y = "Absolute Error", color = "Model") +
    theme_minimal() +
    scale_color_manual(values = c("blue", "red"), labels = c("SC", "SCMO")) +
    theme(legend.title = element_blank())

# Checking that the ATT errors are normal/estimates are normal. 
hist(merged_data_long$error, breaks = 20, main = "Distribution of ATT Estimates - SC", xlab = "ATT Estimate")
hist(merged_data$ATT_SCMO, breaks = 20, main = "Distribution of ATT Estimates - SCMO", xlab = "ATT Estimate")
hist(merged_data$ATT_SC, breaks = 20, main = "Distribution of ATT Estimates - SC", xlab = "ATT Estimate")
# plot density plots of ATT_SCMO and ATT_SC together
ggplot(merged_data, aes(x = ATT_SCMO, fill = "SCMO")) +
    geom_density(alpha = 0.5) +
    geom_density(aes(x = ATT_SC, fill = "SC"), alpha = 0.5) +
    labs(title = "Density of ATT Estimates", x = "ATT Estimate", fill = "Method") +
    theme_minimal(base_size = 15) +
    scale_fill_manual(values = c("blue", "red")) +
    theme(legend.title = element_blank())

# save the density plot
ggsave("figures/placebo_att_density_plot_datav4_v01.png", width = 8, height = 8)

# View(head(merged_data_long))
library(fixest)

# Post treatment regression
model1 <- feols(ATT ~ mean_deforestation_post * model, data = merged_data_long, cluster = ~ geometry_name)
summary(model1)

model2 <- feols(error ~ mean_deforestation_post * model, data = merged_data_long, cluster = ~ geometry_name)
summary(model2)

# Pre treatment regression
model3 <- feols(ATT ~ mean_deforestation_pre * model, data = merged_data_long , cluster = ~ geometry_name)
summary(model3)

model4 <- feols(error ~ mean_deforestation_pre * model, data = merged_data_long , cluster = ~ geometry_name)
summary(model4)
