
'Methods to check the low-rank/shared factor structure assumption:'
'
Method 1: Singular value decomposition (SVD) check  

Examine whether a small number of singular vectors explain most of the total variation across your outcome series. 
If yes, the low-rank/shared factor structure holds. 
This is a matrix-level check, not a per-variable regression.

Method 2: Leave-one-outcome-out fit check

Hold out each outcome one at a time, estimate the combined synthetic control weights using only the remaining outcomes, 
and then inspect how well those weights fit the held-out outcome series in the pre-treatment period. 
The logic is: if there truly are shared common factors, weights trained on other outcomes should still track any individual
outcome reasonably well, even without seeing it.
'

# libraries
library(dplyr)

data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_V4.csv")
# pseudo code

# Suppose your data is in long format:
# columns: unit, time, outcome1, outcome2, outcome3
data <- data %>%
  group_by(geometry_name) %>%
  filter(!any(is.na(NDVI))) %>%
  ungroup() %>%
  as.data.frame()

# Create matrix of outcomes
Y <- data %>%
  arrange(geometry_name, year) %>%
  select(deforestation, NDVI, SR_B1, SR_B2, SR_B5) %>%
  as.matrix()

# SVD (Singular Value Decomposition)
svd_res <- svd(Y)

# Variance explained
singular_values <- svd_res$d
var_explained <- singular_values^2 / sum(singular_values^2)

# Cumulative variance
cum_var <- cumsum(var_explained)

cum_var
