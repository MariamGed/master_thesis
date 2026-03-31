

library(dplyr)

# pseudo code

# Suppose your data is in long format:
# columns: unit, time, outcome1, outcome2, outcome3

# Create matrix of outcomes
Y <- df %>%
  arrange(unit, time) %>%
  select(outcome1, outcome2, outcome3) %>%
  as.matrix()

# Optional but recommended: center/scale
Y_scaled <- scale(Y)

# SVD (Singular Value Decomposition)
svd_res <- svd(Y_scaled)

# Variance explained
singular_values <- svd_res$d
var_explained <- singular_values^2 / sum(singular_values^2)

# Cumulative variance
cum_var <- cumsum(var_explained)

cum_var