# Follow the gsynth tutorial to process the data
# Link to tutorial: https://yiqingxu.org/packages/gsynth/articles/tutorial.html

# Load libraries
library(gsynth) # For generalised synth control 
library(panelView) # For visualising panel data

# Load the data
setwd("/Users/mariamgedenidze/Desktop/YSE Thesis/master_thesis/")
data <- read.csv("data/combined_dataV0.2.csv")

# Visualise the data 
panelview(NDVI ~ treatment, data = data, index = c("new_index", "time"), pre.post = TRUE)
panelview(NDVI ~ treatment, data = data, index = c("new_index", "time"), type = "outcome")

# Run gsynth
system.time(
    out <- gsynth(yearly_loss ~ treatment + B1 + B2 + NDVI, data = data, 
                  index = c("new_index","time"), force = "two-way", 
                  CV = TRUE, r = c(0, 2), se = TRUE, 
                  inference = "parametric", nboots = 1000, 
                  parallel = TRUE)
)

print(out)
out$est.att
out$est.avg
out$est.beta

plot(out)
# plot the estimated counterfactual. raw = "none" (the default option) means that we do not include the raw data in this figure
plot(out, type = "counterfactual", raw = "none", main="")

# plot the estimated counterfactual with raw data
plot(out, type = "counterfactual", raw = "all")

# print loadings
plot(out, type = "loadings")
