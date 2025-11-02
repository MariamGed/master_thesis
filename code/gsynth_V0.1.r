# Follow the gsynth tutorial to process the data
# Link to tutorial: https://yiqingxu.org/packages/gsynth/articles/tutorial.html
# link to package: https://cran.r-project.org/web/packages/gsynth/gsynth.pdf#page=3.00 

# Load libraries
library(gsynth) # For generalised synth control 
library(panelView) # For visualising panel data

# Load the data
setwd("/Users/mariamgedenidze/Desktop/YSE Thesis/master_thesis/")
#data <- read.csv("data/combined_dataV0.2.csv")
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")

# ----- Preprocessing the data, only for the annual_full -----
data$treatment <- 0

# If geometry_name=='treated', then treatment=1, else treatment=0
data$treatment[data$geometry_name == "treated" & data$year >= 2012] <- 1
data$treatment <- data$treatment %>% as.numeric()
data$geometry_name <- data$geometry_name %>% as.factor()


# Visualise the data 
#panelview(NDVI ~ treatment, data = data, index = c("new_index", "time"), pre.post = TRUE)
#panelview(NDVI ~ treatment, data = data, index = c("new_index", "time"), type = "outcome")

panelview(deforestation ~ treatment, data = data, index = c("geometry_name", "year"), pre.post = TRUE)

colnames(data)
# Run gsynth
system.time(
    out <- gsynth(deforestation ~ treatment, data = data, # + 'SR_B1' + 'SR_B2' + 'SR_B3'
                  index = c("geometry_name","year"), force = "two-way", 
                  CV = TRUE, r = c(0, 2), se = TRUE, 
                  inference = "parametric", nboots = 1000, 
                  parallel = TRUE)
)

print(out)
out$est.att # same as print(out)
out$est.avg
out$est.beta

plot(out)
# plot the estimated counterfactual. raw = "none" (the default option) means that we do not include the raw data in this figure
plot(out, type = "counterfactual", raw = "none", main="")

# plot the estimated counterfactual with raw data
plot(out, type = "counterfactual", raw = "all")

# print loadings
#plot(out, type = "loadings")
