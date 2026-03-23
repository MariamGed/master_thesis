#  ---- Test 2: Treatment time placebo  -----
# Change treatment time from 2012 to 2010
# Result: see which model can better reproduce 0 ATT in year 2010 and 2011.


# Libraries
library(readr)
library(magrittr)
library(dplyr)
library(panelView)
library(augsynth) # multiple outcome synth 
library(fect) # single outcome gsynth
library(ggplot2)
library(tidyr)

# load data
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_full_V2.csv")
#View(head(data,30))

run_synth_methods <- function(data, treatment_point, treatment_year, model = c("SC", "SCMO")){
    # Data prep
    data$treatment <- 0
    data$treatment[data$geometry_name == treatment_point & data$year >= treatment_year] <- 1
    data$treatment <- data$treatment %>% as.numeric()
    data$geometry_name <- data$geometry_name %>% as.factor()
    # Sort the data by geometry_name and year
    data <- data[order(data$geometry_name, data$year), ]
    
    # Drop column .geo and .index
    data$.geo <- NULL
    data$system.index <- NULL
    
    if (model == "SC") {
      outcome <- augsynth(deforestation ~ treatment| NDVI + SR_B1 + SR_B2 + SR_B5, geometry_name, year, data, progfunc = 'Ridge', scm=T)
    }  
    else if(model == "SCMO"){
        # clean data
        data <- data %>% 
            filter(year != 2013) %>% 
            filter(!is.na(NDVI))
    
        outcome <- augsynth(deforestation + NDVI + SR_B1 + SR_B2 + SR_B5 ~ treatment, geometry_name, year, data, t_int=treatment_year, progfunc = 'Ridge', scm=T)
    }
    return(outcome)
}

scmo <- run_synth_methods(data, treatment_point = "treated", treatment_year = 2010, model = "SCMO")
c <- summary(scmo, grid_size = 2) #
att_scmo <- c[["att"]]
scmo_bias_1 <- abs(att_scmo[att_scmo$Time == 2010 & att_scmo$Outcome == "deforestation", "Estimate"])
scmo_bias_2 <- abs(att_scmo[att_scmo$Time == 2011 & att_scmo$Outcome == "deforestation", "Estimate"])


sc <- run_synth_methods(data, treatment_point = "treated", treatment_year = 2010, model = "SC")
d <- summary(sc)
att_sc <- d[["att"]]
sc_bias_1 <- abs(att_sc[att_sc$Time == 2010, "Estimate"])
sc_bias_2 <- abs(att_sc[att_sc$Time == 2011, "Estimate"])

print(paste("SC bias in 2010:", sc_bias_1, "SCMO bias in 2010:", scmo_bias_1))
print(paste("SC bias in 2011:", sc_bias_2, "SCMO bias in 2011:", scmo_bias_2))

# save results
write.csv(att_sc, "results/time_placebo_SC.v01.csv") # synthetic control
write.csv(att_scmo, "results/time_placebo_SCMO.v01.csv") # synthetic control with multiple outcomes

'
Discussion:

Overall, the synthetic control method shows smaller bias than the synthetic control method with multiple outcomes in both 2010 and 2011. 
This suggests that the synthetic control method may be better at reproducing the counterfactual outcomes in the absence of treatment, while the synthetic control method with multiple outcomes may introduce more bias due to the inclusion of additional covariates. 
However, it is important to note that these results are specific to this particular dataset and treatment point, and may not generalize to other contexts.

Results:
[1] "SC bias in 2010: 0.001285, SCMO bias in 2010: 0.00840"
[1] "SC bias in 2011: 0.002118, SCMO bias in 2011: 0.00266"

'