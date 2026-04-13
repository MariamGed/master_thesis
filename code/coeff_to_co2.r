# This notebook converts ATT values from synthetic control to avoided CO2 emission equivalents

coeff_to_co2 <- function(coeff, project_area_hectares = 30300) {
    area_saved <- project_area_hectares * coeff # ATT --> prevented deforestation Area in hectares 
    aboveground_biomass <- 463 * area_saved     # Hectares --> Mg of biomass on the saved area   
    carbon <- 0.47 * aboveground_biomass        # Mg of biomass --> Mg of carbon on the saved area
    co2_equivalent <- 3.67 * carbon             # Mg of carbon --> Mg of CO2 equivalent on the saved area
    return(c(area_saved = area_saved, co2_equivalent = co2_equivalent))
}

scmo <- coeff_to_co2(0.002019464,28752 ) #30377.8062
scmo

sc <- coeff_to_co2(0.006163774)
sc

my_area_est <- 30300
real <- 28752

# Project's reported value: https://www.reddprojectsdatabase.org/271-maisa-redd-project/

# calculate deforestation areas prior to 2012
library(dplyr)
data <- read.csv("data/Maisa_single_treated_annual/Maisa_2001_2020_annual_V4.csv")

data_treated <- data %>%
  filter(geometry_name == "treated")

data_treated <- data_treated %>%
  dplyr::select(year, deforestation) %>% 
  mutate(deforestation_ha = deforestation * 0.09 * 28752) # convert deforestation from pixels to hectares

total_def_before2012 <- sum(data_treated %>% filter(year > 2001 & year < 2012) %>% pull(deforestation_ha))

# def = #deforested pixels/#pixels per area.
# Sum = sum total # deforested pixels/#pixels per area

# Area per pixel (30m x 30m) = 900 m² = 0.09 hectares
# Total area of the project = 30300 hectares (30377.8062 ha)
# total_def_before2012_ha <- total_def_before2012 * 0.09 * 28752


