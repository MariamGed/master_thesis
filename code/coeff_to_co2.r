# This notebook converts ATT values from synthetic control to avoided CO2 emission equivalents

coeff_to_co2 <- function(coeff, project_area_hectares = 30300) {
    area_saved <- project_area_hectares * coeff # ATT --> prevented deforestation Area in hectares 
    aboveground_biomass <- 463 * area_saved     # Hectares --> Mg of biomass on the saved area   
    carbon <- 0.47 * aboveground_biomass        # Mg of biomass --> Mg of carbon on the saved area
    co2_equivalent <- 3.67 * carbon             # Mg of carbon --> Mg of CO2 equivalent on the saved area
    return(c(area_saved = area_saved, co2_equivalent = co2_equivalent))
}

scmo <- coeff_to_co2(0.002372673, 28752)
scmo

sc <- coeff_to_co2(0.006163774)
sc

my_area_est <- 30300
real <- 28752

# Project's reported value: https://www.reddprojectsdatabase.org/271-maisa-redd-project/