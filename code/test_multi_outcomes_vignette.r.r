
# Testing the installation of the multiple_outcomes package from ebenmichael GitHub repository
library(magrittr)
library(dplyr)
library(augsynth)

data(kansas)
View(kansas)

# Run single outcome synth control for each outcome variable we want to model later
syn_lngdpcapita <- augsynth(lngdpcapita ~ treated, fips, year_qtr, kansas, progfunc="None", scm=T)
summary(syn_lngdpcapita)
plot(syn_lngdpcapita)

kansas$lntotalwagescapita <- log(kansas$totalwagescapita)
syn_lntotalwagescapita <- augsynth(lntotalwagescapita ~ treated, fips, year_qtr, kansas, progfunc="None", scm=T)

syn_estabscapita <- augsynth(estabscapita ~ treated, fips, year_qtr, kansas, progfunc="None", scm=T)

# Run multiple outcomes synth control
syn_multi <- augsynth(lngdpcapita + lntotalwagescapita + estabscapita ~ treated, fips, year_qtr, kansas, progfunc="None", scm=T)
summary(syn_multi)
plot(syn_multi)
