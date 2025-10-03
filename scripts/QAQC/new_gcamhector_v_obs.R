# Comparison of GCAM Hector (new and old) with observations.

# 0. Set Up --------------------------------------------------------------------
source("scripts/constants.R")

# Loads the helper functions and the comparison data.
source("scripts/fxns_calibration.R")

library(ggplot2)
theme_set(theme_bw())

comparison_data %>%
    filter(year <= 2005) %>%
    filter(variable %in% c(CONCENTRATIONS_CO2(), "OHC", "gmst")) ->
    comp_data1

comparison_data %>%
    filter(variable %in% c(CONCENTRATIONS_CO2(), "OHC", "gmst")) ->
    comp_data2

# 1. New GCAM-hector -----------------------------------------------------------

ini <- here::here("inputs/hector-gcam.ini")
hc  <- newcore(ini, name = "new")
run(hc, runtodate = 2023)
new_out <- fetchvars_4comparison(hc = hc,
                                 comp = comp_data2)

nfile <- "scripts/QAQC/new_obs_comp.csv"
write.csv(file = nfile, x = new_out, row.names = FALSE )


