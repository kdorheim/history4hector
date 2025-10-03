# Run old hector with old GCAM inputs for comparison with observations.

# 0. Set Up --------------------------------------------------------------------
source("scripts/constants.R")

# Loads the helper functions and the comparison data.
source("scripts/fxns_calibration.R")

# we need to make sure that we are running the old version of
# hector with the older inputs
remotes::install_github("jgcri/hector@main")
library(hector)

library(ggplot2)
theme_set(theme_bw())

comparison_data %>%
    filter(year <= 2005) %>%
    filter(variable %in% c(CONCENTRATIONS_CO2(), "OHC", "gmst")) ->
    comp_data1

comparison_data %>%
    filter(variable %in% c(CONCENTRATIONS_CO2(), "OHC", "gmst")) ->
    comp_data2

# 1. Old GCAM-hector -----------------------------------------------------------
ini <- system.file(package = "hector", "input/hector_ssp245.ini")
hc  <- newcore(ini, name = "old")
run(hc, runtodate = 2023)
old_out <- fetchvars_4comparison(hc = hc,
                                 comp = comp_data1)

ofile <- "scripts/QAQC/old_obs_comp.csv"
write.csv(file = ofile, x = old_out, row.names = FALSE )



