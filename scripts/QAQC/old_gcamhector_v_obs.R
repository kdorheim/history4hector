# Run old hector with old GCAM inputs for comparison with observations.

# 0. Set Up --------------------------------------------------------------------
source("scripts/constants.R")

# Loads the helper functions and the comparison data.
source("scripts/fxns_calibration.R")

# we need to make sure that we are running the old version of
# hector with the older inputs
remotes::install_github("jgcri/hector@a61d64af3b9cc6d0d63cd661ee0164067a8f0165")
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


# Other variables needed for the historical benchmarking
aerosol_vars <- c("RF_BC", "RF_OC", "RF_NH3", "RF_SO2", "RF_aci")
hist_warming <- c("global_tas")
other_vars <- c("RF_O3_trop", "RF_H2O_strat", "RF_vol", "RF_albedo", "RF_misc", "heatflux", "CO2_concentration", "FCH4", "RF_tot")

VARS <- c(aerosol_vars, hist_warming, other_vars)

fetchvars(hc, 1745:2023, vars = VARS) %>%
    mutate(variable = if_else(variable == "FCH4", "RF_CH4", variable)) ->
    out

ofile <- "scripts/QAQC/old_hist.csv"
write.csv(file = ofile, x = out, row.names = FALSE)





