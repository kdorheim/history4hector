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




# Other variables needed for the historical benchmarking
aerosol_vars <- c("RF_BC", "RF_OC", "RF_NH3", "RF_SO2", "RF_aci")
hist_warming <- c("global_tas")
other_vars <- c("RF_O3_trop", "RF_H2O_strat", "RF_vol", "RF_albedo", "RF_misc", "heatflux", "CO2_concentration", "RF_CH4", "RF_tot")

VARS <- c(aerosol_vars, hist_warming, other_vars)

fetchvars(hc, 1745:2023, vars = VARS) ->
    out

ofile <- "scripts/QAQC/new_hist.csv"
write.csv(file = ofile, x = out, row.names = FALSE )

