# Generate the hector run results that are going to be used in benchmarking
# from the "dev" calibrated hector.
# TODO might want to make a better way to denote which hector parameters are
# being used.

# 0. Set Up --------------------------------------------------------------------

source("scripts/constants.R")
source("scripts/fxns_calibration.R")
source("scripts/QAQC/fxns.R")

WRITE_TO <- here::here("scripts/QAQC/data")
dir.create(WRITE_TO, showWarnings = FALSE, recursive = TRUE)

# 1. Observations Comparison ---------------------------------------------------

# TODO this would be a good place to set up the different parameter values
# if there are different versions we would like to test...
ini <- here::here("inputs/hector-gcam.ini")
hc  <- newcore(ini, name = "dev")
run(hc, runtodate = 2023)
fetchvars_4comparison(hc = hc,
                      comp = comparison_data) %>%
    mutate(source = "hectorV3.5.X") ->
    new_hector_out

write.csv(x = new_hector_out, file = file.path(WRITE_TO, "gcamhectorDEV_obs_comparison.csv"), row.names = FALSE)



# 2.AR6 Benchmarks -------------------------------------------------------------

VARS <- c("global_tas", "heatflux", "RF_tot", "RF_BC", "RF_OC", "RF_NH3", "RF_SO2", "RF_aci", "RF_O3_trop",
          "RF_H2O_strat", "RF_vol", "RF_albedo", "RF_misc", "RF_CH4")

fetchvars(hc, 1746:2023, vars = VARS) %>%
    AR6B.get_historical_fxn %>%
    mutate(source = "hectorV3.5.X") ->
    out

write.csv(x = out, file = file.path(WRITE_TO, "gcamhectorDEV_AR6benchmarks.csv"), row.names = FALSE)



# 3. Inputs --------------------------------------------------------------------

here::here("inputs/gcam_emissions.csv") %>%
    read.csv(comment.char = ";") %>%
    pivot_longer(-Date, names_to = "variable") %>%
    rename(year = Date) %>%
    mutate(source = "dev",
           file = "gcam_inputs") ->
    gcam_inputs

here::here("inputs/default_inputs.csv") %>%
    read.csv(comment.char = ";") %>%
    pivot_longer(-Date, names_to = "variable") %>%
    rename(year = Date) %>%
    mutate(source = "dev",
           file = "default_inputs") ->
    default_inputs

out <- rbind(gcam_inputs, default_inputs)
write.csv(x = out, file = file.path(WRITE_TO, "gcamhectorDEV_inputs.csv"), row.names = FALSE)







