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



# 4. AWGP CO2 100 --------------------------------------------------------------

# Some functions that are defined here because they are only relevant to this
# section.
# TODO this might end up moving to a fxns script....

# Helper function for running the IRF following the AR6 protocol to get the CO2 AWGP 100
# Args
#   HC: an active hector core, it should have the parameter values of interest
#       already defined here
#   IMPULSE_YR: default set to 1900
#   HORIZON: default set to 100
#   end_yr: default set to 2000
co2_awgp_100 <- function(HC, IMPULSE_YR = 1900, HORIZON = 100, end_yr = 2000){

    stopifnot(class(hc)[1] == "hcore")

    # these should most likely be set to default
    VARS <- RF_CO2()
    YRS <- 1745:2400

    # This should be
    CO2_Tg <- 1
    CONVERSION_FACTOR <- (12.01/44.01) * 1e-3 # convert from Tg CO2 to PgC

    run(HC, runtodate = end_yr)
    baseline <- fetchvars(HC, dates = YRS, vars = VARS)

    # the impulse run
    baseline_val <- fetchvars(HC, IMPULSE_YR, FFI_EMISSIONS())
    impulse_val <- baseline_val$value +  (CO2_Tg * CONVERSION_FACTOR)
    setvar(HC, IMPULSE_YR, var = FFI_EMISSIONS(), values = impulse_val, unit = getunits(FFI_EMISSIONS()))
    run(HC, runtodate = end_yr)
    impulse <- fetchvars(hc, dates = YRS, vars = VARS)


    impulse %>%
        rename(impulse=value) %>%
        left_join(baseline) %>%
        mutate(irf = impulse - value,
               year = year - (IMPULSE_YR+1)) %>%
        filter(year <= HORIZON) %>%
        filter(year >= 0) %>%
        pull(irf) %>%
        sum ->
        out

    return(out)
}


ini <- here::here("scripts/QAQC/inputs/control_AWGPCO2100.ini")

# Load the parameter values to test out
here::here("data/intermed/hector_params.csv") %>%
    read.csv ->
    params

hc <- my_setvar_fxn(newcore(ini), pars = params)

data.frame(value = co2_awgp_100(hc),
           variable = "CO2 AGWP 100",
           source = "hectorV3.5.X") ->
    out

write.csv(x = out, file = file.path(WRITE_TO, "gcamhectorDEV_CO2AGWP100.csv"), row.names = FALSE)




ini <- here::here("inputs/hector-gcam.ini")
hc  <- newcore(ini, name = "dev")
run(hc, runtodate = 2023)
fetchvars(hc, 1746:2023, vars = c(NBP())) %>%
    ggplot(aes(year, value)) +
    geom_line()
