# Generate the hector run results that are going to be used in benchmarking
# from the "dev" calibrated hector.
# TODO might want to make a better way to denote which hector parameters are
# being used.

# 0. Set Up --------------------------------------------------------------------

source("scripts/constants.R")
source("scripts/fxns_calibration.R")
source("scripts/QAQC/Q0.fxns.R")

WRITE_TO <- here::here("scripts/QAQC/data")
dir.create(WRITE_TO, showWarnings = FALSE, recursive = TRUE)

VERSION <- "DEV"

# TODO I think that some sort of for loop here to go over different
# parameter files would be the best option.
current_dev <- here::here("data", "intermed", "hector_params.csv")

# Load some older calibration parameterizations

here::here("data", "intermed", "calibration_archive") %>%
    list.files(full.names = TRUE)  ->
    other_param_files

tags <- gsub(pattern = "hector_params-|.csv" , x = basename(other_param_files), replacement = "")
names(other_param_files) <- tags

param_list <- c("current" = current_dev, other_param_files)


# 1. Main Chunk ----------------------------------------------------------------

for(i in seq_along(param_list)){

    # Define the current name indicating the dev version & the calibration
    NAME <- paste0(VERSION, "-", names(param_list)[i])

    # Load the calibrated hector parameters
    calibrated_hector_params <- read.csv(param_list[i])

    # A. Observations Comparison -----------------------------------------------
    # Set up a hector core with the new inputs and hector parameters. Then
    # use the helper function to process hector output for the observation
    # calibration.
    ini <- here::here("inputs/hector-gcam.ini")
    hc  <- newcore(ini, name = NAME)
    hc <- my_setvar_fxn(hc, pars = calibrated_hector_params)

    reset(hc)
    run(hc, runtodate = 2023)

    fetchvars_4comparison(hc = hc,
                          comp = comparison_data) %>%
        mutate(source = NAME) ->
        new_hector_out

    write.csv(x = new_hector_out,
              file = file.path(WRITE_TO, paste0("obs_comparison-", NAME, ".csv")), row.names = FALSE)

    # B.AR6 Benchmarks -------------------------------------------------------------
    # Using that same hector core let's get the AR6 benchmark values.
    VARS <- c("global_tas", "heatflux", "RF_tot", "RF_BC", "RF_OC", "RF_NH3", "RF_SO2", "RF_aci", "RF_O3_trop",
              "RF_H2O_strat", "RF_vol", "RF_albedo", "RF_misc", "RF_CH4")

    fetchvars(hc, 1746:2023, vars = VARS) %>%
        AR6B.get_historical_fxn %>%
        mutate(source = NAME) ->
        out

    write.csv(x = out, file = file.path(WRITE_TO, paste0("AR6benchmarks-", NAME, ".csv")), row.names = FALSE)


    # C. AWGP CO2 100 --------------------------------------------------------------

    # Set up the hector core with the AWGP CO2 100 ini file
    ini <- here::here("scripts/QAQC/inputs/control_AWGPCO2100.ini")
    hc <- my_setvar_fxn(newcore(ini), pars = calibrated_hector_params)
    reset(hc)

    # Compute the AGWP 100 for CO2!
    data.frame(value = co2_awgp_100(hc),
               variable = "CO2 AGWP 100",
               source = NAME) ->
        out

    write.csv(x = out, file = file.path(WRITE_TO, paste0("CO2AGWP100-", NAME, ".csv")), row.names = FALSE)


} # end parameter for loop


# D. Inputs --------------------------------------------------------------------
# Since the inputs do not vary with parameters gathering the dev inputs into
# nice data frame for comparison exists outside of the parameter for loop.
here::here("inputs/gcam_emissions.csv") %>%
    read.csv(comment.char = ";") %>%
    pivot_longer(-Date, names_to = "variable") %>%
    rename(year = Date) %>%
    mutate(source = VERSION,
           file = "gcam_inputs") ->
    gcam_inputs

here::here("inputs/default_inputs.csv") %>%
    read.csv(comment.char = ";") %>%
    pivot_longer(-Date, names_to = "variable") %>%
    rename(year = Date) %>%
    mutate(source = VERSION,
           file = "default_inputs") ->
    default_inputs

rbind(gcam_inputs, default_inputs) ->
    out

write.csv(x = out, file = file.path(WRITE_TO, paste0("input-", VERSION, ".csv")), row.names = FALSE)



