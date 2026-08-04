# This script processes the output from a gcam v 8.8 - hector v3.2 run so that
# it can be used in the observations and benchmarking comparisons. This assumes
# that the gcam run has already been complete and outputs (both the data base)
# and the hector outputstream are saved in the raw-data file.

# 0. Set Up --------------------------------------------------------------------

source("scripts/constants.R")
source("scripts/fxns_calibration.R")
source("scripts/QAQC/fxns.R")

WRITE_TO <- here::here("scripts/QAQC/data")
dir.create(WRITE_TO, showWarnings = FALSE, recursive = TRUE)

# 1. Observations Comparison ---------------------------------------------------

here::here("data/raw-data/gcam8.8-hectorv3.2/gcam-hector-outputstreamReference.csv") %>%
    read.csv(comment.char = "#") %>%
    filter(spinup == 0) %>%
    mutate(scenario = "gcamV8.8-hectorV3.2") %>%
    mutate(variable = if_else(variable == "FCH4", "RF_CH4", variable)) %>%
    select(scenario, year, variable, value, units) ->
    hector_output


fetchvars_4comparison(hc = NULL,
                      comp = comparison_data,
                      hector_out = hector_output) %>%
    mutate(source = "gcamV8.8-hectorV3.2") ->
    hector_for_obs_comparison

write.csv(x = hector_for_obs_comparison, file = file.path(WRITE_TO, "gcamV8.8hectorV3.2_obs_comparison.csv"), row.names = FALSE)


# 2.AR6 Benchmarks -------------------------------------------------------------

AR6B.get_historical_fxn(hector_output) %>%
    mutate(source = "gcamV8.8-hectorV3.2") ->
    out

write.csv(x = out, file = file.path(WRITE_TO, "gcamV8.8hectorV3.2_AR6benchmarks.csv"), row.names = FALSE)



# 3. Inputs --------------------------------------------------------------------

here::here("data/raw-data/gcam8.8-hectorv3.2/gcam_emissions.csv") %>%
    read.csv(comment.char = ";") %>%
    pivot_longer(-Date, names_to = "variable") %>%
    rename(year = Date) %>%
    mutate(source = "gcamV8.8-hectorV3.2",
           file = "gcam_inputs") ->
    gcam_inputs

here::here("data/raw-data/gcam8.8-hectorv3.2/default_emissions.csv") %>%
    read.csv(comment.char = ";") %>%
    pivot_longer(-Date, names_to = "variable") %>%
    rename(year = Date) %>%
    mutate(source = "gcamV8.8-hectorV3.2",
           file = "default_inputs") ->
    default_inputs


out <- rbind(gcam_inputs, default_inputs)

write.csv(x = out, file = file.path(WRITE_TO, "gcamV8.8-hectorV3.2_inputs.csv"), row.names = FALSE)





