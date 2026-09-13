# This script processes the output from a gcam v 8.8 - hector v3.2 run so that
# it can be used in the observations and benchmarking comparisons. This assumes
# that the gcam run has already been complete and outputs (both the data base)
# and the hector output stream are saved in the raw-data file.
#
# In theory this script could be updated to other versions of GCAM-hector coupling
# with pretty minimal changes.
#
# This script really only needs to be run once.

# 0. Set Up --------------------------------------------------------------------

source("scripts/constants.R")
source("scripts/fxns_calibration.R")
source("scripts/QAQC/Q0.fxns.R")

WRITE_TO <- here::here("scripts/QAQC/data")
dir.create(WRITE_TO, showWarnings = FALSE, recursive = TRUE)

# Name of the GCAM-Hector coupling version.
GCAM_HECTOR <- "gcam8.8-hectorv3.2"

# 1. Observations Comparison ---------------------------------------------------

# Load the gcam-hector output stream file.
list.files(here::here("scripts", "QAQC", "raw-data", GCAM_HECTOR),
           pattern = "gcam-hector-outputstreamReference.csv", full.names = TRUE) %>%
    read.csv(comment.char = "#") %>%
    filter(spinup == 0) %>%
    mutate(scenario = GCAM_HECTOR) %>%
    mutate(variable = if_else(variable == "FCH4", "RF_CH4", variable)) %>%
    select(scenario, year, variable, value, units) ->
    hector_output

# Process the hector results so that we can easily compare wit observations.
fetchvars_4comparison(hc = NULL,
                      comp = comparison_data,
                      hector_out = hector_output) %>%
    mutate(source = GCAM_HECTOR) ->
    hector_for_obs_comparison

write.csv(x = hector_for_obs_comparison,
          file = file.path(WRITE_TO, paste0("obs_comparison-", GCAM_HECTOR, ".csv")), row.names = FALSE)


# 2.AR6 Benchmarks -------------------------------------------------------------

# Similarly process the hector results get the AR6 benchmarks.
AR6B.get_historical_fxn(hector_output) %>%
    mutate(source = GCAM_HECTOR) ->
    out

write.csv(x = out, file = file.path(WRITE_TO, paste0("AR6benchmarks-", GCAM_HECTOR, ".csv")), row.names = FALSE)



# 3. Inputs --------------------------------------------------------------------

# Load the input files so we can compare with the new inputs.
list.files(here::here("scripts", "QAQC", "raw-data", GCAM_HECTOR),
           pattern = "gcam_emissions.csv", full.names = TRUE) %>%
    read.csv(comment.char = ";") %>%
    pivot_longer(-Date, names_to = "variable") %>%
    rename(year = Date) %>%
    mutate(source = GCAM_HECTOR,
           file = "gcam_inputs") ->
    gcam_inputs

list.files(here::here("scripts", "QAQC", "raw-data", GCAM_HECTOR),
           pattern = "default_emissions.csv", full.names = TRUE) %>%
    read.csv(comment.char = ";") %>%
    pivot_longer(-Date, names_to = "variable") %>%
    rename(year = Date) %>%
    mutate(source = GCAM_HECTOR,
           file = "default_inputs") ->
    default_inputs

out <- rbind(gcam_inputs, default_inputs)

write.csv(x = out, file = file.path(WRITE_TO, paste0("input-", GCAM_HECTOR, ".csv")), row.names = FALSE)
