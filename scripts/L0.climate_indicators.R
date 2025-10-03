# Description: Read in the climate indicators data.
# TODO
# - need to add citation
# - download
# - a better way to read in the data

# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# 1. Main Chunk ----------------------------------------------------------------

# Determine which variables to save
vars_to_save <- c("CH4", "N2O")

# Extract the GHG variables of interest.
file.path(DIRS$RAW_DATA, "ClimateIndicator-data-9612b1d",
          "data",
          "greenhouse_gas_concentrations",
          "ghg_concentrations.csv") %>%
    read.csv() %>%
    rename(year = timebound_lower) %>%
    pivot_longer(-year, names_to = "variable") %>%
    filter(variable %in% vars_to_save) %>%
    arrange(variable, year) ->
    raw_ghg_missing

# TODO set these in the ini file
# Use the preindustiral values set in Hector's ini file as the
# 1745 values.
M0=731.41
N0=273.87
preindust_ghg_vals <- data.frame(year = c(1745, 1745),
                                    variable = c("CH4", "N2O"),
                                    value = c(M0, N0))
# Use linear interpolation to fill in the missing years.
preindust_ghg_vals %>%
    rbind(raw_ghg_missing) %>%
    add_missing_data(expected_years = 1745:FINAL_HIST_YEAR, fill = 1) %>%
    mutate(variable = paste0(variable, "_concentration")) ->
    obs_ghg

# 2. Save Output ---------------------------------------------------------------

obs_ghg %>%
    write.csv(file = file.path(DIRS$INTERMED, "L0.climate_indicators.csv"),
              row.names = FALSE)


