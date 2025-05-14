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
   # filter(year >= 1850) %>%
    pivot_longer(-year, names_to = "variable") %>%
    filter(variable %in% vars_to_save) %>%
    arrange(variable, year) ->
    raw_ghg_missing

# Fill in missing data using linear interpolation.
add_missing_data(raw_ghg_missing,
                 expected_years = min(raw_ghg_missing$year):FINAL_HIST_YEAR,
                 fill = 1) %>%
    # TODO this function does not work when the data is
    # starts at 1850
    extend_to_1745 %>%
    mutate(variable = paste0(variable, "_concentration")) ->
    obs_ghg

# 2. Save Output ---------------------------------------------------------------

obs_ghg %>%
    write.csv(file = file.path(DIRS$INTERMED, "L0.climate_indicators.csv"),
              row.names = FALSE)


