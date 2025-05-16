# Description: Read in the climate indicators data.
# TODO
# - need to add citation
# - download
# - a better way to read in the data

# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# 1. Main Chunk ----------------------------------------------------------------
# --- GHG ----------------------------------------------------------------------
# Determine which variables to save
vars_to_save <- c("CH4", "N2O")

# Extract the GHG variables of interest.
file.path(DIRS$RAW_DATA, "ClimateIndicator-data-9612b1d",
          "data",
          "greenhouse_gas_concentrations",
          "ghg_concentrations.csv") %>%
    read.csv %>%
    rename(year = timebound_lower) %>%
    select(-c(time, timebound_upper)) %>%
    pivot_longer(-year, names_to = "variable") %>%
   filter(variable %in% vars_to_save) %>%
    arrange(variable, year) ->
    raw_ghg_missing

raw_ghg_missing %>%
    split(raw_ghg_missing$variable) %>%
    lapply(add_missing_data, expected_years = 1750:FINAL_HIST_YEAR, fill = 1) %>%
    bind_rows %>%
    # TODO this function does not work when the data is
    # starts at 1850
    extend_to_1745 %>%
    mutate(variable = paste0(variable, "_concentration")) %>%
    # Manually adding the units according to the ClimateIndicator data file.
    mutate(units = "ppt") %>%
    mutate(units = if_else(variable == "CO2_concentration", "ppm", units)) %>%
    mutate(units = if_else(variable %in% c("CH4_concentration", "N2O_concentration"), "ppb", units)) ->
    obs_ghg

obs_ghg %>%
    mutate(sector = "WORLD",
           source = "ClimateIndicator") ->
    output


# 2. Save Output ---------------------------------------------------------------

output %>%
    check_req_names(req_cols = HEADERS$L0) %>%
    write.csv(file = file.path(DIRS$INTERMED, "L0.ClimateIndicators_raw.csv"),
              row.names = FALSE)


