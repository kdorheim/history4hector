# Prepare the data used in the calibration process.
# K. Dorheim & P. Scully
# 1. [GHG] from CMIP7
# 2. Global Mean Temperature
# 3. Ocean Heat Content

# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# 1. GHG -----------------------------------------------------------------------
## CMIP7 historical [GHG]
## Nicholls, Z., Meinshausen, M., Pflüger, M., & Lewis, J. (2025).
## CMIP GHG Forcing. Zenodo. https://doi.org/10.5281/ZENODO.14892947

list.files(DIRS$RAW_DATA, pattern = "hist_cmip7_conc.csv", full.names = TRUE) %>%
    lapply(read.csv) %>%
    bind_rows() %>%
    # use consistent variable names as with Hector
    mutate(variable = if_else(variable == "n2o", CONCENTRATIONS_N2O(), variable)) %>%
    mutate(variable = if_else(variable == "co2", CONCENTRATIONS_CO2(), variable)) %>%
    mutate(variable = if_else(variable == "ch4", CONCENTRATIONS_CH4(), variable)) %>%
    # add units information
    mutate(units = if_else(variable == CONCENTRATIONS_N2O(), getunits(N2O_CONSTRAIN()), NA)) %>%
    mutate(units = if_else(variable == CONCENTRATIONS_CH4(), getunits(CH4_CONSTRAIN()), units)) %>%
    mutate(units = if_else(variable == CONCENTRATIONS_CO2(), getunits(CO2_CONSTRAIN()), units)) %>%
    mutate(source = "cmip7") %>%
    # extend the inputs to 1745
    extend_to_1745 ->
    cmip7_ghg

# 2. Global Temp. --------------------------------------------------------------

# Since the temperatures are all relative to a baseline this helper function
# normalize the data to help facilitate the comparison of the data.
# Args
#   data: data frame of global temperature
#   yrs: vector of base period years
# Returns: data frame of the normalized temperature data
normalize_data_fxn <- function(data, yrs){

    # confirm that the df has the required columns and that there is
    # data for all the base period years.
    req_names <- c("year", "value")
    stopifnot(all(req_names %in% names(data)))
    stopifnot(all(yrs %in% data$year))

    # format the units label
    units <- paste("relative to", paste(range(yrs), collapse = "-"))

    # the reference temperature
    data %>%
        filter(year %in% yrs) %>%
        pull(value) %>%
        mean ->
        ref_value

    # normalize to the reference value
    data %>%
        mutate(value = value - ref_value,
               units = units) ->
        out

    return(out)
}


# HadCRUT5 Data Set
# Morice, C. P., Kennedy, J. J., Rayner, N. A., Winn, J. P., Hogan, E.,
#   Killick, R. E., et al. (2021). An updated assessment of near‐surface
#   temperature change from 1850: The HadCRUT5 data set. Journal of Geophysical
#   Research, 126(3). https://doi.org/10.1029/2019jd032361
fname <- file.path(DIRS$RAW_DATA, "HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv")
stopifnot(file.exists(fname))

read.csv(fname, col.names = c("year", "value", "lower", "upper")) %>%
    select(year, value, lower, upper) ->
    hadcrut_raw_data

# Normalize the lower and upper bounds
hadcrut_raw_data %>%
    select(year, value = lower) %>%
    normalize_data_fxn(yrs = 1951:1980) %>%
    pull(value) ->
    lower_values

hadcrut_raw_data %>%
    select(year, value = upper) %>%
    normalize_data_fxn(yrs = 1951:1980) %>%
    pull(value) ->
    upper_values

hadcrut_raw_data %>%
    select(year, value) %>%
    mutate(variable = GMST(),
           source = "HadCRUT5") %>%
    normalize_data_fxn(yrs = 1951:1980) %>%
    cbind(lower = lower_values,
          upper = upper_values) ->
    hadcrut_temp

# Aggregate the values.
temp_obs <- hadcrut_temp


# 3. OHC -----------------------------------------------------------------------
# Kuhlbrodt, T., Voldoire, A., Palmer, M. D., Geoffroy, O., & Killick, R. E. (2023).
#   Historical Ocean Heat Uptake in Two Pairs of CMIP6 Models: Global and Regional
#   Perspectives. Journal of Climate, 36(7), 2183–2203. https://doi.org/10.1175/JCLI-D-22-0468.1
fname <- file.path(DIRS$RAW_DATA, "OHC_ensemble_Kuhlbrodt_etal_2022.csv")
stopifnot(file.exists(fname))

# Reading in only OHC data
ohc_data <- read.table(fname,
                       skip = 2,
                       sep = ",",
                       colClasses = c("numeric", "NULL", "NULL", "NULL",
                                      "NULL", "NULL", "NULL", "numeric",
                                      "numeric"))

# Fixing table formatting
ohc_data <- na.omit(ohc_data)
colnames(ohc_data) <- c("year", "value", "unc")

# Getting rid of non-integer years
ohc_data$year <- ohc_data$year - 0.5

# Adding in new columns to match Hector data frames
ohc_data$source <- "obs"
ohc_data$variable <- "OHC"
ohc_data$units <- "ZJ"

# Adding in confidence interval (if applicable)
ohc_data$lower <- ohc_data$value - ohc_data$unc
ohc_data$upper <- ohc_data$value + ohc_data$unc


# Z. Save Results --------------------------------------------------------------

# Save all the GHG observations
cmip7_ghg %>%
    write.csv(file = file.path(DIRS$CALIBRATION_DATA, "C.ghg_data.csv"),
              row.names = FALSE)


# Save the temperature observations
temp_obs %>%
    write.csv(file = file.path(DIRS$CALIBRATION_DATA, "C.gmst_data.csv"),
              row.names = FALSE)

# Save the ocean heat content data
ohc_data %>%
    mutate(units = "2005-2014 base period") %>%
    write.csv(file = file.path(DIRS$CALIBRATION_DATA, "C.ohc_data.csv"), row.names = FALSE)


