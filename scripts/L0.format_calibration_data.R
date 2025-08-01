# Prepare the data used in the calibration process.
# 1. GHG observations
# 2. Global Mean Temperature
# 3. Ocean Heat Content

# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# 1. GHG -----------------------------------------------------------------------
## 1A. N2O  --------------------------------------------------------------------

# Load the N2O NOAA observations
fname <- file.path(DIRS$RAW_DATA, "n2o_annmean_gl.csv")
stopifnot(file.exists(fname))

read.csv(fname, comment.char = "#") %>%
    select(year, value = mean) %>%
    mutate(variable = CONCENTRATIONS_N2O(),
           source = "NOAA") ->
    noaa_n2o

# Load the N2O observations from RCMIP.
system.file(package  = "hector", "input/tables") %>%
    file.path("ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    filter(Date <= max(noaa_n2o$year)) %>%
    select(year = Date, value = N2O_CONSTRAIN()) %>%
    mutate(variable = CONCENTRATIONS_N2O(),
           source = "RCMIP") %>%
    filter(year < min(noaa_n2o$year)) ->
    rmcip_obs

# Combine the NOAA and RCMIP values.
noaa_n2o %>%
    rbind(rmcip_obs) %>%
    select(year, value, variable) %>%
    mutate(units = getunits(N2O_CONSTRAIN())) %>%
    arrange(year) ->
    n2o_obs

## 1B. CH4  --------------------------------------------------------------------

# Load the CH4 NOAA observations
fname <- file.path(DIRS$RAW_DATA, "ch4_annmean_gl.csv")
stopifnot(file.exists(fname))

read.csv(fname, comment.char = "#") %>%
    select(year, value = mean) %>%
    mutate(variable = CONCENTRATIONS_CH4(),
           source = "NOAA") ->
    noaa_ch4


# Load the CH4 observations from RCMIP.
system.file(package  = "hector", "input/tables") %>%
    file.path("ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    filter(Date <= max(noaa_ch4$year)) %>%
    select(year = Date, value = CH4_CONSTRAIN()) %>%
    mutate(variable = CONCENTRATIONS_CH4(),
           source = "RCMIP") %>%
    filter(year < min(noaa_ch4$year)) ->
    rmcip_obs


# Combine the NOAA and RCMIP values.
noaa_ch4 %>%
    rbind(rmcip_obs) %>%
    select(year, value, variable) %>%
    mutate(units = getunits(CH4_CONSTRAIN())) %>%
    arrange(year) ->
    ch4_obs




# Z. Save Results --------------------------------------------------------------




