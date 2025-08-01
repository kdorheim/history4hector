# Prepare the data used in the calibration process.
# 1. GHG observations
# 2. Global Mean Temperature
# 3. Ocean Heat Content

# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# 1. GHG -----------------------------------------------------------------------
# Citation for all of the GHGs
# Lan, X., K.W. Thoning, and E.J. Dlugokencky: Trends in globally-averaged CH4,
#   N2O, and SF6 determined from NOAA Global Monitoring Laboratory measurements.
#   Version 2025-07, https://doi.org/10.15138/P8XG-AA10
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


## 1C. CO2  --------------------------------------------------------------------


# Load the CH4 NOAA observations
fname <- file.path(DIRS$RAW_DATA, "co2_annmean_mlo.csv")
stopifnot(file.exists(fname))

read.csv(fname, comment.char = "#") %>%
    select(year, value = mean) %>%
    mutate(variable = CONCENTRATIONS_CO2(),
           source = "NOAA") ->
    noaa_co2


# Load the CH4 observations from RCMIP.
system.file(package  = "hector", "input/tables") %>%
    file.path("ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    filter(Date <= max(noaa_co2$year)) %>%
    select(year = Date, value = CO2_CONSTRAIN()) %>%
    mutate(variable = CONCENTRATIONS_CO2(),
           source = "RCMIP") %>%
    filter(year < min(noaa_co2$year)) ->
    rmcip_obs


# Combine the NOAA and RCMIP values.
noaa_co2 %>%
    rbind(rmcip_obs) %>%
    select(year, value, variable) %>%
    mutate(units = getunits(CO2_CONSTRAIN())) %>%
    arrange(year) ->
    co2_obs


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


# NASA GISS global temperature
# GISTEMP Team, 2019: GISS Surface Temperature Analysis (GISTEMP), version 3.
#   NASA Goddard Institute for Space Studies. Dataset accessed 20YY-MM-DD
#   at https://data.giss.nasa.gov/gistemp/.
# Hansen, J., R. Ruedy, M. Sato, and K. Lo, 2010: Global surface temperature
#   change, Rev. Geophys., 48, RG4004, doi:10.1029/2010RG000345.
fname <- file.path(DIRS$RAW_DATA, "GLB.Ts+dSST.csv")
stopifnot(file.exists(fname))

# Load data
read.csv(fname, skip = 1) %>%
    select(year = Year, value = "J.D") %>%
    mutate(value = as.numeric(value)) %>%
    na.omit %>%
    mutate(variable = GMST(),
           source = "GISS",
           units = "1951–1980 base period") %>%
    normalize_data_fxn(yrs = 1951:1980) ->
    nasa_temp


# NOAA global temperature
# Huang, B., X. Yin, M. J. Menne, R. Vose, and H. Zhang, NOAA Global Surface
#   Temperature Dataset (NOAAGlobalTemp), Version 6.0.0 [indicate subset used].
#   NOAA National Centers for Environmental Information. https://doi.org/10.25921/rzxg-p717
fname <- file.path(DIRS$RAW_DATA, "aravg.ann.land_ocean.90S.90N.v6.0.0.202506.asc")
stopifnot(file.exists(fname))

read.table(fname, col.names = c("year", "value", "total error",
                                "high-frequency", "low-frequency",
                                "bias error")) %>%
    select(year, value) %>%
    mutate(variable = GMST(),
           source = "NOAA") %>%
    normalize_data_fxn(yrs = 1951:1980) ->
    noaa_temp

# HadCRUT5 Data Set
# Morice, C. P., Kennedy, J. J., Rayner, N. A., Winn, J. P., Hogan, E.,
#   Killick, R. E., et al. (2021). An updated assessment of near‐surface
#   temperature change from 1850: The HadCRUT5 data set. Journal of Geophysical
#   Research, 126(3). https://doi.org/10.1029/2019jd032361
fname <- file.path(DIRS$RAW_DATA, "HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv")
stopifnot(file.exists(fname))

read.csv(fname, col.names = c("year", "value", "lower", "upper")) %>%
    select(year, value) %>%
    mutate(variable = GMST(),
           source = "HadCRUT5") %>%
    normalize_data_fxn(yrs = 1951:1980) ->
    hadcrut_temp

# Aggregate the values.
nasa_temp %>%
    bind_rows(hadcrut_temp,
              noaa_temp) %>%
    summarise(value = mean(value),  .by = c("year", "units", "variable")) %>%
    mutate(source = "obs") ->
    temp_obs






# Z. Save Results --------------------------------------------------------------

# Save all the GHG observations
rbind(n2o_obs, ch4_obs, co2_obs) %>%
    mutate(source = "obs") %>%
    write.csv(file = file.path(DIRS$CALIBRATION_DATA, "C.ghg_data.csv"),
              row.names = FALSE)

# Save the temperature observations
write.csv(temp_obs, file = file.path(DIRS$CALIBRATION_DATA, "C.gmst_data.csv"),
          row.names = FALSE)


