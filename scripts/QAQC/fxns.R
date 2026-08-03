# Some functions that are pretty helpful with some of the QAQC!

library(dplyr)
library(tidyr)

# Helper function to normalize hector output
# Args
#   d: data frame of hector results
#   yrs: vector of reference period
# Returns: data frame of normalized results
normalize_fxn <- function(d, yrs){

    # check inputs
    req_cols <- c("variable", "scenario", "value")
    stopifnot({
        d %>%
            select(variable, scenario) %>% distinct %>%
            nrow == 1})
    stopifnot(all(yrs %in% d$year))


    d %>%
        filter(year %in% yrs) %>%
        pull(value) %>%
        mean ->
        ref_value

    d %>%
        mutate(value = value - ref_value) ->
        out

    return(out)
}



# Helper function convert ocean heat flux to ocean heat content
# Args
#   d: data frame of historical hector results
# Returns: data frame of ocean heat content
AR6B.internal_ohc <- function(data){

    # Ocean heat content constants
    OCEAN_AREA <- 5100656e8 * (1 - 0.29) # The total area of the ocean
    W_TO_ZJ <- 3.155693e-14              # Watts to ZJ

    yrs <- 1971:2018

    data %>%
        filter(variable == "heatflux") %>%
        mutate(value = value * OCEAN_AREA * W_TO_ZJ) %>%
        mutate(variable = "OHC", units = "ZJ") %>%
        filter(year %in% yrs) %>%
        summarise(value = mean(value), .by = c(scenario, variable, units)) %>%
        mutate(value = value * length(yrs)) %>%
        mutate(year = "1971-2018") ->
        out

    return(out)

}


# Helper function that gathers all of the historical rf
# Args
#   d: data frame of historical hector results
# Returns: data frame of historical rf benchmarks
AR6B.internal_hist_rf <- function(data){

    # Save a copy of the version
    v <- unique(data$version)

    # The mean aerosol RF
    data %>%
        filter(year %in% 2005:2015) %>%
        filter(variable %in% c("RF_BC", "RF_OC", "RF_NH3", "RF_SO2", "RF_aci")) %>%
        # get the total aerosol RF per YEAR
        summarise(value = sum(value), .by = c("scenario", "year", "units")) %>%
        # get the mean aerosol over our period of interest
        summarise(value = mean(value), .by = c(scenario, units)) %>%
        mutate(variable = "total aerosol RF", year = "2005-15") ->
        aero_rf

    # CH4 forcing in 2019
    data %>%
        filter(variable == "RF_CH4" & year == 2019) ->
        ch4_rf

    # The non GHG RF in 2019
    non_ghg_vars <- c("RF_BC", "RF_OC", "RF_NH3", "RF_SO2", "RF_aci", "RF_O3_trop",
                      "RF_H2O_strat", "RF_vol", "RF_albedo", "RF_misc")
    data %>%
        filter(year == 2019) %>%
        filter(variable %in% non_ghg_vars) %>%
        summarise(value = sum(value), .by = c("scenario", "year", "units")) %>%
        mutate(variable = "non_ghg_ERF") ->
        nonghg_rf

    # The wmghg RF
    data %>%
        filter(year == 2019) %>%
        filter(variable == "RF_tot") %>%
        mutate(value = value - nonghg_rf$value,
               variable = "wmghg RF") ->
        wmghg_rf

    # Format all the historical RF values
    bind_rows(ch4_rf, wmghg_rf, nonghg_rf) %>%
        mutate(year = as.character(year)) %>%
        bind_rows(aero_rf) %>%
        mutate(version = v) ->
        out

    return(out)
}


# Helper function that gathers all of the historical temp
# Args
#   d: data frame of historical hector results
# Returns: data frame of historical temp benchmark
AR6B.internal_hist_temp <- function(data){

    # historical warming
    data %>%
        filter(year %in% 1750:2100) %>%
        filter(variable == "global_tas") %>%
        normalize_fxn(yrs = 1850:1900) %>%
        filter(year %in% 1995:2014) %>%
        summarise(value = mean(value), .by = c(scenario, variable, units)) %>%
        mutate(variable = "hist. warming",
               year = "1995-2014") ->
        out

    return(out)

}


# The function that gets all the historical benchmarks
# TODO there are some values from 2019... which might be a problem
# Args
#   rslts: data.frame of hector results
# Returns: data.frame of the historical benchmarks from AR6
AR6B.get_historical_fxn <- function(rslts){

    rslts %>%
        # filter(scenario == "ssp245") %>%
        filter(year <= 2025) %>%
        mutate(scenario = "historical") ->
        data


    hist_ohc   <- AR6B.internal_ohc(data)
    hist_rf    <- AR6B.internal_hist_rf(data)
    hist_temp  <- AR6B.internal_hist_temp(data)


    bind_rows(hist_ohc,
              hist_rf,
              hist_temp) %>%
        mutate(version = unique(data$version)) ->
        out

    return(out)

}
