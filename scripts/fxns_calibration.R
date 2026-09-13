# Functions used by the Hector calibration script!


# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# 1. newcore helper functions --------------------------------------------------

# Load the CH4 and N2O constraints that are used in the calibration hc runs.
fname <- file.path(DIRS$CALIBRATION_DATA, "C.ghg_data.csv")
stopifnot(file.exists(fname))
ghg_constraints <- read.csv(fname)

# Set up a new Hector core with constrained CH4 and N2O concentrations
# Args
#   ini: path to the hector ini
#   name: "no name" by default but will name the hector results if a
#           different string is provided
# Returns: active hector core ready to run in constraint mode
newcore_CH4_N2O <- function(ini, name = "no name"){

    stopifnot(file.exists(ini))

    # Instantiate the hector core
    hc <- newcore(inifile = ini, name = name)

    # Subset the GHG's that should be used as constraints.
    ch4 <- filter(ghg_constraints, variable == CONCENTRATIONS_CH4())
    n2o <- filter(ghg_constraints, variable == CONCENTRATIONS_N2O())

    # Apply the constraints to the Hector core
    setvar(core = hc, dates = ch4$year, var = CH4_CONSTRAIN(),
           values = ch4$value, unit = getunits(CH4_CONSTRAIN()))
    reset(hc)

    setvar(core = hc, dates = n2o$year, var = N2O_CONSTRAIN(),
           values = n2o$value, unit = getunits(N2O_CONSTRAIN()))
    reset(hc)

    return(hc)

}



# Set up a new Hector core with constrained CH4, N2O, and CO2 concentrations
# Args
#   ini: path to the hector ini
#   name: "no name" by default but will name the hector results if a
#           different string is provided
#   CH4: default set to FALSE, if set to TRUE then the historical CH4 constraints will be used
#   CO2: default set to FALSE, if set to TRUE then the historical CO2 constraints will be used
#   N2O: default set to FALSE, if set to TRUE then the historical N2O constraints will be used
# Returns: active hector core ready to run in constraint mode
newcore_w_constraints <- function(ini, name = "no name", CH4 = FALSE, CO2 = FALSE, N2O = FALSE){

    stopifnot(file.exists(ini))

    # Instantiate the hector core
    hc <- newcore(inifile = ini, name = name)

    # Subset the GHG's that should be used as constraints.
    ch4 <- filter(ghg_constraints, variable == CONCENTRATIONS_CH4())
    n2o <- filter(ghg_constraints, variable == CONCENTRATIONS_N2O())
    co2 <- filter(ghg_constraints, variable == CONCENTRATIONS_CO2())


    if(CH4){
        # Apply the constraints to the Hector core
        setvar(core = hc, dates = ch4$year, var = CH4_CONSTRAIN(),
               values = ch4$value, unit = getunits(CH4_CONSTRAIN()))
        reset(hc)
    }
    if(N2O){
        setvar(core = hc, dates = n2o$year, var = N2O_CONSTRAIN(),
               values = n2o$value, unit = getunits(N2O_CONSTRAIN()))
        reset(hc)
    }

    if(CO2){
        setvar(core = hc, dates = co2$year, var = CO2_CONSTRAIN(),
               values = co2$value, unit = getunits(CO2_CONSTRAIN()))
        reset(hc)
    }

    return(hc)
}


# 2. Comparison data -----------------------------------------------------------

# Import all the comparison data
list(file.path(DIRS$CALIBRATION_DATA, "C.ghg_data.csv"),
     file.path(DIRS$CALIBRATION_DATA, "C.gmst_data.csv"),
     file.path(DIRS$CALIBRATION_DATA, "C.ohc_data.csv")) %>%
    lapply(read.csv) %>%
    bind_rows %>%
    filter(year <= FINAL_HIST_YEAR) ->
    comparison_data


# 3. fetchvars helpers ---------------------------------------------------------

# Ocean heat content constants
OCEAN_AREA <- 5100656e8 * (1 - 0.29) # The total area of the ocean
W_TO_ZJ <- 3.155693e-14              # Watts to ZJ


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
    #stopifnot(all(yrs %in% data$year))

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


# Fetch the relevant Hector results for the data comparison,
# make sure that they are in the correct units.
# Args
#   hc: active hector core that has completed a run or set to NULL
#   comp: data.frame of the observations to use in the comparison
#   hector_out: data.frame of hector results or set to NULL, default set to NULL
# Returns: data.frame of Hector results in the correct units for comparison
fetchvars_4comparison <- function(hc, comp, hector_out = NULL){

    null_length <- sum(c(is.null(hc), is.null(hector_out)))
    stopifnot(null_length == 1)

    hector_ch4 <- hector_n2o <- NULL

    # If the object read in is a hector core use fetchvars to get the
    # equalivent to compare with observations otherwise extract from
    # a hector output data frame.
    if(class(hc)[[1]] == "hcore"){

        # Make sure that the hector core has been run!
        stopifnot(!hc[["strtdate"]] == hc[["enddate"]])

        yrs <- min(comp$year):max(comp$year)

        fetchvars(core = hc,
                  dates = yrs,
                  vars = CONCENTRATIONS_CO2()) ->
            hector_co2

        fetchvars(core = hc,
                  dates = yrs,
                  vars = GMST()) %>%
            normalize_data_fxn(yrs = 1951:1980) %>%
            mutate(units = "relative to 1951-1980") ->
            hector_gmst

        fetchvars(core = hc,
                  dates = yrs,
                  vars = HEAT_FLUX()) %>%
            # Convert from heat flux to OHC that can be used
            mutate(value = value * OCEAN_AREA * W_TO_ZJ) %>%
            mutate(value = cumsum(value),
                   variable = "OHC") %>%
            # Normalize to the correct reference period.
            normalize_data_fxn(yrs = 2005:2014) %>%
            mutate(units = "2005-2014 base period") ->
            hector_ohc


        if (CONCENTRATIONS_CH4() %in% comp$variable) {
            fetchvars(core = hc,
                      dates = yrs,
                      vars = CONCENTRATIONS_CH4()) ->
                hector_ch4

        }


        if (CONCENTRATIONS_N2O() %in% comp$variable) {
            fetchvars(core = hc,
                      dates = yrs,
                      vars = CONCENTRATIONS_N2O()) ->
                hector_n2o

        }





    } else if(is.data.frame(hector_out)){


        yrs <- min(comp$year):max(comp$year)


        hector_out %>%
            filter(year %in% yrs, variable == CONCENTRATIONS_CO2()) ->
            hector_co2

        hector_out %>%
            filter(year %in% yrs, variable == GMST()) %>%
            normalize_data_fxn(yrs = 1951:1980) %>%
            mutate(units = "relative to 1951-1980") ->
            hector_gmst



        hector_out %>%
            filter(year %in% yrs, variable == HEAT_FLUX()) %>%
            # Convert from heat flux to OHC that can be used
            mutate(value = value * OCEAN_AREA * W_TO_ZJ) %>%
            mutate(value = cumsum(value),
                   variable = "OHC") %>%
            # Normalize to the correct reference period.
            normalize_data_fxn(yrs = 2005:2014) %>%
            mutate(units = "2005-2014 base period") ->
            hector_ohc

        if (CONCENTRATIONS_CH4() %in% comp$variable) {
            hector_out %>%
                filter(year %in% yrs, variable == CONCENTRATIONS_CH4()) ->
                hector_ch4

        }


        if (CONCENTRATIONS_N2O() %in% comp$variable) {
            hector_out %>%
                filter(year %in% yrs, variable == CONCENTRATIONS_N2O()) ->
                hector_n2o

        }



    }



    bind_rows(hector_co2,
              hector_gmst,
              hector_ohc,
              hector_n2o,
              hector_ch4) %>%
        select(scenario, year, variable, hector = value, units) ->
        out

    return(out)
}









# 4. setvar helpers ------------------------------------------------------------

# Helper function that sets an active Hector core with user defined parameters
# Args
#   hc: active hector core
#   pars: vector of the hector parameter values
# Returns: an active hector core with the new parameter values
my_setvar_fxn <- function(hc, pars){

    print(pars)


    for(i in 1:length(pars)){

        var <- names(pars)[[i]]
        val <- pars[[i]]

        setvar(core = hc, dates = NA, var = var,
               values = val, unit = getunits(var))
        reset(hc)

    }

    return(hc)


}


# 5. error & other functions ---------------------------------------------------

# NAE uncertainty function
# Args
#   wide: data.frame formatted wide of Hector output and observations
# Returns: data.frame of the mean normalize absolute error (based on uncertainty range)
E4_unc_helper <- function(wide){

    # Make sure that all the required columns are included!
    req_cols <- c("variable", "hector", "lower", "upper", "value")
    stopifnot(all(req_cols %in% names(wide)))

    wide %>%
        filter(variable %in% c(GMST(), "OHC")) ->
        xx

    # Determine if the hector observation is out side of the uncertainty bounds...
    lower_cond <- xx$hector <= xx$lower
    upper_cond <- xx$hector >= xx$upper

    # Assume an error of 0!
    NAE <- rep(0, length(lower_cond))

    lower_error <- abs(xx$hector - xx$lower) / abs(xx$value)
    upper_error <- abs(xx$hector - xx$upper) / abs(xx$value)

    NAE[lower_cond] <- lower_error[lower_cond]
    NAE[upper_cond] <- upper_error[upper_cond]

    xx %>%
        mutate(NAE = NAE / abs(value)) %>%
        summarise(value = mean(NAE), .by = variable) ->
        out

    return(out)

}


# Normalized abs difference for each variable, analogous to
# Experiment 4 (E4, unc) from P. Scully et al in prep
# Args
#   hector_data: data.frame of hector output ready for
#       comparison (produced from fetchvars_4comparison)
#   comp_data: data.frame of the observations
# Returns: NAE for each of the variables considered
obj_E4_unc <- function(hector_data, comp_data){

    # Join Hector and observational data.
    comp_data %>%
        filter(variable %in% hector_data$variable) %>%
        left_join(hector_data, by = join_by(year, variable, units)) ->
        wide_hector_obs

    # Throw an error if there is an issue with the join.
    stopifnot(!any(is.na(wide_hector_obs$hector)))

    # Calculate the MAE for CO2 which does not need to account for uncertainty.
    wide_hector_obs %>%
        filter(variable == CONCENTRATIONS_CO2()) %>%
        mutate(NAE = abs(hector - value)/abs(value)) %>%
        summarise(value = mean(NAE), .by = variable) ->
        co2_error

    # Calculate the MAE for temp & heat flux which needs to account for the
    # uncertainty range.
    gmst_hf_error <- E4_unc_helper(wide_hector_obs)

    out <- rbind(co2_error, gmst_hf_error)

    return(out)

}



# MSE
#   hector_data: data.frame of hector output ready for
#       comparison (produced from fetchvars_4comparison)
#   comp_data: data.frame of the observations
# Returns: MSE for each of the variables considered
obj_MSE <- function(hector_data, comp_data){

    # Join Hector and observational data.
    comp_data %>%
        filter(variable %in% hector_data$variable) %>%
        left_join(hector_data, by = join_by(year, variable, units)) ->
        wide_hector_obs

    # Throw an error if there is an issue with the join.
    stopifnot(!any(is.na(wide_hector_obs$hector)))

    # Calculate the MSE of each of the variables.
    wide_hector_obs %>%
        mutate(SE = (hector - value)^2) %>%
        summarise(value = mean(SE), .by = "variable") ->
        out

    return(out)

}




# Helper function that returns the objective function to be used by optim
# Args
#   p: hector parameter values to be optimized
#   err_fn: function such as obj_E4_unc that will be used to determine teh MSE
#   obs: data.frame of the observational values
#   core: active Hector core
internal_fn <- function(p, err_fn, obs, core){

    stopifnot(isactive(core))
    stopifnot(is.character(getunits(names(p))))
    max_yr <- FINAL_HIST_YEAR

    # Make sure that the initial guess runs without error
    # otherwise an error will be thrown.
    core <- my_setvar_fxn(hc = core, pars = p)
    run(core, runtodate = max_yr)


    fn <- function(par){

        core <- my_setvar_fxn(hc = core, pars = par)
        run(core, runtodate = max_yr)
        hector_data <- fetchvars_4comparison(hc = core, comp = obs)

        error <- err_fn(hector_data = hector_data, comp_data = obs)
        out <- mean(error$value)
        return(out)
    }

    return(fn)
}


