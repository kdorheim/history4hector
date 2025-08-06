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
# Returns: active hector core ready to run in constraint mode
newcore_CO2_CH4_N2O <- function(ini, name = "no name"){

    stopifnot(file.exists(ini))

    # Instantiate the hector core
    hc <- newcore(inifile = ini, name = name)

    # Subset the GHG's that should be used as constraints.
    ch4 <- filter(ghg_constraints, variable == CONCENTRATIONS_CH4())
    n2o <- filter(ghg_constraints, variable == CONCENTRATIONS_N2O())
    co2 <- filter(ghg_constraints, variable == CONCENTRATIONS_CO2())


    # Apply the constraints to the Hector core
    setvar(core = hc, dates = ch4$year, var = CH4_CONSTRAIN(),
           values = ch4$value, unit = getunits(CH4_CONSTRAIN()))
    reset(hc)

    setvar(core = hc, dates = n2o$year, var = N2O_CONSTRAIN(),
           values = n2o$value, unit = getunits(N2O_CONSTRAIN()))
    reset(hc)

    setvar(core = hc, dates = co2$year, var = CO2_CONSTRAIN(),
           values = co2$value, unit = getunits(CO2_CONSTRAIN()))
    reset(hc)

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


# Fetch the relevant Hector results for the data comparison,
# make sure that they are in the correct units.
# Args
#   hc: active hector core that has completed a run
#   comp: data.frame of the observations to use in the comparison
# Returns: data.frame of Hector results in the correct units for comparison
fetchvars_4comparison <- function(hc, comp){

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


    bind_rows(hector_co2,
              hector_gmst,
              hector_ohc) %>%
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
        mutate(NAE = NAE) %>%
        summarise(MNAE = mean(NAE), .by = variable) ->
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
        summarise(MNAE = mean(NAE), .by = variable) ->
        co2_error

    # Calculate the MAE for temp & heat flux which needs to account for the
    # uncertainty range.
    gmst_hf_error <- E4_unc_helper(wide_hector_obs)

    out <- rbind(co2_error, gmst_hf_error)

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
        out <- mean(error$MNAE)
        return(out)
    }

    return(fn)
}




# Z. Testing calibration protocol --------------------------------------------
#
inital_guess <- c("diff" = 2.5)
ini <- "inputs/hector-gcam.ini"
core <- newcore_CO2_CH4_N2O(ini, name = "contrs")
comparison_data %>%
    filter(variable != CONCENTRATIONS_CO2()) ->
    comp_data

err_fn <- obj_E4_unc

fxn <- internal_fn(p = inital_guess, err_fn = err_fn, obs = comp_data, core = core)
fit1<- optim(par = inital_guess, fn = fxn, lower = c(0.1), upper = c(10), method = "L-BFGS-B")

# Trying to decide what else should be included in this funciotn...


fit1$convergence
fit1$par

fit1<- list()
fit1$par <- c("S" = 3, "diff" = 1.21)
# okay so now that the temperature parmeters have been resolved what
# about the co2 parameters?

inital_guess <- c("beta" = 0.36, "q10_rh" = 2.1)
ini <- "inputs/hector-gcam.ini"
core <- newcore_CH4_N2O(ini, name = "free co2")
core <- my_setvar_fxn(core, pars = round(fit1$par, 2))

comparison_data %>%
    filter(variable == CONCENTRATIONS_CO2()) ->
    comp_data

err_fn <- obj_E4_unc
fxn <- internal_fn(p = inital_guess, err_fn = err_fn, obs = comp_data, core = core)
fit2 <- optim(par = inital_guess, fn = fxn)
round(fit2$par, 2)

# Z. Testings and Plots --------------------------------------------------------

YRS <- 1850:2020
VARS <- c(CONCENTRATIONS_CO2(), CONCENTRATIONS_N2O(), CONCENTRATIONS_CH4(),
          EMISSIONS_CH4(), EMISSIONS_N2O(), RF_TOTAL())

# The free running
ini <- system.file(package = "hector", "input/hector_ssp245.ini")
hc1 <- newcore(ini, name = "free running")
run(hc1)
out1 <- fetchvars_4comparison(hc1, comparison_data)


hc2 <- newcore_CH4_N2O(ini, "CH4 & N2O constrainted")
run(hc2)
out2 <- fetchvars(hc2, YRS, VARS)

hc3 <- newcore_CO2_CH4_N2O(ini, "CH4, N2O, & CO2 constrainted")
run(hc3)
out3 <- fetchvars(hc3, YRS, VARS)


to_plot <- rbind(out1, out2, out3)

to_plot %>%
    filter(variable == RF_TOTAL()) %>%
    ggplot() +
    geom_line(aes(year, value, color = scenario, linetype = scenario))







