# Description: Read in all the L1 and L2 data to determine the hector
# inputs needed by during a GCAM that will go into the non-gcam input csv table.
#
# This script generates two files
#   (1) long format of the csv file that might be useful for debugging
#   (2) properly formatted input table
# TODO
#   How do we want to deal with the future period that is needed for the non
#   GCAM emissions?? Is there a better assumption we could make other than SSP2-45?
# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Generates some helpful quality assessment plots if set to true.
CHECK <- FALSE

# --- Import Data --------------------------------------------------------------
# Load data
DIRS$INTERMED %>%
    list.files(pattern = "L1",
               full.names = TRUE) ->
    L1_files

L1_files %>%
    lapply(read.csv) %>%
    do.call(what = "rbind") ->
    L1_data

# There are some specific data that is required
file.path(DIRS$INTERMED, "L0.climate_indicators.csv") %>%
    read.csv ->
    conc_data

# Read in other L2 emissions that are needed to calculate
# the natural emissions.
file.path(DIRS$INTERMED, "L2.hector_gcam_inputs.csv") %>%
    read.csv %>%
    filter(variable %in% c(EMISSIONS_N2O())) ->
    other_global_emiss


DIRS$MAPPING %>%
    list.files(pattern = "M.hector_nongcam.csv",
               full.names = TRUE) %>%
    read.csv(comment.char = "#") ->
    mapping



# --- Define Helper Functions  ------------------------------------------------------

# Using observations of N2O concentrations and the anthropocentric N2O emissions
# back calculate the natural N2O emissions for Hector
# Args
#   n2o_conc: data frame of observations of global N2O concentrations
#   total_emiss: data frame of Hector's N2O_emissions
# Returns: data frame of the N2O natural emissions for Hector
get_natural_N2O <- function(n2o_conc, n2o_emiss){

    # Confirm that we are only working with the correct variables.
    stopifnot(unique(n2o_emiss$variable) == EMISSIONS_N2O())
    stopifnot(unique(n2o_conc$variable) == CONCENTRATIONS_N2O())

    # As defined in table S2 of Dorheim et al. 2024
    tau_0 <- 132
    N2O_conc_0 <- 273.87

    # Save the values for 1745 as the starting point.
    my_n2o_conc <- c(N2O_conc_0)
    my_tau      <- c(tau_0)

    total_E <- (N2O_conc_0/tau_0) * 4.8
    new_nat_n2o <- total_E - n2o_emiss$value[1]

    my_nat_n2o  <- c(new_nat_n2o)

    for(t in n2o_emiss$year[-1]){

        # Extract the information that we need in our calculation
        # for the present time step.
        antro_emiss <- n2o_emiss$value[n2o_emiss$year == t]
        lag_n2o <- my_n2o_conc[t-1745]
        current_n2o <- n2o_conc$value[n2o_conc$year == t]

        # Calculate the elements of the N2O concentration equation
        delta_n2o <- current_n2o - lag_n2o
        tau <- tau_0 * (lag_n2o/N2O_conc_0)^(-0.05)
        total_emiss <- 4.8 * (delta_n2o + lag_n2o/tau)
        new_nat_emiss <- total_emiss - antro_emiss

        # Make sure that the natural emissions strictly positive
        # if not then assume 0 natural emissions and update the
        # concentrations accordingly.
        if(new_nat_emiss < 0){
            update_delta_n2o <- antro_emiss/4.8 - lag_n2o/tau
            my_n2o_conc <- c(my_n2o_conc, lag_n2o + update_delta_n2o)
            my_nat_n2o <- c(my_nat_n2o, 0)
        } else {
            my_nat_n2o <- c(my_nat_n2o, new_nat_emiss)
            my_n2o_conc <- c(my_n2o_conc, lag_n2o + delta_n2o)
        }

    }


    data.frame(year = n2o_emiss$year,
               value = my_nat_n2o,
               variable = NAT_EMISSIONS_N2O(),
               units = getunits(NAT_EMISSIONS_N2O())) ->
        out

    return(out)

}

# 1. Main Chunk ----------------------------------------------------------------
# --- Aggregate global emissions -----------------------------------------------
# Aggregate to global emissions. Note that there are some additional
# variables that are missing that need to be handled individually.
L1_data %>%
    right_join(mapping,
               by = join_by("variable", "sector", "source"), relationship = "many-to-many") %>%
    summarise(value = sum(value), .by = c("hector_variable", "year")) %>%
    select(variable = hector_variable, year, value) %>%
    mutate(units = getunits(variable)) %>%
    extend_to_1745 ->
    global_total


# --- Natural N2O emissions ----------------------------------------------------
# Calculate the natural N2O emissions from the N2O concentration observations
# and the anthropogenic N2O emissions. Since N2O concentrations are independent
# of temperature and carbon cycle feedback unlike [CH4]. The natural CH4
# emissions must be calculated after the free Hector parameters are tuned.
n2o_conc  <- filter(conc_data, variable == CONCENTRATIONS_N2O())
n2o_antro_emiss <- filter(other_global_emiss, variable == EMISSIONS_N2O())

# Calculate natural N2O emissions
natural_n2o <- get_natural_N2O(n2o_conc, n2o_antro_emiss)

# Hold the future natural N2O emissions constant for the rest
# of the future period. This is the approach taken by other RCMs
# see FAIR v1.3 documentation (Smith et al. 2018).
natural_n2o %>%
    filter(year %in% 2009:2014) %>%
    pull(value) %>%
    mean ->
    future_value

natural_n2o %>%
    bind_rows(
        data.frame(year = (max(natural_n2o$year)+1):2300,
                   value = future_value,
                   variable = NAT_EMISSIONS_N2O(),
                   units = getunits(NAT_EMISSIONS_N2O()))) ->
    final_natural_n2o


if(CHECK){

    ini <- system.file(package = "hector", "input/hector_ssp245.ini")
    hc <- newcore(ini)
    setvar(hc, dates = n2o_emiss$year, var = EMISSIONS_N2O(), values = n2o_emiss$value, unit = getunits(EMISSIONS_N2O()))
    reset(hc)
    setvar(hc, dates = final_natural_n2o$year, var = NAT_EMISSIONS_N2O(), values = final_natural_n2o$value, unit = getunits(NAT_EMISSIONS_N2O()))
    reset(hc)
    run(hc)
    out <- fetchvars(hc, n2o_conc$year, vars = CONCENTRATIONS_N2O())

    out$value - n2o_conc$value

    ggplot() +
        geom_line(data = out, aes(year, value, color = "hector")) +
        geom_line(data = n2o_conc, aes(year, value, color = "obs"))


    out$value - n2o_conc$value
}



global_total %>%
    rbind(final_natural_n2o) ->
    global_total

# --- Natural CH4 Emissions ----------------------------------------------------

# Place holder for the natural CH4 emissions, they will be added post
# calibration but for now there needs to be some sort of value such
# that the hector_gcam.ini can run.
data.frame(variable = NATURAL_CH4(),
           year = unique(global_total$year),
           value = 300,
           units = getunits(NATURAL_CH4())) %>%
    rbind(global_total) ->
    global_total


# --- Base Period RF -----------------------------------------------------------

# All RF values during the base period must be set to 0, manually insure
# this here.
base_yrs <- 1745:1750

global_total %>%
    mutate(value = if_else(year %in% base_yrs & units == "W/m2", 0, value)) %>%
    na.omit %>%
    filter(year <= FINAL_FUT_YEAR) ->
    output


# 2. Save Output ---------------------------------------------------------------
# First check to make sure that all of the required variables are present
# and that there are no additional extra ones that have snuck in.
extra_emiss <- setdiff(global_total$variable, NON_GCAM_EMISS)
stopifnot(length(extra_emiss) == 0)

missing_vars <- setdiff(NON_GCAM_EMISS, global_total$variable)
stopifnot(length(missing_vars) == 0)


# Save a copy of the long format data frame for future reference.
output %>%
    check_req_names(req_cols = HEADERS$L2) %>%
    write.csv(file = file.path(DIRS$INTERMED, "L2.hector_nongcam_inputs.csv"),
              row.names = FALSE)

# Save the input table. This will also ensure that all the variables
# have all the data for all the years.

write_hector_csv(x = output, required = NON_GCAM_EMISS,
                 write_to = DIRS$INPUTS, save_as = "default_emissions.csv")


# Z. Quality Check -------------------------------------------------------------

if(CHECK){

    source("scripts/dev/hector_comp_data.R")

    output %>%
        filter(variable == RF_ALBEDO()) %>%
        ggplot(aes(year, value)) +
        geom_line()

}
