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
#   name: "no name" by default but will name the hector results if a different string is provided
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
#   name: "no name" by default but will name the hector results if a different string is provided
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


# 2. newcore helper functions --------------------------------------------------


# Z. Testings and Plots --------------------------------------------------------

YRS <- 1850:2020
VARS <- c(CONCENTRATIONS_CO2(), CONCENTRATIONS_N2O(), CONCENTRATIONS_CH4(),
          EMISSIONS_CH4(), EMISSIONS_N2O(), RF_TOTAL())

# The free running
ini <- system.file(package = "hector", "input/hector_ssp245.ini")
hc1 <- newcore(ini, name = "free running")
run(hc1)
out1 <- fetchvars(hc, YRS, VARS)


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







