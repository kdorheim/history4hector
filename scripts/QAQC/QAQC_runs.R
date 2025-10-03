# Hector runs that quantify the changes in Hector inputs and effect on runs.

# 0. Set Up --------------------------------------------------------------------
# Loads the helper functions and the comparison data.
source("scripts/constants.R")
source("scripts/fxns_calibration.R")

library(dplyr)
library(ggplot2)
theme_set(theme_bw())

BASE_DIR <- here::here()

FINAL_HIST_YEAR <- 2023
YRS <- 1750:FINAL_HIST_YEAR


# Single Var Runs --------------------------------------------------------------
# Change only one Hector variable at a time, use the final emissions even though
# they are formatted odd.
read.csv(here::here("inputs", "gcam_emissions.csv"), comment.char = ";") %>%
    pivot_longer(-Date, names_to = "variable") %>%
    rename(year = Date) ->
    d1

read.csv(here::here("inputs", "default_emissions.csv"), comment.char = ";") %>%
    pivot_longer(-Date, names_to = "variable") %>%
    rename(year = Date) ->
    d2

input_data <- rbind(d1, d2)

unique(input_data$variable) %>%
    lapply(function(var){

        input_data %>%
            filter(variable == var) ->
            use_this

        vars_to_keep <- c(GLOBAL_TAS(), RF_TOTAL(), var)

        if(grepl(pattern = "emissions", var)){

            if(var == EMISSIONS_CH4()){
                vars_to_keep <- c(vars_to_keep, "RF_CH4", CONCENTRATIONS_CH4())
            } else if(var %in% c(FFI_EMISSIONS(), DACCS_UPTAKE(), LUC_EMISSIONS(), LUC_UPTAKE())) {
                vars_to_keep <- c(vars_to_keep, RF_CO2(), NBP(), CONCENTRATIONS_CO2())
            } else if(var %in% c(EMISSIONS_CO(), EMISSIONS_NMVOC(), EMISSIONS_NOX(), NAT_EMISSIONS_N2O())) {

            } else {
                vars_to_keep <- c(vars_to_keep, paste0("RF_", gsub(x = var, pattern = "_emissions", replacement = "")))
            }

        }

        hc <- newcore(here::here("scripts", "QAQC", "old-inputs", "hector-gcam.ini"),
                      name = var)
        setvar(core = hc,
               dates = use_this$year,
               var = var,
               values = use_this$value,
               unit = getunits(var))
        reset(hc)
        run(hc, runtodate = 2005)
        out <- fetchvars(hc, dates = 1750:2005, vars = vars_to_keep)
        return(out)
    }) %>%
    do.call(what = "rbind")  ->
    rslts

# Run the old hector-gcam historical
hc <- newcore(here::here("scripts", "QAQC", "old-inputs", "hector-gcam.ini"),
              name = "old")
run(hc, 2005)
fetchvars(hc, dates = 1750:2005, vars = unique(rslts$variable)) %>%
    select(year, variable, old = value)  ->
    out

rslts %>%
    left_join(out, by = join_by(year, variable), relationship = "many-to-many") %>%
    na.omit %>%
    select(scenario, year, variable, value, old, units) %>%
    mutate(SE = (value - old)^2) ->
    output


fname <- file.path(BASE_DIR, "scripts", "QAQC", "single_variable_runs.csv")
write.csv(output, file = fname, row.names = FALSE)


