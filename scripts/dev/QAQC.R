# Load required packages
library(dplyr)
library(ggplot2)
library(hector)
theme_set(theme_bw())

BASE_DIR <- here::here()

FINAL_HIST_YEAR <- 2022
YRS <- 1750:FINAL_HIST_YEAR

# Full Hector Runs -------------------------------------------------------------
# The variables to save.
vars <- c("BC_emissions", "CH4_emissions", "CO_emissions", "ffi_emissions",
          "N2O_emissions", "NH3_emissions", "NMVOC_emissions", "NOX_emissions",
          "OC_emissions", "SO2_emissions", "RF_tot", "RF_CO2",
          "RF_N2O", "RF_H2O_strat", "RF_O3_trop", "RF_BC", "RF_OC", "FCH4",
          "RF_NH3", "RF_SO2", "RF_aci", "global_tas", "O3_concentration",
          "CO2_concentration", "N2O_concentration", "CH4_concentration", NBP(), NPP(), VEG_C())

vars <- c(vars, GLOBAL_TAS(), RF_TOTAL())
# Run default hector
ini <- system.file(package = "hector", "input/hector_ssp245.ini")
hc  <- newcore(ini, name  ="default hector")
run(hc, runtodate = FINAL_HIST_YEAR)
out1 <- fetchvars(hc, dates = YRS, vars = vars)


# Run old gcam set up
ini <- file.path(BASE_DIR, "data", "dev", "hector-gcam.ini")
hc  <- newcore(ini, name  ="old gcam set up")
run(hc, runtodate = 2005)
out3 <- fetchvars(hc, dates = YRS, vars = vars)


# Run hector with with new gcam set up
ini <- file.path(BASE_DIR, "inputs", "hector-gcam.ini")
hc  <- newcore(ini, name = "new hist.")
run(hc, runtodate = FINAL_HIST_YEAR)
out2 <- fetchvars(hc, dates = YRS, vars = vars)

hector_runs <- rbind(out1, out2, out3)

fname <- file.path(BASE_DIR, "data", "dev", "full_hector_runs.csv")
write.csv(hector_runs, file = fname, row.names = FALSE)


# Single Var Runs --------------------------------------------------------------
# Change only one Hector variable at a time.

d1 <- read.csv(file.path(BASE_DIR, "data", "intermed",
                         "L2.hector_gcam_inputs.csv"))
d2 <- read.csv(file.path(BASE_DIR, "data", "intermed",
                         "L2.hector_nongcam_inputs.csv"))
input_data <- rbind(d1, d2)


lapply(unique(input_data$variable), function(var){

    input_data %>%
        filter(variable == var) ->
        use_this

    vars_to_keep <- c(GLOBAL_TAS(), RF_TOTAL(), var)

    if(grepl(pattern = "emissions", var)){

        if(var == EMISSIONS_CH4()){
            vars_to_keep <- c(vars_to_keep, "FCH4", CONCENTRATIONS_CH4())
        } else if(var %in% c(FFI_EMISSIONS(), DACCS_UPTAKE(), LUC_EMISSIONS(), LUC_UPTAKE())) {
            vars_to_keep <- c(vars_to_keep, RF_CO2(), NBP(), CONCENTRATIONS_CO2())
        } else if(var %in% c(EMISSIONS_CO(), EMISSIONS_NMVOC(), EMISSIONS_NOX(), NAT_EMISSIONS_N2O())) {

        } else {
            vars_to_keep <- c(vars_to_keep, paste0("RF_", gsub(x = var, pattern = "_emissions", replacement = "")))
        }

    }

    hc <- newcore(system.file(package = "hector", "input/hector_ssp245.ini"),
                  name = var)
    setvar(core = hc, dates = use_this$year,
           var = var, values = use_this$value,
           unit = getunits(var))
    reset(hc)
    run(hc)
    out <- fetchvars(hc, dates = 1750:2100, vars = vars_to_keep)
    return(out)
}) %>%
    do.call(what = "rbind") ->
    rslts


# Run default SSP245 Hector - which will be a comparison with the single
# variable runs
hc <- newcore(system.file(package = "hector", "input/hector_ssp245.ini"),
              name = "default")
run(hc)
fetchvars(hc, dates = 1750:2100, vars = unique(rslts$variable)) %>%
    select(year, variable, default = value) ->
    out

rslts %>%
    left_join(out, by = join_by(year, variable)) %>%
    select(scenario, year, variable, value, default, units) %>%
    mutate(abs_diff = abs(value - default)) ->
    output

fname <- file.path(BASE_DIR, "data", "dev", "single_variable_runs.csv")
write.csv(output, file = fname, row.names = FALSE)

output %>%
    filter(variable == GLOBAL_TAS()) %>%
    filter(year <= 2022) %>%
    summarise(mean_abs_dif = mean(abs_diff), .by = "scenario") %>%
    arrange(desc(mean_abs_dif))

# The largest changes in the global temp came from the changes in
# RF_albedo & SV the rest are much smaller




