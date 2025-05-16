# Description: Read in all the L1 data and aggregate to global totals
# required for the gcam hector input table.
#
# This script generates two files
#   (1) long format of the csv file that might be useful for debugging
#   (2) properly formatted input table
# TODO
#   How do we want to deal with the future period that is needed for the non GCAM emissions??
# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


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

DIRS$MAPPING %>%
    list.files(pattern = "M.hector_gcam.csv",
               full.names = TRUE) %>%
    read.csv(comment.char = "#") ->
    mapping


# 1. Main Chunk ----------------------------------------------------------------
# --- Aggregate global emissions -----------------------------------------------
# Aggregate to global emissions. Note that there are some additional
# variables that are missing that need to be handled individually.
L1_data %>%
    left_join(mapping,
              by = join_by("variable", "sector", "source")) %>%
    summarise(value = sum(value), .by = c("hector_variable", "year")) %>%
    select(variable = hector_variable, year, value) %>%
    mutate(units = getunits(variable)) %>%
    filter(year <= FINAL_HIST_YEAR) %>%
    extend_to_1745 ->
    global_total


# --- CO2 Emissions ------------------------------------------------------------
# There are some extra rules for the carbon cycle emissions
# 1. the Global Carbon Project emissions must be extended until 1745 but
#       this cannot be done with the regular extend_to_1745 since we need
#       to fill in emissions from 1745 to 1850.
# 2. they must be strictly positive and we need to add the update emissions
#       as well.
global_total %>%
    filter(variable == LUC_EMISSIONS()) %>%
    pull(year) ->
    luc_emissions_yrs

# Determine which years of data we are missing from the luc emissions
missing_yrs <- setdiff(1745:FINAL_HIST_YEAR, luc_emissions_yrs)

global_total %>%
    filter(variable == LUC_EMISSIONS()) ->
    incomplete_LUC_EMISS


# 1745 LUC EMISSIONS from the CMIP6 era Hector inputs.
start_value <- 0.0812
end_yr            <- max(missing_yrs)
end_value         <- incomplete_LUC_EMISS$value[incomplete_LUC_EMISS$year == end_yr+1]
missing_yr_length <- length(missing_yrs)

early_luc_emissions <- data.frame(x = c(1, missing_yr_length),
                                  y = c(start_value, end_value))

fit <- nls(data = early_luc_emissions, y ~ start_value * (1 + b)^x, start = list(b = 0.01))
extrapolated_vals <- predict(fit, newdata = data.frame(x = 1:missing_yr_length))

data.frame(year = missing_yrs,
                     variable = LUC_EMISSIONS(),
                     value = extrapolated_vals,
                     units = getunits(LUC_EMISSIONS())) ->
    missing_luc


# Add the missing luc emissions to the global total.
global_total %>%
    rbind(missing_luc) %>%
    arrange(variable, year) ->
    global_total


# Confirm that the LUC and FFI emission are strictly positive
global_total %>%
    filter(variable %in% c(LUC_EMISSIONS(), FFI_EMISSIONS())) %>%
    pull(value) ->
    vals

# TODO there is a more sophisticated way to handle this but
# for now if all the emissions are strictly positive we can then
# add in the uptake carbon variables.
stopifnot(all(vals > 1e-8))

n <- length(1745:FINAL_HIST_YEAR)
data.frame(year = rep(1745:FINAL_HIST_YEAR, each = 2),
           value = 0,
           variable = rep(c(DACCS_UPTAKE(), LUC_UPTAKE()), n/2)) %>%
    mutate(units = getunits(variable)) %>%
    arrange(variable, year) ->
    c_uptake

global_total %>%
    rbind(c_uptake) %>%
    na.omit ->
    global_total

output <- global_total

# 2. Save Output ---------------------------------------------------------------
# First check to make sure that all of the required variables are present
# and that there are no additional extra ones that have snuck in.
extra_emiss <- setdiff(global_total$variable, GCAM_EMISS)
stopifnot(length(extra_emiss) == 0)

missing_vars <- setdiff(GCAM_EMISS, global_total$variable)
stopifnot(length(missing_vars) == 0)

output %>%
    check_req_names(req_cols = HEADERS$L2) %>%
    write.csv(file = file.path(DIRS$INTERMED, "L2.hector_gcam_inputs.csv"),
              row.names = FALSE)

write_hector_csv(x = output, required = GCAM_EMISS,
                 write_to = DIRS$TABLES, save_as = "gcam_emissions.csv")

# Z. Quality Check -------------------------------------------------------------

if(FALSE){

    source("scripts/dev/hector_comp_data.R")

    # Compare new emissions vs. the older ones..
    em <- LUC_EMISSIONS()

    hector_comp %>%
        filter(variable == em) ->
        comp_to_plot

    output %>%
        filter(variable == em) ->
        to_plot

    ggplot() +
        geom_line(data = comp_to_plot, aes(year, value, color = "old")) +
        geom_line(data = to_plot, aes(year, value, color = "new")) +
        labs(title = em)



    hc <- newcore(system.file(package = "hector", "input/hector_ssp245.ini"))
    run(hc, runtodate = 2023)
    out1 <- fetchvars(hc, dates = 1745:2023, GLOBAL_TAS())


    setvar(hc, dates = to_plot$year,
           var = LUC_EMISSIONS(),
           values = to_plot$value,
           unit = getunits(LUC_EMISSIONS()))
    reset(hc)
    run(hc)
    out2 <- fetchvars(hc, dates = 1745:2023, GLOBAL_TAS())

    out1$scenario <- "old"
    out2$scenario <- "new"

    ggplot() +
        geom_line(data = out1, aes(year, value, color = scenario)) +
        geom_line(data = out2, aes(year, value, color = scenario)) +
        labs(title = paste0("Global TAS using difference LUC emissions"),
             y = "Temp Anomoly", x = NULL)


}


