# Comparison of GCAM Hector (new and old) with observations.

# 0. Set Up --------------------------------------------------------------------
source("scripts/constants.R")

# Loads the helper functions and the comparison data.
source("scripts/fxns_calibration.R")

library(ggplot2)
theme_set(theme_bw())

comparison_data %>%
    filter(year <= 2005) %>%
    filter(variable %in% c(CONCENTRATIONS_CO2(), "OHC", "gmst")) ->
    comp_data1

comparison_data %>%
    filter(variable %in% c(CONCENTRATIONS_CO2(), "OHC", "gmst")) ->
    comp_data2

# 1. Old GCAM-hector -----------------------------------------------------------
ini <- here::here("scripts/QAQC/old-inputs/hector-gcam.ini")
hc  <- newcore(ini, name = "old")
run(hc, runtodate = 2005)
old_out <- fetchvars_4comparison(hc = hc,
                                 comp = comp_data1)


# 2. New GCAM-hector -----------------------------------------------------------
ini <- here::here("inputs/hector-gcam.ini")
hc  <- newcore(ini, name = "new")
run(hc, runtodate = 2022)
new_out <- fetchvars_4comparison(hc = hc,
                                 comp = comp_data2)

# fetchvars(hc, dates = 1745:2022, vars = CONCENTRATIONS_CH4()) %>%
#     ggplot(aes(year, value)) +
#     geom_line()


# Z. Comparison Plots ----------------------------------------------------------

ggplot() +
    geom_line(data = comp_data2, aes(year, value), size = 1) +
    geom_line(data = old_out, aes(year, hector, color = scenario), size = .75) +
    geom_line(data = new_out, aes(year, hector, color = scenario), size = .75) +
    facet_wrap("variable", scales = "free", ncol = 1) +
    labs(y = NULL, x = NULL)
