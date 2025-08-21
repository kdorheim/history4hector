# Make the plots related to the QAQC runs

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(hector)
theme_set(theme_bw())

BASE_DIR <- here::here()
PLOTS_DIR <- file.path(BASE_DIR, "scripts", "QAQC", "figs")
dir.create(PLOTS_DIR, recursive = TRUE, showWarnings = FALSE)

FINAL_HIST_YEAR <- 2022
YRS <- 1750:FINAL_HIST_YEAR


# 1. Prep Data -----------------------------------------------------------------

fname <- file.path(BASE_DIR, "scripts", "QAQC", "single_variable_runs.csv")
rslts <- read.csv(fname)

rslts %>%
    rename(new_value = value) %>%
    summarise(MSE = mean(SE), .by = c("scenario", "variable")) %>%
    arrange(desc(MSE)) ->
    MSE_table

# 2. Make Single Var Plots -----------------------------------------------------

# Each scenario here corresponds to a run where a single input is varied.
scns <- unique(rslts$scenario)

lapply(scns, function(scn){

    print(scn)
    vars_to_plot <- c(scn, RF_TOTAL(), GLOBAL_TAS())

    rslts %>%
        filter(scenario == scn) %>%
        filter(variable %in% vars_to_plot) %>%
        filter(year <= 2030) %>%
        mutate(variable = factor(variable, levels = vars_to_plot, ordered = TRUE)) ->
        single_emiss_to_plot

    MSE_table %>%
        filter(scenario == scn) %>%
        filter(variable %in% vars_to_plot) %>%
        mutate(MSE = signif(MSE, digits = 3)) %>%
        mutate(variable = factor(variable, levels = vars_to_plot, ordered = TRUE)) ->
        tb

    tbs <- lapply(split(tb, tb$variable), "[", -1)

    df <- tibble(x = rep(Inf, length(tbs)),
                 y = rep(-Inf, length(tbs)),
                 variable = factor(vars_to_plot, levels = vars_to_plot, ordered = TRUE),
                 tbl = tbs)

    ggplot() +
        geom_line(data = single_emiss_to_plot, aes(year, old, color = "old")) +
        geom_line(data = single_emiss_to_plot, aes(year, value, color = "NEW")) +
        facet_wrap("variable", scales = "free") +
        scale_color_manual(values = c("old" = "darkgrey", NEW = "red")) +
        labs(y = NULL, x = NULL) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        labs(title = paste0("old gcam inputs + new ", scn, " only")) +
        geom_table(data = df, aes(x = x, y = y, label = tbl),
                 hjust = 1, vjust = 0) ->
        plot; plot
    fname <- file.path(PLOTS_DIR, paste0(scn, "_only.png"))
    ggsave(plot, filename = fname, width = 10, height = 5.5)

})


# 3. Hector vs. Obs ------------------------------------------------------------

# Load the comparison data
comparison_data %>%
    filter(year <= 2005) %>%
    filter(variable %in% c(CONCENTRATIONS_CO2(), "OHC", "gmst")) ->
    comp_data1

comparison_data %>%
    filter(variable %in% c(CONCENTRATIONS_CO2(), "OHC", "gmst")) ->
    comp_data2

carbon_vars <- c(NPP(), VEG_C(), OCEAN_C(), SOIL_C())

# Run Hector
ini <- here::here("scripts/QAQC/old-inputs/hector-gcam.ini")
hc  <- newcore(ini, name = "old")
run(hc, runtodate = 2005)
old_out <- fetchvars_4comparison(hc = hc,
                                 comp = comp_data1)
old_carbon <- fetchvars(hc, 1850:2005, vars = carbon_vars)

ini <- here::here("inputs/hector-gcam.ini")
hc  <- newcore(ini, name = "new")
run(hc, runtodate = 2022)
new_out <- fetchvars_4comparison(hc = hc,
                                 comp = comp_data2)
new_carbon <- fetchvars(hc, 1850:2005, vars = carbon_vars)


ggplot() +
    geom_line(data = comp_data2, aes(year, value), size = 1) +
    geom_line(data = old_out, aes(year, hector, color = scenario), size = .75) +
    geom_line(data = new_out, aes(year, hector, color = scenario), size = .75, alpha = 0.5) +
    facet_wrap("variable", scales = "free", ncol = 1) +
    scale_color_manual(values = c("old" = "darkgrey", new = "red")) +
    coord_cartesian(xlim = c(1850, 2022)) +
    labs(y = NULL, x = NULL)

ggplot() +
    geom_line(data = old_carbon, aes(year, value, color = scenario), size = .75) +
    geom_line(data = new_carbon, aes(year, value, color = scenario), size = .75, alpha = 0.5) +
    facet_wrap("variable", scales = "free", ncol = 1) +
    scale_color_manual(values = c("old" = "darkgrey", new = "red")) +
    coord_cartesian(xlim = c(1850, 2022)) +
    labs(y = NULL, x = NULL)

