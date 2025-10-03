# Make the plots related to the QAQC runs

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(hector)
theme_set(theme_bw())
source("scripts/constants.R")
source("scripts/fxns_calibration.R")


BASE_DIR <- here::here()
PLOTS_DIR <- file.path(BASE_DIR, "scripts", "QAQC", "figs")
dir.create(PLOTS_DIR, recursive = TRUE, showWarnings = FALSE)

FINAL_HIST_YEAR <- 2023
YRS <- 1750:FINAL_HIST_YEAR


# 1. Prep Data -----------------------------------------------------------------

fname <- file.path(BASE_DIR, "scripts", "QAQC", "single_variable_runs.csv")
rslts <- read.csv(fname)

rslts %>%
    rename(new_value = value) %>%
    summarise(MSE = mean(SE), .by = c("scenario", "variable")) %>%
    arrange(desc(MSE)) ->
    MSE_table


MSE_table %>%
    filter(variable == RF_TOTAL()) %>%
    mutate(RMSE = sqrt(round(MSE, 8))) %>%
    select(variable, input = scenario, RMSE) %>%
    write.csv("diff.csv", row.names = FALSE)




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

# Load the historical data
list("./scripts/QAQC/old_obs_comp.csv",
     "./scripts/QAQC/new_obs_comp.csv") %>%
    lapply(read.csv) %>%
    bind_rows ->
    hector_out

# Load the comparison data
comparison_data %>%
    filter(year <= 2022) %>%
    filter(variable %in% c(CONCENTRATIONS_CO2(), "OHC", "gmst")) %>%
    select(year, obs = value, variable) ->
    comp_data

hector_out %>%
    inner_join(comp_data) ->
    wide_df

wide_df %>%
    summarise(MAE = mean(abs(obs - hector)), .by = c(scenario, variable)) %>%
    mutate(MAE = signif(MAE, 2)) ->
    tb


tbs <- lapply(split(tb, tb$variable), "[", -2)

df <- tibble(x = rep(-Inf, length(tbs)),
             y = rep(Inf, length(tbs)),
             variable = names(tbs),
             tbl = tbs)


wide_df %>%
    ggplot() +
    geom_line(aes(year, obs)) +
    geom_line(aes(year, hector, color = scenario)) +
    facet_wrap("variable", scales = "free", ncol = 1) +
    labs(x = NULL, y = NULL) +
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    geom_table(data = df, aes(x = x, y = y, label = tbl),
               hjust = 0, vjust = 1) ->
    plot; plot

fname <- file.path(PLOTS_DIR, "calibration_plot.png")
ggsave(plot, filename = fname, width = 8, height = 6)




