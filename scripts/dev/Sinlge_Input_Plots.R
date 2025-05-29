# QAQC Continued
# Make nice plots of the sinlge variable runs


# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(hector)
theme_set(theme_bw())

BASE_DIR <- here::here()
PLOTS_DIR <- file.path(BASE_DIR, "scripts", "dev", "new_figs")
dir.create(PLOTS_DIR, recursive = TRUE, showWarnings = FALSE)

FINAL_HIST_YEAR <- 2022
YRS <- 1750:FINAL_HIST_YEAR


# 1. Prep Data -----------------------------------------------------------------

fname <- file.path(BASE_DIR, "data", "dev", "single_variable_runs.csv")
rslts <- read.csv(fname)


fname <- file.path(BASE_DIR, "data", "dev", "full_hector_runs.csv")
full_hector_runs <- read.csv(fname)

full_hector_runs %>%
    filter(scenario == "old gcam set up") ->
    old_gcam


rslts %>%
    rename(new_value = value) %>%
    left_join(old_gcam %>%
                  select(year, variable, value), by = join_by(year, variable)) %>%
    mutate(SE = (value - new_value)^2) %>%
    na.omit() %>%
    summarise(MSE = mean(SE), .by = c("scenario", "variable")) %>%
    arrange(desc(MSE)) ->
    MSE_table


MSE_table %>%
    filter(variable == GLOBAL_TAS()) %>%
    arrange(desc(MSE))

# 2. Make Plots -----------------------------------------------------------------


scns <- unique(rslts$scenario)
#scns <- scns[!grepl(pattern = "HFC|CFC|halo|HFC365_emissions|C2F6|CF4|SF6_", x = scns)]
#scn <- "luc_emissions"

lapply(scns, function(scn){

    print(scn)
    vars_to_plot <- c(scn, RF_TOTAL(), GLOBAL_TAS())

    rslts %>%
        filter(scenario == scn) %>%
        filter(variable %in% vars_to_plot) %>%
        filter(year <= 2030) %>%
        mutate(variable = factor(variable, levels = vars_to_plot, ordered = TRUE)) ->
        single_emiss_to_plot

    old_gcam %>%
        filter(variable %in% vars_to_plot) %>%
        mutate(variable = factor(variable, levels = vars_to_plot, ordered = TRUE)) ->
        default_to_plot

    MSE_table %>%
        filter(scenario == scn) %>%
        filter(variable %in% vars_to_plot) %>%
        mutate(MSE = signif(MSE, digits = 3)) %>%
        mutate(variable = factor(variable, levels = vars_to_plot, ordered = TRUE)) ->
        tb

    tbs <- lapply(split(tb, tb$variable), "[", -1)

    df <- tibble(x = rep(-Inf, length(tbs)),
                 y = rep(Inf, length(tbs)),
                 variable = factor(vars_to_plot, levels = vars_to_plot, ordered = TRUE),
                 tbl = tbs)

    ggplot() +
        geom_line(data = default_to_plot, aes(year, value, color = "default")) +
        geom_line(data = single_emiss_to_plot, aes(year, value, color = "NEW")) +
        facet_wrap("variable", scales = "free") +
        scale_color_manual(values = c("default" = "darkgrey", NEW = "red")) +
        labs(y = NULL, x = NULL) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        labs(title = paste0("GCAM + new ", scn, " only")) +
        geom_table(data = df, aes(x = x, y = y, label = tbl),
                   hjust = 0, vjust = 1) ->
        plot; plot
    fname <- file.path(PLOTS_DIR, paste0(scn, "_only.png"))
    ggsave(plot, filename = fname, width = 10, height = 5.5)

})




