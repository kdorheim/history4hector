# First thing we are going to want to do is asses how the hector results
# compare with observations.

# 0. Set Up -------------------------------------------------------------------

source("scripts/constants.R")
source("scripts/fxns_calibration.R")

# Load the packages and set some helpful plotting aesthetics.
library(ggpmisc)
library(ggplot2)
theme_set(theme_bw())
library(scales) # the package that is used to emulate the standard ggplot2 color scheme
WIDTH <- 10
HEIGHT <- 8


FIGS_DIR <- file.path("scripts", "QAQC", "figs", "obs_comparisons")
dir.create(FIGS_DIR, showWarnings = FALSE, recursive = TRUE)


# Load the comparison data
here::here("scripts/QAQC/data") %>%
    list.files(pattern = "gcamV8.8hectorV3.2_obs_comparison.csv", full.names = TRUE) %>%
    read.csv() ->
    old_hector_obs_out



# 1. Run hector ----------------------------------------------------------------

ini <- here::here("inputs/hector-gcam.ini")
hc  <- newcore(ini, name = "new")
run(hc, runtodate = 2023)
fetchvars_4comparison(hc = hc,
                      comp = comparison_data) %>%
    mutate(source = "hectorV3.5.X") ->
    new_hector_out


# 2. Define Consistent Color Pallet -------------------------------------------
hector_data_obs_comp <- rbind(new_hector_out, old_hector_obs_out)

# Assign each hector output version a different color.
hector_ids <- unique(hector_data_obs_comp$source)
COLOR_SCHEME <- scales::hue_pal()(length(hector_ids))
names(COLOR_SCHEME) <-  hector_ids

# Add black for the observations
COLOR_SCHEME <- c(COLOR_SCHEME, "obs" = "black")

# 2. Compare with observations -------------------------------------------------


# For each of the variables let's take a look a the RMSE & also a plot looking
# at the differences. This will make a plot for each variable.
for(VAR in unique(new_hector_out$variable)){
    print(VAR)
    comparison_data %>%
        filter(variable == VAR) ->
        comp_to_plot

    hector_data_obs_comp %>%
        filter(variable == VAR) %>%
        filter(year %in% ((min(comp_to_plot$year)-20):(max(comp_to_plot$year)+20))) ->
        hector_to_plot

    # Now calculate the RMSE between the observations and the difference hector versions,
    # this comparison will need to stop at 2005 aka the older version of gcam's
    # transition from historical to future emissions.
    hector_to_plot %>%
        left_join(comp_to_plot %>%
                      select(year, variable, value)) %>%
        select(year, variable, hector, source, value) %>%
        na.omit %>%
        filter(year <= 2005) %>%
        summarise(RMSE = sqrt(mean((hector - value) ^ 2)), .by = c(variable, source)) %>%
        mutate(RMSE = signif(RMSE, digits = 3)) ->
        tb

    # Format the table so that it can be included on the figure
    tbs <- lapply(split(tb, tb$variable), "[", -1)

    df <- tibble(x = rep(-Inf, length(tbs)),
                 y = rep(Inf, length(tbs)),
                 source = tb$source,
                 tbl = tbs)

    ggplot() +
        geom_line(data = comp_to_plot, aes(year, value, color = "obs")) +
        geom_ribbon(data = comp_to_plot, aes(year, ymin = lower, ymax = upper, fill = "obs"), alpha = 0.5) +
        geom_line(data = hector_to_plot, aes(year, hector, color = source)) +
        scale_color_manual(values = COLOR_SCHEME) +
        scale_fill_manual(values = COLOR_SCHEME) +
        labs(title = VAR, y = hector_data_obs_comp$units[1]) +
        theme(legend.title = element_blank(), legend.position = "bottom") +
        geom_table(data = df, aes(x = x, y = y, label = tbl),
                   hjust = 0, vjust = 1)

    ggsave(filename = file.path(FIGS_DIR, paste0(VAR, "_obs.png")), width = WIDTH, height = HEIGHT)

}









