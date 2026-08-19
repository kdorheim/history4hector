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


FIGS_DIR <- file.path("scripts", "QAQC", "figs")
dir.create(FIGS_DIR, showWarnings = FALSE, recursive = TRUE)


# 1. Import Data  --------------------------------------------------------------

# Load the comparison data
here::here("scripts/QAQC/data") %>%
    list.files(pattern = "obs_comparison-", full.names = TRUE) %>%
    lapply(read.csv) %>%
    bind_rows ->
    hector_data_obs_comp


# 2. Compare with Observations -------------------------------------------------

# Assign each hector output version a different color.
hector_ids <- unique(hector_data_obs_comp$source)
COLOR_SCHEME <- scales::hue_pal()(length(hector_ids))
names(COLOR_SCHEME) <-  hector_ids

# Add black for the observations
COLOR_SCHEME <- c(COLOR_SCHEME, "obs" = "black")

# For each of the variables let's take a look a the RMSE & also a plot looking
# at the differences. This will make a plot for each variable.
for(VAR in unique(hector_data_obs_comp$variable)){
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
        labs(title = VAR, y = hector_to_plot$units[1]) +
        theme(legend.title = element_blank(), legend.position = "bottom") +
        geom_table(data = df, aes(x = x, y = y, label = tbl),
                   hjust = 0, vjust = 1)

    ggsave(filename = file.path(FIGS_DIR, "obs_comparisons", paste0(VAR, "_obs.png")), width = WIDTH, height = HEIGHT)

}


# 3. AR6 Benchmarking ----------------------------------------------------------
### 3A. Load & Format Data -----------------------------------------------------

VARS <- c("OHC", "RF_CH4", "wmghg RF",  "total aerosol RF", "hist. warming")


# Load the hector results
here::here("scripts/QAQC/data") %>%
    list.files(pattern = "AR6benchmarks-", full.names = TRUE) %>%
    lapply(read.csv) %>%
    bind_rows %>%
    filter(variable %in% VARS) ->
    hector_data_AR6benchmarks

# Load the AR6 results, this includes the mean and the individual SCM results
# AR6 scm benchmark results
"scripts/QAQC/raw-data/ar6_scms.csv" %>%
    read.csv %>%
    filter(variable %in% VARS) ->
    ar6_scm_benchmarks

"scripts/QAQC/raw-data/ar6.csv" %>%
    read.csv %>%
    filter(variable %in% VARS) ->
    ar6_ipcc_benchmarks


### 3B. Color Scheme  ----------------------------------------------------------

# Subset the color scheme to include only the hector versions we are going to be
# comparing in the AR6 benchmark metrics
COLOR_SCHEME <- COLOR_SCHEME[names(COLOR_SCHEME) %in% hector_data_AR6benchmarks$source]

# Add the color codes for the AR6 sources of information
COLOR_SCHEME <- c(COLOR_SCHEME,"ipcc ar6" = "black", "scms from ar6" = "grey")


# Additional aesthetics to control for this section
JW <- 0.4

### 3C. Plots!  ----------------------------------------------------------------

# Plot all of the variables at once!
ggplot() +
    geom_errorbar(data = ar6_ipcc_benchmarks,
                  aes(variable, ymin = min, ymax = max),
                  width=.2, alpha = 0.85) +
    geom_point(data = hector_data_AR6benchmarks, aes(variable, value, color = source),
               position = position_jitter(height = 0, width = 0), alpha = 0.85) +
    geom_point(data = ar6_scm_benchmarks, aes(variable, value, color = "scms from ar6"),
               shape = 4, position = position_jitter(height = 0, width = JW)) +
    geom_point(data = ar6_ipcc_benchmarks, aes(variable, value, color = "ipcc ar6"), shape = 4) +
    labs(y = NULL, x = NULL, title = "Hist. Benchmarks Metrics") +
    facet_wrap("variable", scales = "free") +
    theme(legend.title = element_blank()) +
    scale_color_manual(values = COLOR_SCHEME) ->
    plot; plot

ggsave(filename = file.path(FIGS_DIR, "ar6_benchmarks", "all_hist_metrics.png"), width = WIDTH, height = HEIGHT)


# Plot each metric individually
for(V in VARS){
    ggplot() +
        geom_errorbar(data = ar6_ipcc_benchmarks %>%
                          filter(variable == V),
                      aes(variable, ymin = min, ymax = max),
                      width=.5, alpha = 0.85) +
        geom_point(data = hector_data_AR6benchmarks  %>%
                       filter(variable == V), aes(variable, value, color = source),
                   position = position_jitter(height = 0, width = 0), alpha = 0.85, size = 2) +
        geom_point(data = ar6_scm_benchmarks  %>%
                       filter(variable == V), aes(variable, value, color = "scms from ar6"),
                   shape = 4, position = position_jitter(height = 0, width = JW), size = 2) +
        geom_point(data = ar6_ipcc_benchmarks %>%
                       filter(variable == V), aes(variable, value, color = "ipcc ar6"), shape = 4, size = 2) +
        labs(y = NULL, x = NULL, title = paste0("Hist. Benchmark for ", V)) +
        facet_wrap("variable", scales = "free") +
        theme(legend.title = element_blank()) +
        labs(y = ar6_ipcc_benchmarks$units[ar6_ipcc_benchmarks$variable == V],
             caption = ar6_ipcc_benchmarks$year[ar6_ipcc_benchmarks$variable == V]) +
        scale_color_manual(values = COLOR_SCHEME) ->
        plot; plot

    ggsave(filename = file.path(FIGS_DIR, "ar6_benchmarks", paste0(V, "_hist_metrics.png")), width = WIDTH, height = HEIGHT)

}


# 4. AWGP CO2 100 --------------------------------------------------------------
### 4A. Format Benchmark Data --------------------------------------------------

# The benchmark value
conversion <- 1/1e-9
zieger <- 8.95e-14 * conversion
uncert <- 1.3e-14 * conversion

### 4B. Hector Data ------------------------------------------------------------

# Load the hector results
here::here("scripts/QAQC/data") %>%
    list.files(pattern = "CO2AGWP100", full.names = TRUE) %>%
    lapply(read.csv) %>%
    bind_rows ->
    hector_data_CO2AGWP100


### 4C. COLOR SCHEME -----------------------------------------------------------

# Subset the color scheme to include only the hector versions we are going to be
# comparing in the AR6 benchmark metrics
COLOR_SCHEME <- COLOR_SCHEME[names(COLOR_SCHEME) %in% hector_data_CO2AGWP100$source]

# Add the color codes for the AR6 sources of information
COLOR_SCHEME <- c(COLOR_SCHEME, "zieger" = "grey")



### 4D. Plot -------------------------------------------------------------------
ggplot() +
    annotate("rect",
             xmin = -Inf, xmax = Inf, # Extends across the entire x-axis
             ymin = zieger - uncert,
             ymax = zieger + uncert,
             alpha = 0.85, # Transparency level
             fill = "grey") + # Color of the shaded area
    geom_hline(yintercept = zieger) +
    geom_point(data = hector_data_CO2AGWP100, aes(variable, value, color = source)) +
    scale_color_manual(values = COLOR_SCHEME) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    labs(x = NULL, y = "AWGP CO2 100", caption = "older hector results not avaiable") ->
    plot;plot

ggsave(filename = file.path(FIGS_DIR, "AWGP", "AWGPCO2100.png"), width = WIDTH, height = HEIGHT)
