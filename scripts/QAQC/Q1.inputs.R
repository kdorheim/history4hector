# Let's take a look at how much the inputs have changed.

# 0. Set Up -------------------------------------------------------------------
#
# source("scripts/constants.R")
# source("scripts/fxns_calibration.R")

# Load the packages and set some helpful plotting aesthetics.
library(magrittr)
library(tidyr)
library(dplyr)
library(ggpmisc)
library(hector)
library(ggplot2)
theme_set(theme_bw())
library(scales) # the package that is used to emulate the standard ggplot2 color scheme
WIDTH <- 10
HEIGHT <- 8


WRITE_TO <- file.path("scripts", "QAQC", "figs", "inputs")
dir.create(WRITE_TO, showWarnings = FALSE, recursive = TRUE)



# 1. Load Data -----------------------------------------------------------------
# Load the inputs
list.files(here::here("scripts", "QAQC", "data"),
           pattern = "input-", full.names = TRUE) %>%
    lapply(read.csv) %>%
    bind_rows() ->
    inputs_df

# Assign a unique color to all of the different inputs, really it should
# just be a comparison of two...
ids <- unique(inputs_df$source)
COLOR_SCHEME <- scales::hue_pal()(length(ids))
names(COLOR_SCHEME) <-  ids

# 2. Comparison of the non GCAM inputs -----------------------------------------

inputs_df  %>%
    filter(file == "default_inputs") %>%
    pivot_wider(names_from = source, values_from = value) %>%
    na.omit %>%
    mutate(SE = (DEV - `gcam8.8-hectorv3.2`)^2) %>%
    summarise(RMSE = sqrt(mean(SE)), .by = "variable") %>%
    arrange(desc(RMSE)) ->
    RMSE_default_inputs

write.csv(RMSE_default_inputs, file = file.path(WRITE_TO, "RMSE_default_inputs.csv"), row.names = FALSE)
# There should be 0 change in the emissions however with the forcing inputs we
# that might not be the case here since one of the corrections was in how
# the forcing inputs are being normalized.

# There might be some variables (forcing) worth checking out...
VARS <- c("SV", "RF_albedo")

for(v in VARS){

    RMSE_default_inputs %>%
        filter(variable == v) ->
        this_RMSE

    inputs_df %>%
        filter(variable == v) ->
        to_plot

    to_plot %>%
        ggplot(aes(year, value, color = source)) +
        geom_line() +
        labs(title = paste0(v, "  input comparison"),
             y = getunits(v),
             caption = paste0("RMSE: ", this_RMSE$RMSE)) +
        scale_color_manual(values = COLOR_SCHEME)

    ggsave(filename = file.path(WRITE_TO, paste0(v, "-inputs.png")), width = WIDTH, height = HEIGHT)

}


# 3. Comparison of the GCAM inputs ---------------------------------------------

inputs_df  %>%
    filter(file == "gcam_inputs") %>%
    pivot_wider(names_from = source, values_from = value) %>%
    na.omit %>%
    mutate(SE = (DEV - `gcam8.8-hectorv3.2`)^2) %>%
    summarise(RMSE = sqrt(mean(SE)), .by = "variable") %>%
    arrange(desc(RMSE)) ->
    RMSE_default_inputs

write.csv(RMSE_default_inputs, file = file.path(WRITE_TO, "RMSE_gcam_inputs.csv"), row.names = FALSE)


# There might be some variables (forcing) worth checking out...
VARS <- RMSE_default_inputs$variable

for(v in VARS){

    RMSE_default_inputs %>%
        filter(variable == v) ->
        this_RMSE

    inputs_df %>%
        filter(variable == v) ->
        to_plot

    to_plot %>%
        ggplot(aes(year, value, color = source)) +
        geom_line() +
        labs(title = paste0(v, "  input comparison"),
             y = getunits(v),
             caption = paste0("RMSE: ", this_RMSE$RMSE)) +
        scale_color_manual(values = COLOR_SCHEME)

    ggsave(filename = file.path(WRITE_TO, paste0(v, "-inputs.png")), width = WIDTH, height = HEIGHT)

}



# 4. Natural Emissions ---------------------------------------------------------

# So one of the big changes between this past hector integration and the udpate
# is that we now have time varying natural emissions for CH4 and N2O during the
# historical period. Quickly plot the differences here...

# Manually create the data frame of the older results...
bind_rows(
    data.frame(variable = NATURAL_CH4(),
               value = 335, # pulled directly from the old gcam ini file
               year = unique(inputs_df$year),
               source =  "gcam8.8-hectorv3.2"),
    data.frame(variable = NAT_EMISSIONS_N2O(),
               value = 9.7, # pulled directly from the old gcam ini file
               year = unique(inputs_df$year),
               source =  "gcam8.8-hectorv3.2"),
    inputs_df) %>%
    filter(variable %in% c(NATURAL_CH4(), NAT_EMISSIONS_N2O())) %>%
    select(-file) ->
    natural_emissions_df

natural_emissions_df  %>%
    pivot_wider(names_from = source, values_from = value) %>%
    na.omit %>%
    mutate(SE = (DEV - `gcam8.8-hectorv3.2`)^2) %>%
    summarise(RMSE = sqrt(mean(SE)), .by = "variable") %>%
    arrange(desc(RMSE)) ->
    RMSE_natemiss_inputs

write.csv(RMSE_natemiss_inputs, file = file.path(WRITE_TO, "RMSE_natemiss_inputs.csv"), row.names = FALSE)



# There might be some variables (forcing) worth checking out...
VARS <- unique(natural_emissions_df$variable)

for(v in VARS){

    RMSE_natemiss_inputs %>%
        filter(variable == v) ->
        this_RMSE

    natural_emissions_df %>%
        filter(variable == v) ->
        to_plot

    to_plot %>%
        ggplot(aes(year, value, color = source)) +
        geom_line() +
        labs(title = paste0(v, "  input comparison"),
             y = getunits(v),
             caption = paste0("RMSE: ", this_RMSE$RMSE)) +
        scale_color_manual(values = COLOR_SCHEME)

    ggsave(filename = file.path(WRITE_TO, paste0(v, "-inputs.png")), width = WIDTH, height = HEIGHT)

}



