# A place for quick data vis stuff

# 0. Set Up --------------------------------------------------------------------

library(ggplot2)
library(dplyr)

source("scripts/constants.R")


# 1. Checking Temp -------------------------------------------------------------

DIRS$CALIBRATION_DATA %>%
    list.files("C.gmst_data.csv", full.names = TRUE) %>%
    read.csv ->
    temp


    ggplot() +
    geom_line(data = temp, aes(year, value)) +
        geom_ribbon(data = temp, aes(year, ymin = lower, ymax = upper), alpha = 0.5) +
        coord_cartesian(xlim = c(2020, 2025))
