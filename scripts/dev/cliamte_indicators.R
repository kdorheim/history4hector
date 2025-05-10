# Let's compare climate indicators with rcmip inputs.

library(dplyr)
library(ggplot2)



f <- "data/dev/ClimateIndicator-data-9612b1d/data/greenhouse_gas_concentrations/ghg_concentrations.csv"
read.csv(f) %>%
    select(year = time, value = CH4) %>%
    mutate(year = floor(year)) ->
    climate_indicators


"data/dev/rcmip-concentrations-annual-means-v5-1-0.csv" %>%
    read.csv %>%
    filter(Region == "World") %>%
    filter(Variable == "Atmospheric Concentrations|CH4") %>%
    pivot_longer(starts_with("X"), names_to = "year") %>%
    na.omit %>%
    mutate(year = as.integer(gsub(replacement = "",
                                  x = year,
                                  pattern = "X"))) %>%
    filter(Scenario == "historical") ->
    rcmip

rcmip %>%
    filter(Variable == "Atmospheric Concentrations|CH4") ->
xx


ggplot() +
    geom_point(data = xx, aes(year, value, color = "rcmip")) +
    geom_point(data = climate_indicators, aes(year, value, color = "climate ind"))
