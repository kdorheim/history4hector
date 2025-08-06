# beta q10_rh
# 0.54   1.00
#
# diff
# 1.21
#



source("scripts/dev/calibration_fxns.R")

YRS <- 1745:FINAL_HIST_YEAR

ini <- "inputs/hector-gcam.ini"
hc <- newcore_CH4_N2O(ini, name = "default")
run(hc, runtodate = FINAL_HIST_YEAR)
out1 <- fetchvars_4comparison(hc, comp = comparison_data)


#0.8726901 2.6739298
ini <- "inputs/hector-gcam.ini"
hc <- newcore_CH4_N2O(ini, name = "new")
fitted_params <- c("beta" = 0.87, "q10_rh" = 2.67, "diff" = 1.21)
hc <- my_setvar_fxn(hc, fitted_params)
run(hc, runtodate = FINAL_HIST_YEAR)
out2 <- fetchvars_4comparison(hc, comp = comparison_data)


out1 %>%
    rbind(out2) ->
    hector_rlsts


comparison_data %>%
    filter(variable %in% hector_rlsts$variable) ->
    comp_to_plot



# Problem we are WAY over estimating CO2 concentrations... which is honestly
# kind of shocking...
ggplot() +
    geom_line(data = hector_rlsts, aes(year, hector, color = scenario)) +
    geom_line(data = comp_to_plot, aes(year, value)) +
    geom_ribbon(data = comp_to_plot, aes(year, ymin = lower, ymax = upper)) +
    facet_wrap("variable", scales = "free", ncol = 1) +
    coord_cartesian(xlim = c(1970, 2000 ))



ini <- system.file(package =  "hector", "input/hector_ssp245.ini")
hc <- newcore(ini)
run(hc)
out1 <- fetchvars(hc, 1850:2100, vars = c(CONCENTRATIONS_CO2(), GMST(), HEAT_FLUX()))
out1$scenario <- "default"
hc <- my_setvar_fxn(hc, fitted_params)
run(hc)
out2 <- fetchvars(hc, 1850:2100, vars = c(CONCENTRATIONS_CO2(), GMST(), HEAT_FLUX()))
out2$scenario <- "new"


out1 %>%
    rbind(out2) %>%
    ggplot(aes(year, value, color = scenario)) +
    geom_line() +
    facet_wrap("variable", scales = "free", ncol = 1)


