# Description: Using the concentrations from the climate indicators data
# calculate the halocarbon emissions for Hector.

# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Load data
DIRS$INTERMED %>%
    list.files(pattern = "L0.ClimateIndicators_raw.csv",
               full.names = TRUE) %>%
    read.csv ->
    data_raw

# Load the halocarbon parameter values.
DIRS$INTERMED %>%
    list.files(pattern = "L0.hector_halocarbon_params.csv",
               full.names = TRUE) %>%
    read.csv ->
    halo_params

# Read in the mapping file.
DIRS$MAPPING %>%
    list.files(pattern = "M.halocarbons.csv",
               full.names = TRUE) %>%
    read.csv ->
    mapping

# 1. Calculate halo conc -------------------------------------------------------

# Subset the concentrations to only the halocarbons used by Hector.
data_raw %>%
    inner_join(mapping, by = join_by(variable, units)) %>%
    filter(name %in% halo_params$name) ->
    concentrations_data

nn <- "CCl4"

halo_params %>%
    filter(name == nn) ->
    params


system.file(package = "hector",
            "input/tables/ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    select(year = Date, value = paste0(nn, "_emissions")) ->
    E_data


# So the hector equations is very different than what is included in the
# manuscript... also I think that it is going to be harder when it comes to
# taking time into account...

get_conc <- function(params, E_data){


    AtmDryAir <- 1.8
    tau <- params$tau
    alpha <- 1/tau
    H0 <- params$H0
    molar_mass <- params$molarMass


    # save a vector of the concentrations
    conc <- c()

    for(i in 1:nrow(E_data)){

        if (i == 1){
            prev <- H0
        } else {
            prev <- conc[i -1]
        }


        new <- prev * exp(-alpha) + (E_data$value[i]/molar_mass) / (0.1 * AtmDryAir) * tau * (1 - exp(-alpha))

        conc <- c(conc, new)
    }

    vals <- c(conc)

    out <- data.frame(name = params$hector_variable,
                      value = vals,
                      year = E_data$year)

    return(out)


}


out1 <- get_conc(params = params, E_data = E_data)


# so idk why stuff is looking different than what are a expecting,
# let's see when we comapre with actual output
ini <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- newcore(ini)
run(hc)

name <- nn

"~/Documents/Hector-WD/command_line/hector/output/outputstream_ssp245.csv" %>%
    read.csv(comment.char = "#") %>%
    filter(spinup == 0) %>%
    filter(grepl(x = component, pattern = name)) %>%
    filter(variable == "hc_concentration") ->
    hector_output


ggplot() +
    geom_line(data = out1, aes(year, value, color = "mine")) +
    geom_line(data = hector_output, aes(year, value, color = "hector"))


# 2. Backwards calculation  ----------------------------------------------------

# GCAM_EMISS we need to check


#
#
# [21] "OC_emissions"       "SF6_emissions"      "SO2_emissions"

# looks okay
# C2F6, CF4, HFC125, HFC134a, HFC143a, HFC227ea, HFC23, HFC245fa
# HFC32


# problem
#


 nn <- "SF6"


"~/Documents/Hector-WD/command_line/hector/output/outputstream_ssp245.csv" %>%
    read.csv(comment.char = "#") %>%
    filter(spinup == 0) %>%
    filter(grepl(x = component, pattern = nn)) %>%
    filter(variable == "hc_concentration") ->
    conc_data


system.file(package = "hector",
            "input/tables/ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    select(year = Date, value = paste0(nn, "_emissions")) ->
    E_data

halo_params %>%
    filter(name == nn) ->
    params


get_emiss <- function(params, conc_data){

    AtmDryAir <- 1.8
    tau <- params$tau
    alpha <- 1/tau
    H0         <- params$H0
    molar_mass <- params$molarMass
    n <- length(conc_data$value)
    emiss <- c()

    conc_ts <- c(H0, conc_data$value)


    # okay so the concentrations have to be lagged some how...
    num <- conc_ts[2:n] - (exp(-1/tau) * conc_ts[1:n-1])
    denom <- tau * (1 - exp(-1/tau))
    my_emiss <- (num/denom) * (0.1 * AtmDryAir * molar_mass)

    out <- data.frame(year = E_data$year[1:n-1] + 1,
                      value = my_emiss,
                      variable = params$hector_variable)

    return(out)

}



out <- get_emiss(params, conc_data)

ggplot() +
    geom_line(data = E_data, aes(year, value)) +
    geom_line(data = out, aes(year, value, color = "mine"))


# why the is it being negtaive??? I can do it forward? but not backwards...
# I think it has to do with suttle changes in the concentrations times series
# but have elected to over write with 0 since we know that we cannot have
# negative emissions in this case.

# Checking Historical Hector Core ----------------------------------------------

DIRS$INTERMED %>%
    file.path("L0.hector_halocarbon_params.csv") %>%
    read.csv() ->
    halo_params


DIRS$INTERMED %>%
    file.path("L1.halo_hector.csv") %>%
    read.csv() ->
    halo_emiss_new

# Run Hector under default set up
all_halo_vars <- fxntable$string[grepl(pattern = paste0(halo_params$name, collapse = "|"),
                      fxntable$string)]

forcing_halo_vars <- all_halo_vars[grepl(pattern ="Fad" , x = all_halo_vars)]
emiss_halo_vars <-  all_halo_vars[grepl(pattern ="emissions" , x = all_halo_vars)]
extra <- c(GLOBAL_TAS(), RF_TOTAL())


ini <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- newcore(ini)
run(hc)
default_out <- fetchvars(hc, dates = unique(halo_emiss_new$year), vars = c(forcing_halo_vars, emiss_halo_vars, extra))



# How what happens when we run Hector with all of the new halo carbon emissions?

for(halo in halo_params$hector_variable){

    emiss_df <- halo_emiss_new[halo_emiss_new$variable == halo,]
    setvar(hc, dates =  emiss_df$year, var = halo, values = emiss_df$value, unit = "Gg")
    reset(hc)

}

run(hc)

new_out <- fetchvars(hc, dates = unique(halo_emiss_new$year), vars = c(forcing_halo_vars, emiss_halo_vars, extra))



default_out$source <- "default"
new_out$source <- "new halo"


bind_rows(default_out, new_out) ->
    to_plot


[1] "FadjCF4"       "FadjC2F6"      "FadjHFC23"     "FadjHFC32"     "FadjHFC4310"   "FadjHFC125"
[7] "FadjHFC134a"   "FadjHFC143a"   "FadjHFC227ea"  "FadjHFC245fa"  "FadjSF6"       "FadjCFC11"
[13] "FadjCFC12"     "FadjCFC113"    "FadjCFC114"    "FadjCFC115"    "FadjCCl4"      "FadjCH3CCl3"
[19] "FadjHCFC22"    "FadjHCFC141b"  "FadjHCFC142b"  "Fadjhalon1211" "Fadjhalon1301" "Fadjhalon2402"
[25] "FadjCH3Cl"     "FadjCH3Br"

rf_names <- paste0("RF_", halo_params$name)


var <- "RF_HFC227ea"
to_plot %>%
    filter(variable == var) %>%
    ggplot() +
    geom_line(aes(year, value, color = source))


# things to take a look at
# RF_HFC32, RF_HFC4310,
