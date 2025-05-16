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


# ---- Helper Functions --------------------------------------------------------
# Calculate emissions based on halo carbon concentrations
#   params: data.frame of halo carbon parameters scraped from the ini files
#   conc_data: data.frame of the halocarbon concentrations
# Returns: data.frame of the halocarbon emissions
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

    out <- data.frame(year = conc_data$year[1:n-1] + 1,
                      value = my_emiss,
                      variable = params$hector_variable)

    return(out)

}

# 1. Main Chunk ----------------------------------------------------------------

# Subset the concentrations to only the halocarbons used by Hector.
data_raw %>%
    inner_join(mapping, by = join_by(variable, units)) %>%
    filter(name %in% halo_params$name) ->
    concentrations_data

halo   <- halo_params$name
output <- data.frame()

for(halo in halo_params$name){

    conc   <- filter(concentrations_data, name == halo)
    params <- filter(halo_params, name == halo)


    output <- rbind(output, get_emiss(params, conc))

}

# There should be no negative emissions in this data set.
output %>%
    mutate(value = if_else(value < 0, 0, value)) %>%
    mutate(source = unique(data_raw$source),
           units = "Gg",
           sector = "WORLD") ->
    output


# 2. Save Output ---------------------------------------------------------------

output %>%
    check_req_names(req_cols = HEADERS$L1) %>%
    write.csv(file = file.path(DIRS$INTERMED, "L1.halo_hector.csv"),
              row.names = FALSE)

