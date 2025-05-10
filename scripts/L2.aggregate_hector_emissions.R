# Description: Read in all the L1 data and use the L2 data to aggregate.
# TODO there seems to be a problem with the time step?
# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# --- Define Helper Functions  ------------------------------------------------------

# Using observations of N2O concentrations and the anthropocentric N2O emissions
# back calculate the natural N2O emissions for Hector
# Args
#   n2o_conc: data frame of observations of global N2O concentrations
#   total_emiss: data frame of Hector's N2O_emissions
# Returns: data frame of the N2O natural emissions for Hector
get_natural_N2O <- function(n2o_conc, total_emiss){

    # Confirm that we are only working with the correct variables.
    stopifnot(unique(total_emiss$variable) == EMISSIONS_N2O())
    stopifnot(unique(n2o_conc$variable) == CONCENTRATIONS_N2O())


    # As defined in table S2 of Dorheim et al. 2024
    tau_0 <- 132
    N2O_conc_0 <- 273.87

    # Save information about the number of entries
    n <- nrow(n2o_conc)

    # Determine the change in N2O concentrations
    # per time step.
    delta_n2o <- diff(n2o_conc$value)

    # Calculate N2O lifetime
    tau_n2o <- tau_0 * (n2o_conc$value[1:n-1]/N2O_conc_0)^(-0.05)

    # Calculate the total emissions based on equation (S1)
    my_emiss <- 4.8 * (delta_n2o + n2o_conc$value[1:n-1]/tau_n2o)

    # Calculate the difference between total emissions associated with
    # n2o concentrations and the anthropocentric emissions.
    natural_emiss <- my_emiss-total_emiss$value[2:n]

    data.frame(year = total_emiss$year[1:n-1],
               value = natural_emiss,
               variable = NAT_EMISSIONS_N2O()) %>%
        na.omit() %>%
        mutate(units = getunits(NAT_EMISSIONS_N2O())) ->
        out

    return(out)

}


# --- Import Data  -----------------------------------------------------------------

# Load data
DIRS$INTERMED %>%
    list.files(pattern = "L1",
               full.names = TRUE) ->
    L1_files

L1_files %>%
    lapply(read.csv) %>%
    do.call(what = "rbind") ->
    L1_data

file.path(DIRS$INTERMED, "L0.CMIP6_ghgs.csv") %>%
    read.csv ->
    conc_data

DIRS$MAPPING %>%
    list.files(pattern = "L2",
               full.names = TRUE) %>%
    read.csv(comment.char = "#") ->
    mapping


# 1. Main Chunk ----------------------------------------------------------------
# --- Aggregate global emissions -----------------------------------------------
# Aggregate to global emissions. Note that there are some additional
# variables that are missing that need to be handled individually.
L1_data %>%
    left_join(mapping,
              by = join_by("variable", "sector", "source")) %>%
    summarise(value = sum(value), .by = c("hector_variable", "year")) %>%
    select(variable = hector_variable, year, value) %>%
    mutate(units = getunits(variable)) %>%
    filter(year <= FINAL_YEAR) %>%
    extend_to_1745 ->
    global_total


# --- Natural N2O emissions ----------------------------------------------------
# Calculate the natural N2O emissions from the N2O concentration observations
# and the anthropogenic N2O emissions.
n2o_conc  <- filter(conc_data, variable == CONCENTRATIONS_N2O())
n2o_emiss <- filter(global_total, variable == EMISSIONS_N2O())

# Calculate natural N2O emissions
natural_n2o <- get_natural_N2O(n2o_conc, n2o_emiss)

# Hold the future natural N2O emissions constant for the rest
# of the future period. This is the approach taken by other RCMs
# see FAIR v1.3 documentation (Smith et al. 2018).
natural_n2o %>%
    filter(year %in% 2009:2014) %>%
    pull(value) %>%
    mean ->
    future_value

natural_n2o %>%
    bind_rows(
        data.frame(year = max(natural_n2o$year):2300,
                   value = future_value,
                   variable = NAT_EMISSIONS_N2O(),
                   units = getunits(NAT_EMISSIONS_N2O()))) ->
    final_natural_n2o

# --- Natural CH4 Emissions ----------------------------------------------------

# TODO this is a place holder for now because Hector does not have the capability
# to do time varying Natural CH4 emissions...





# --- Consolidate Emissions ----------------------------------------------------

global_total %>%
    bind_rows(final_natural_n2o) ->
    output

# 2. Save Output ---------------------------------------------------------------

output %>%
    check_req_names(req_cols = HEADERS$L2) %>%
    write.csv(file = file.path(DIRS$INTERMED, "L2.hector_inputs.csv"),
              row.names = FALSE)



# Z. Quality Check -------------------------------------------------------------

if(FALSE){

    source("scripts/dev/hector_comp_data.R")

    # Compare new emissions vs. the older ones..
    em <- EMISSIONS_CH4()

    hector_comp %>%
        filter(variable == em) ->
        comp_to_plot

    output %>%
        filter(variable == em) ->
        to_plot

    ggplot() +
        geom_line(data = comp_to_plot, aes(year, value, color = "old")) +
        geom_line(data = to_plot, aes(year, value, color = "new")) +
        labs(title = em)


}


