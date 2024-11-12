# The PRIMAP-hist national historical emissions time series
# (1750-2023) (v2.6, updated September 2024)
# Citations
#    Gütschow, J.; Busch, D.; Pflüger, M. (2024): The PRIMAP-hist national historical emissions time
#       series v2.6 (1750-2023). zenodo. 10.5281/zenodo.13752654.
#   Gütschow, J.; Jeffery, L.; Gieseke, R.; Gebel, R.; Stevens, D.; Krapp, M.; Rocha, M. (2016):
#       The PRIMAP-hist national historical emissions time series, Earth Syst. Sci. Data, 8, 571-603,
#       doi:10.5194/essd-8-571-2016
#
# Prepare historical SF6 emissions for Hector
# TODO
#   - do we need CH4 and N2O emissions from non-biomass burning sources from here?
#   - clean up the comparison section

# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# 1. Load Data -----------------------------------------------------------------

# Import the data
file <- list.files(DIRS$RAW_DATA, pattern = "PRIMAP", full.names = TRUE)
raw_data <- read.csv(file)

# Clean up the column names so that they don't have any .. in them
col_names <- colnames(raw_data)
new_names <- gsub("\\..*", "", col_names)
colnames(raw_data) <- new_names

# Import the mapping data
mapping <- read.csv(list.files(DIRS$MAPPING, pattern = "PRIMAP_hector_mapping",
                               full.names = TRUE), comment.char = "#") %>%
    # TODO this is unideal
    mutate(category = as.character(category))

# Split the mapping file into the emission types that represent the total
# global values that will be ready to read into hector and the emissions
# that need to be aggregated with additional sources because they
# do not account for all sectors contributing to global emissions.
mapping %>%
    filter(!is.na(hector_variable)) ->
    mapping_to_hector

mapping %>%
    filter(is.na(hector_variable)) ->
    sectoral_mapping


# 2. Format Data -----------------------------------------------------------------

# Subset the data so that we are only working with the global data and not
# the country level results.
earth_id <- raw_data$area == "EARTH"
global_data <- raw_data[earth_id, ]

# Prepare the data for processing with via mapping file.
global_data %>%
    pivot_longer(cols = starts_with("X"), names_to = "year") %>%
    mutate(year = as.integer(gsub(replacement = "", x = year, pattern = "X"))) ->
    long_data

# 3. Total Global Emissions ----------------------------------------------------

# Process the emissions that are ready for Hector
long_data %>%
    inner_join(mapping_to_hector, by = join_by(scenario, entity, unit, category)) %>%
    mutate(value = value * cf) %>%
    select(year, value, variable = hector_variable, units = hector_units) ->
    ready_for_hector


# Write the emissions out as csv files, need to decide if this should be
# a single csv file or separately for each emissions type?
fname <- "Hector_SF6.csv"
write.csv(ready_for_hector, file = file.path(DIRS$L1, fname), row.names = FALSE)


# Z. Comparison with Hector inputs ---------------------------------------------
# TODO remove this eventually after feel good about things, load hector_comp
source(here::here("scripts", "dev", "hector_comp_data.R"))


hector_comp %>%
    filter(variable %in% c(ready_for_hector$variable))->
    default

# For the emission species do not have to be further aggregated write out to
#
long_data %>%
    inner_join(mapping, by = join_by(scenario, entity, unit, category)) %>%
    mutate(value = value * cf) %>%
    select(year, value, variable = hector_variable, units = hector_units) ->
    ready_for_hector


ggplot() +
    geom_line(data = ready_for_hector, aes(year, value, linetype = category,
                                  color = category)) +
    geom_line(data = default, aes(year, value, color = "hector"))

