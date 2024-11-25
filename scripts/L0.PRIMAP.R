# The PRIMAP-hist national historical emissions time series
# (1750-2023) (v2.6, updated September 2024)
# Citations
#    Gütschow, J.; Busch, D.; Pflüger, M. (2024): The PRIMAP-hist national historical emissions time
#       series v2.6 (1750-2023). zenodo.10.5281/zenodo.13752654.
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
mapping <- read.csv(list.files(DIRS$MAPPING, pattern = "L0.PRIMAP_hector_mapping",
                               full.names = TRUE), comment.char = "#")


# 2. Format Data -----------------------------------------------------------------
# Subset the data so that we are only working with the global data and not
# the country level results. We are also interested in the global results
# that are derived from the third party inventories the HISTTP
# see Gütschow et al. 2016 for more details.
# We are also intrested in the aggregate total values which are coded with
# M.0.EL, see https://zenodo.org/records/13752654 for details.
earth_id <- which(raw_data$area == "EARTH")
inventory_id <- which(raw_data$scenario == "HISTTP")
total_id <- which(raw_data$category == "M.0.EL")
id <- intersect(intersect(earth_id, inventory_id), total_id)
global_data <- raw_data[id, ]


# Prepare the data for processing with via mapping file.
global_data %>%
    pivot_longer(cols = starts_with("X"), names_to = "year") %>%
    mutate(year = as.integer(gsub(replacement = "", x = year, pattern = "X"))) %>%
    inner_join(mapping, by = join_by(scenario, entity, unit, category)) %>%
    # Convert to hector units!
    mutate(value = value * cf) %>%
    select(year, total = hector_total, sector = hector_sector,
           variable = hector_variable, value, units = hector_units) %>%
    mutate(source = "PRIMAP") ->
    out

write.csv(out, file = file.path(DIRS$L0, "L0.PRIMAP_emissions.csv"), row.names = FALSE)


# Z. Comparison with Hector inputs ---------------------------------------------
if(FALSE){
    # TODO remove this eventually after feel good about things, load hector_comp
    source(here::here("scripts", "dev", "hector_comp_data.R"))


    hector_comp$source <- "default"
    to_plot <- bind_rows(out, hector_comp)
    em <- EMISSIONS_N2O()

    to_plot %>%
        filter(variable == em) %>%
        ggplot() +
        geom_line(aes(year, value, color = source)) +
        labs(title = em, y = getunits(em))
}


