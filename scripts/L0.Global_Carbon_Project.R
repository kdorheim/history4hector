# Format the Global Carbon Project data for Hector

# TODO
#   - how do we get we want to get the before 1850 values?
#   - how do we want to handle the daccs uptake? i think right now
#   - how do want to handle the different GCB releases?
#   - set up the mapping file?
# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# TODO need to decide what want to do with sources and versions... also may be copy
# and paste the full data ciation here...
GCP_YEAR <- "2023v1"
CITATION <- "Global Carbon Budget 2023 (Friedlingstein et al., 2023b, ESSD)"





# 1. Main Chunk CO2 Emissions -------------------------------------------------

# Check inputs
gcb_file <- list.files(DIRS$RAW_DATA, pattern = "Global_Carbon_Budget", full.names = TRUE)
assert_that(file.exists(gcb_file), msg =  "missing data need run get-raw-data.sh")

version <- grepl(pattern = GCP_YEAR, x = gcb_file)
assert_that(version, msg =  "wrong version of GCP data located")


# Read in the fossil fuels data
readxl::read_xlsx(path = gcb_file, sheet = "Historical Budget", skip = 15) %>%
    select(year = Year, "fossil emissions excluding carbonation", "land-use change emissions") ->
    gcb_data

names(gcb_data) <- c("year", FFI_EMISSIONS(), LUC_EMISSIONS())

# Change from wide to long format,
gcb_data %>%
    pivot_longer(-year, names_to = "variable") %>%
    mutate(units = getunits(variable),
           value = value * UNITS$GtC_to_PgC) %>%
    na.omit ->
    gcb_emissions

# TODO this is something that we might want to revisit
# Emissions must be strictly positive, make sure that is the case here, otherwise
# throw an error.
assert_that(isFALSE(any(gcb_emissions$value < 0)), msg = "negative CO2 emissions dected")


# Save the emissions from the global carbon project
write.csv(gcb_emissions,
          file = file.path(DIRS$L0, "L0.hector_GCP.csv"),
          row.names = FALSE)


# Z. Comparison with Hector inputs ---------------------------------------------
# TODO remove this eventually after feel good about things
source(here::here("scripts", "dev", "hector_comp_data.R"))

gcb_emissions$source <- "GCB"
hector_comp$source <- "hector"

bind_rows(gcb_emissions, hector_comp) %>%
    filter(variable == LUC_EMISSIONS()) %>%
    filter(year <= 2024) %>%
    ggplot() +
    geom_line(aes(year, value, color = source))




