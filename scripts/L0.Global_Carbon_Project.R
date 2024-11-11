# Format the Global Carbon Project data for Hector

# TODO
#   - how do we get we want to get the before 1850 values?
#   - how do we want to handle the daccs uptake? i think right now
#   - how do want to handle the different GCB releases?
# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

GCP_YEAR <- "2023v1.0"
CITATION <- "Global Carbon Budget 2023 (Friedlingstein et al., 2023b, ESSD)"








# 1. CO2 Fossil Fuel Emissions -------------------------------------------------

# Check inputs
ffi_file <- list.files(DIRS$RAW_DATA, pattern = "National_Fossil_Carbon_Emissions", full.names = TRUE)
assert_that(file.exists(ffi_file), msg =  "missing data need run get-raw-data.sh")

version <- grepl(pattern = GCP_YEAR, x = ffi_file)
assert_that(version, msg =  "wrong version of GCP data located")


# Read in the fossil fuels data
ffi_data <- readxl::read_xlsx(path = ffi_file, sheet = "Territorial Emissions", skip = 11)

# It is unideal but we know that the years data is stored in the first column,
# extract those values here.
years <- unlist(ffi_data[, 1], use.names = FALSE)
emiss <- ffi_data$World * UNITS$MTonne_to_Pg

# Format into a single data frame
ffi_emiss_df <- data.frame(year = years, value = emiss,
                           variable = FFI_EMISSIONS(),
                           units = getunits(FFI_EMISSIONS()))



# 2. CO2 Land Use Land Cover Change Emissions ----------------------------------


# Check inputs
luc_file <- list.files(DIRS$RAW_DATA, pattern = "National_LandUseChange_Carbon_Emissions", full.names = TRUE)
assert_that(file.exists(luc_file), msg =  "missing data need run get-raw-data.sh")

version <- grepl(pattern = GCP_YEAR, x = luc_file)
assert_that(version, msg =  "wrong version of GCP data located")


# Read in the fossil fuels data
ffi_data <- readxl::read_xlsx(path = luc_file, sheet = "BLUE", skip = 11)

# It is unideal but we know that the years data is stored in the first column,
# extract those values here.
years <- unlist(ffi_data[, 1], use.names = FALSE)
emiss <- ffi_data$World * UNITS$MTonne_to_Pg

# Format into a single data frame
ffi_emiss_df <- data.frame(year = years, value = emiss,
                           variable = FFI_EMISSIONS(),
                           units = getunits(FFI_EMISSIONS()))







# Z. Comparison with Hector inputs ---------------------------------------------
# TODO remove this eventually after feel good about things
source(here::here("scripts", "dev", "hector_comp_data.R"))

hector_comp %>%
    filter(variable == FFI_EMISSIONS()) %>%
    filter(year <= 2024) %>%
    ggplot() +
    geom_line(aes(year, hector_value)) +
    geom_line(data = emiss_df, aes(year, value, color = "GCP")) +
    NULL



