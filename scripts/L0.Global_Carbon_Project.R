# Description: Read in the Global carbon project data and convert to Hector units
# and naming conventions. Emissions may be categorized into
# are categorized into sectors and total.


# Global Carbon Budget 2024 (Friedlingstein et al., 2024b, ESSD)
# Description file:///Users/dorh012/Downloads/Global+Carbon+Budget+v2024+Dataset+Descriptions.pdf
# 0. Set Up --------------------------------------------------------------------
# Start from a clean environment
# TODO this would be dropped if written as a function like gcamdata
remove(list = ls())

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# 1. Main Chunk CO2 Emissions -------------------------------------------------
# Load the mapping file
mapping <- read.csv(list.files(DIRS$MAPPING, pattern = "L0.GCB_hector_mapping.csv",
                               full.names = TRUE), comment.char = "#")


# Check inputs
gcb_file <- list.files(DIRS$RAW_DATA, pattern = "Global_Carbon_Budget_2024_v1.0", full.names = TRUE)
assert_that(file.exists(gcb_file), msg =  "missing data need run get-raw-data.sh")



# Read in the raw data change to long format in preparation for joining
# with the mapping file.
readxl::read_xlsx(path = gcb_file, sheet = "Historical Budget", skip = 15)  %>%
    pivot_longer(-Year, names_to = "gcb_variable")  %>%
    inner_join(mapping, by = join_by(gcb_variable)) %>%
    # Convert to Hector values
    mutate(value = cf * value)  %>%
    select(year = Year, total = hector_total, sector = hector_sector,
           variable = hector_variable, value, units = hector_units) %>%
    mutate(source = "GCP") %>%
    na.omit ->
    out


# TODO this is something that we might want to revisit
# Emissions must be strictly positive, make sure that is the case here, otherwise
# throw an error.
assert_that(isFALSE(any(out$value < 0)), msg = "negative CO2 emissions dected")


# Save the emissions from the global carbon project
write.csv(out, file = file.path(DIRS$L0, "L0.GCP_emissions.csv"), row.names = FALSE)


# Z. Comparison with Hector inputs ---------------------------------------------
if(FALSE){
    # TODO remove this eventually after feel good about things
    source(here::here("scripts", "dev", "hector_comp_data.R"))

    out$source <- "GCB"
    hector_comp$source <- "hector"

    bind_rows(out, hector_comp) %>%
        filter(variable == LUC_EMISSIONS()) %>%
        filter(year <= 2024) %>%
        ggplot() +
        geom_line(aes(year, value, color = source))
}





