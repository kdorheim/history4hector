# Description: Read in the Global carbon project data and format so that is is in
# raw units but a form that is consistent and easy to work with.


# Global Carbon Budget 2024 (Friedlingstein et al., 2024b, ESSD)
# Description file:///Users/dorh012/Downloads/Global+Carbon+Budget+v2024+Dataset+Descriptions.pdf
# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# 1. Main Chunk CO2 Emissions --------------------------------------------------

# Check inputs
gcb_file <- list.files(DIRS$RAW_DATA, pattern = "Global_Carbon_Budget_2024_v1.0",
                       full.names = TRUE)
assert_that(file.exists(gcb_file),
            msg =  "missing data need run get-raw-data.sh")




# Read in the raw data change to long format in preparation for joining
# with the mapping file.
readxl::read_xlsx(path = gcb_file, sheet = "Historical Budget", skip = 15)  %>%
    pivot_longer(-Year, names_to = "variable")  %>%
    # Just reuse the variable name as the sector name since
    # there is only one type of emissions that make up the GCP.
    mutate(sector = variable) %>%
    na.omit %>%
    mutate(source = "GCP") %>%
    rename(year = Year) %>%
    mutate(units = "GtC/yr") ->
    output


# 2. Save emissions ------------------------------------------------------------

# Save the emissions from the global carbon project
output %>%
    check_req_names(req_cols = HEADERS$L0) %>%
    write.csv(file = file.path(DIRS$INTERMED, "L0.GCP_raw.csv"),
              row.names = FALSE)






