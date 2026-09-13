# Description: Read in the Global carbon project data and format so that is is in
# raw units but a form that is consistent and easy to work with.
#
# Global Carbon Budget 2025 (Friedlingstein et al., 2025, ESSD)
# https://globalcarbonbudget.org/datahub/the-latest-gcb-data-2025/

# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# 1. Main Chunk CO2 Emissions --------------------------------------------------

# Check inputs
gcb_file <- list.files(DIRS$RAW_DATA, pattern = "Global_Carbon_Budget_",
                       full.names = TRUE, recursive = TRUE)
assert_that(file.exists(gcb_file), msg = "missing raw-data")




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

# Z. QAQC ----------------------------------------------------------------------

# Let's take a bit of a look of what is going on with the different sinks....
if(FALSE){
    output %>%
        filter(variable %in% c("land sink", "ocean sink", "budget imbalance")) %>%
        ggplot(aes(year, value, color = variable)) +
        geom_line()
}
