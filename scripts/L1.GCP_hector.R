# Description: Convert the global carbon project emissions from native units to
# the units required by Hector.

# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# Load data
DIRS$MAPPING %>%
    list.files(pattern = "L0.GCP_hector_mapping.csv",
               full.names = TRUE) %>%
    read.csv(comment.char = "#") %>%
    mutate(units = format_units_fxn(units)) ->
    mapping

DIRS$INTERMED %>%
    list.files(pattern = "L0.GCP_raw.csv",
               full.names = TRUE) %>%
    read.csv %>%
    mutate(units = format_units_fxn(units)) ->
    gcp_raw


# 1. Main Chunk ----------------------------------------------------------------

gcp_raw %>%
    left_join(mapping, by = c("variable", "units")) %>%
    # Convert to Hector units!
    mutate(value = value * cf) %>%
    filter(!is.na(value)) %>%
    # Switch to Hector naming and units
    select(source,
           year,
           value,
           variable,
           units = hector_units, sector) ->
    output

# 2. Save Output ---------------------------------------------------------------

output %>%
    check_req_names(req_cols = HEADERS$L1) %>%
    write.csv(file = file.path(DIRS$INTERMED, "L1.GCP_hector.csv"),
              row.names = FALSE)

