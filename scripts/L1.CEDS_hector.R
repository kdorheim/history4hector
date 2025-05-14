# Description: Read in the L0 CEDS data and convert to Hector units.

# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Load data
DIRS$INTERMED %>%
    list.files(pattern = "L0.CEDS_raw.csv",
               full.names = TRUE) %>%
    read.csv ->
    ceds_raw

DIRS$MAPPING %>%
    list.files(pattern = "M.CEDS_hector.csv",
               full.names = TRUE) %>%
    read.csv(comment.char = "#") %>%
    mutate(units = format_units_fxn(units)) ->
    mapping


# 1. Main Chunk ----------------------------------------------------------------

ceds_raw %>%
    left_join(mapping, by = join_by(variable)) %>%
    # Apply the conversion factor.
    mutate(value = value * cf) %>%
    check_req_names(req_cols = HEADERS$L1) ->
    output


# 2. Save Output ---------------------------------------------------------------

output %>%
    check_req_names(req_cols = HEADERS$L1) %>%
    write.csv(file = file.path(DIRS$INTERMED, "L1.CEDS_hector.csv"),
              row.names = FALSE)

