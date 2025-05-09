# Description: Read in all the L1 data and use the L2 data to aggregate.

# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Load data
DIRS$INTERMED %>%
    list.files(pattern = "L1",
               full.names = TRUE) %>%
    read.csv ->
    L1_data

DIRS$MAPPING %>%
    list.files(pattern = "L2",
               full.names = TRUE) %>%
    read.csv(comment.char = "#") ->
    mapping


# 1. Main Chunk ----------------------------------------------------------------
# Aggregate to global emissions. Note that there are some additional
# variables that are missing that need to be handled individually.
L1_data %>%
    left_join(mapping,
              by = join_by("variable", "sector", "source")) %>%
    summarise(value = sum(value), .by = c("hector_variable", "year")) %>%
    select(variable = hector_variable, year, value) %>%
    mutate(units = getunits(variable)) ->
    global_total



# TODO
# Add natural CH4 chunk
# Add natural N2O chunk




output <- global_total

# 2. Save Output ---------------------------------------------------------------

output %>%
    check_req_names(req_cols = HEADERS$L2) %>%
    write.csv(file = file.path(DIRS$INTERMED, "L2.hector_inputs.csv"),
              row.names = FALSE)

