# Description: Read in the L1 RCMIP data and convert to Hector units.

# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# Load data
DIRS$INTERMED %>%
    list.files(pattern = "L0.RCMIP_raw.csv",
               full.names = TRUE) %>%
    read.csv %>%
    mutate(units = format_units_fxn(units)) ->
    rcmip_raw


DIRS$MAPPING %>%
    list.files(pattern = "M.RCMIP_hector.csv",
               full.names = TRUE) %>%
    read.csv(comment.char = "#") %>%
    mutate(units = format_units_fxn(units)) ->
    mapping


rcmip_raw %>%
    inner_join(mapping, by = join_by(variable, units)) %>%
    # Apply the conversion factor.
    mutate(value = value * cf) %>%
    filter(!is.na(value)) ->
    output




# 2. Save Output ---------------------------------------------------------------

output %>%
    check_req_names(req_cols = HEADERS$L1) %>%
    write.csv(file = file.path(DIRS$INTERMED, "L1.RCMIP_hector.csv"),
              row.names = FALSE)

# Z. Quality Control -----------------------------------------------------------

rcmip_raw %>%
    filter(grepl(x = tolower(variable), pattern = "alb")) %>%

    filter(sector == "historical-cmip5") %>%
    ggplot(aes(year, value, color = variable)) +
    geom_line()
