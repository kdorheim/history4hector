# Description: Read in the BB4CMIP (the open burning emissions data set) and
# format for downstream use.


# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Load the BB4CMIP data files
# Args
#   f: string path to the data file to read in
# Returns: data.frame of global emission per sector
load_BB4CMIP_fxn <- function(f){

    # Each of the files corresponded to a type of emissions single sector.
    d <- read.csv(f)

    # Make sure we have the global values
    stopifnot(unique(d$region) == "World")

    # Determine emission species
    #em <- unlist(strsplit(basename(f), split = "_|-"))[[1]]

    # Extract the global values
    inds <- which(grepl(x = colnames(d), pattern = "X"))

    # Format the output
    pivot_longer(d, names_to = "year", values_to = "value", cols = inds) %>%
        mutate(year = as.numeric(gsub(replacement = "", pattern = "X", x = year))) %>%
        na.omit ->
        out

    return(out)


}


# 1. Main Chunk ----------------------------------------------------------------
# Import raw data files
file.path(DIRS$RAW_DATA) %>%
    list.files(pattern = "gfed-bb4cmip_cmip7_global_0011", full.names = TRUE) ->
    files

# Assert that all the files exists, if this throws an error
# see the read me in the data-raw directory.
stopifnot(all(file.exists(files)))


# Load data
files %>%
    lapply(load_BB4CMIP_fxn) %>%
    do.call(what = "rbind") ->
    emissions

# Format data
emissions %>%
    rename(source = model, units = unit, sector = variable) %>%
    mutate(units = format_units_fxn(units)) %>%
    # Extract the emissions variable name from the sector information
    mutate(variable = gsub(replacement = "", x = sector,
                           pattern = "Emissions|Biomass Burning")) %>%
    mutate(variable =  gsub(replacement = "", x = variable,
                             pattern = "\\|")) %>%
    mutate(sector =  gsub(replacement = " ", x = sector,
                            pattern = "\\|")) ->
    output



output %>%
    filter(variable == "Nx") %>% head()




# 2. Save Output ---------------------------------------------------------------

output %>%
    check_req_names(req_cols = HEADERS$L0) %>%
    write.csv(file = file.path(DIRS$INTERMED, "L0.BB4CMIP_raw.csv"),
              row.names = FALSE)













