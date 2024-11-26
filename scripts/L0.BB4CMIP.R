# Description: Read in the global biomass burning emissions for CMIP6 (BB4CMIP)
# these emissions will be combined with other CEDS and other anthropogenic
# time series to get the total global emissions that will be used in Hector.
# TODO
# It is unclear what the N2O units are!

# 0. Set Up --------------------------------------------------------------------
# Start from a clean environment
# TODO this would be dropped if written as a function like gcamdata
remove(list = ls())

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# Load the BB4CMIP data files
# Args
#   f: string path to the data file to read in
# Returns: data.frame of global emission per sector
load_BB4CMIP_fxn <- function(f){

    # Each of the files corresponded to a type of emissions single sector.
    d <- read.csv(f)

    # assert that the data only contains one type of data
    ver <- unique(d$ver)
    stopifnot(length(ver) == 1)

    sector <- unique(d$sector)
    stopifnot(length(sector) == 1)

    # Determine emission species
    em <- unlist(strsplit(basename(f), split = "_|-"))[[1]]

    # Extract the global values
    inds <- which(grepl(x = colnames(d), pattern = "X"))
    value <- as.vector(colSums(d[, inds]))
    years <- as.vector(as.numeric(gsub(x = colnames(d)[inds], pattern = "X", replacement = "")))
    units <- "Tg"

    # Format output
    out <- data.frame(year = years, sector, variable = em, value, units)
    return(out)


}

# 1. Main Chunk ----------------------------------------------------------------
# Import raw data files
file.path(DIRS$RAW_DATA, "1750-2015_v1.2_Emissions_bulk_em") %>%
    list.files(pattern = "csv", full.names = TRUE) %>%
    lapply(load_BB4CMIP_fxn) %>%
    do.call(what = "rbind") ->
    emissions

# Rad in the mapping file
file.path(DIRS$MAPPING, "L0.BB4CMIP_hector_mapping.csv") %>%
    read.csv(comment.char = "#") ->
    mapping

# Use the mapping file to aggregate the open burning emissions and
# convert to Hector emissions.
emissions %>%
    inner_join(mapping, by = join_by(sector, variable)) %>%
    mutate(value = value * cf) %>%
    summarise(value = sum(value), .by = c("year", "hector_variable",
                                          "hector_units", "hector_total",
                                          "hector_sector")) %>%
    select(year, total = hector_total, sector = hector_sector,
           variable = hector_variable, value, units = hector_units) %>%
    mutate(source = "BB4CMIP") ->
    out

# Save the emissions from the global carbon project
write.csv(out, file = file.path(DIRS$L0, "L0.BB4CMIP_emissions.csv"), row.names = FALSE)

# Z. Comparison with Hector inputs ---------------------------------------------
if(FALSE){

# TODO remove this eventually after feel good about things
source(here::here("scripts", "dev", "hector_comp_data.R"))
hector_comp$source <- "hector"

em <-  EMISSIONS_N2O()

file.path(DIRS$L0, "L0.CEDS_emissions.csv") %>%
    read.csv() ->
    ceds

out %>%
    filter(variable == em) %>%
   bind_rows(ceds) %>%
    summarise(value = sum(value, na.rm = TRUE), .by = c("year", "variable")) %>%
    mutate(source = "CEDS + BB4CMIP") ->
    out2

bind_rows(out2, hector_comp) %>%
    filter(variable == em) %>%
    filter(year <= 2022) %>%
    ggplot() +
    geom_line(aes(year, value, color = source))
}

