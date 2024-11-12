# Description: Read in the CEDS data and convert to Hector units and naming
# conventions. Since the CEDS data set does not include emission sources
# from biomass burning several some of the emissions are "incomplete" and
# will need to be combined with emissions from other sources. Those
# emission species will be written out to L0 data directory where as the
# emission species are written out to the L1 data directory.
# TODO
# - consider if the L0 outputs should be written out separately for each em type..
# - comment out or delete the diagnostics section
# - write a function that reads in and formats the mapping function
# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Define Helper Functions -----
# Find all the global sector ceds emissions for a specific release version! Error
# will be thrown if the data has not already been downloaded.
# Args
#   DIR: str path to where the ceds data files live
#   ceds_v: str of the release version, I think it might typically be a date
# Returns the str of the full ceds path
find_my_ceds_files <- function(DIR, ceds_v = "v2024_07_08"){

    assert_that(dir.exists(DIR))

    # Find all the possible ceds files
    all_ceds_files <- list.files(DIR, pattern = "CEDS")
    assert_that(length(all_ceds_files) >= 1, msg = "no CEDS files found")

    # Now check to see if there are global emissions by sector... it
    # does seem like the CEDS versions have different file naming patterns
    ceds_global_files <- all_ceds_files[grepl(x = all_ceds_files, pattern = "global")]
    ceds_sector <- ceds_global_files[grepl(x = ceds_global_files, pattern = "sector")]

    # I do not want to be messing around with the fuel types!
    ceds_sector_global <- ceds_sector[!grepl(x = ceds_sector, pattern = "fuel")]
    files <- ceds_sector_global[grepl(x = ceds_sector_global, pattern = ceds_v)]

    assert_that(length(files) >= 1,
                msg = paste0("no global sector emission files found for ", ceds_v))

    # Append the file names on to the directory path
    out <- file.path(DIR, files)
    return(out)
}


# 1. Import Data ---------------------------------------------------------------
# TODO ugh I think there is probably a better way to handle this..
# Load the mapping file.
mapping <- read.csv(list.files(DIRS$MAPPING, pattern = "CEDS_hector_mapping",
                               full.names = TRUE), comment.char = "#")

# Split the mapping file into the emission types that represent the total
# global values that will be ready to read into hector and the emissions
# that need to be aggregated with additional sources because they
# do not account for all sectors contributing to global emissions.
mapping %>%
    filter(total == 1) ->
    mapping_to_hector

mapping %>%
    filter(total == 0) ->
    sectoral_mapping

# Read in all the raw data
find_my_ceds_files(DIRS$RAW_DATA) %>%
    lapply(read.csv) %>%
    do.call(what = "bind_rows") ->
    ceds_raw_data

# 3. Total Global Emissions ----------------------------------------------------
# TODO honestly I think that there might be other perfered values  values for these time series...
ceds_raw_data %>%
    filter(em %in% mapping_to_hector$ceds_variable) %>%
    pivot_longer(starts_with("X"), names_to = "year") %>%
    mutate(year = as.integer(gsub(replacement = "", x = year, pattern = "X"))) %>%
    rename(ceds_variable = em, ceds_units = units) %>%
    inner_join(mapping_to_hector, by = join_by(ceds_variable, ceds_units)) %>%
    mutate(value = value * cf) %>%
    summarise(value = sum(value), .by = c("hector_variable", "year", "hector_units")) %>%
    select(variable = hector_variable, year, value, units = hector_units) ->
    ready_for_hector

# Save the total emissions that a ready to be read into Hector
emiss_tag <- paste0(gsub(x = unique(ready_for_hector$variable),
                         replacement = "", pattern = "_emissions"),
                    collapse = "_")
fname <- paste0("Hector_", emiss_tag, ".csv")
write.csv(ready_for_hector, file = file.path(DIRS$L1, fname), row.names = FALSE)



# 4. Incomplete Global Emissions ------------------------------------------------
# Convert the incomplete global emissions to Hector units, these emission species
# are missing sectors or categories such as emissions from open burning and
# what not.
ceds_raw_data %>%
    filter(em %in% sectoral_mapping$ceds_variable) %>%
    pivot_longer(starts_with("X"), names_to = "year") %>%
    mutate(year = as.integer(gsub(replacement = "", x = year, pattern = "X"))) %>%
    rename(ceds_variable = em, ceds_units = units) %>%
    left_join(sectoral_mapping, by = join_by(ceds_variable, ceds_units)) %>%
    mutate(value = value * cf) %>%
    select(variable = hector_variable, sector, year, units = hector_units, value) ->
    incomplete

# Save the incomplete emissions that will need to be combined with other
# emission sources to have the total global values.
emiss_tag <- paste0(gsub(x = unique(incomplete$variable),
                         replacement = "", pattern = "_emissions"),
                    collapse = "_")
fname <- paste0("Incomplete-CEDS_", emiss_tag, ".csv")
write.csv(incomplete, file = file.path(DIRS$L0, fname), row.names = FALSE)

# Z. Comparison with Hector inputs ---------------------------------------------
# TODO remove this eventually after feel good about things but this
# line will load hector_comp which is the default set of emissions
# that is useful for comparing results with
source(here::here("scripts", "dev", "hector_comp_data.R"))


em_name <- EMISSIONS_OC()

incomplete %>%
    filter(variable == em_name) %>%
    summarise(value = sum(value), .by = c("variable", "year", "units")) ->
    ceds_emiss

hector_comp %>%
    filter(variable == em_name) ->
    default_emiss

ggplot() +
    geom_line(data = ceds_emiss, aes(year, value, color = "ceds")) +
    geom_line(data = default_emiss, aes(year, value, color = "default"))

