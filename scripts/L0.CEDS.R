# Description: Read in the CEDS data and format for downstream use.

# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# Define Helper Functions
# Find all the global sector ceds emissions for a specific release version! Error
# will be thrown if the data has not already been downloaded.
# Args
#   DIR: str path to where the ceds data files live
#   ceds_v: str of the release version, I think it might typically be a date
# Returns the str of the full ceds path
find_my_ceds_files <- function(DIR, ceds_v = "v_2025_03_18"){


    assert_that(dir.exists(DIR))

    # Find all the possible ceds files
    all_ceds_files <- list.files(DIR, pattern = "CEDS", recursive = TRUE)
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


# 1. Main Chunk ----------------------------------------------------------------

# Find and import all the data files.
find_my_ceds_files(DIRS$RAW_DATA) %>%
    lapply(read.csv) %>%
    do.call(what = "bind_rows") ->
    ceds_wide_data

# Change from wide to long format.
ceds_wide_data %>%
    pivot_longer(starts_with("X"), names_to = "year") %>%
    # Drop NAs introduced by the pivot.
    na.omit %>%
    mutate(year = as.integer(gsub(replacement = "",
                                  x = year,
                                  pattern = "X"))) %>%
    mutate(source = "CEDS",
           units = format_units_fxn(units)) %>%
    rename(variable = em) ->
    output

# 2. Save Output ---------------------------------------------------------------

output %>%
    check_req_names(req_cols = HEADERS$L0) %>%
    write.csv(file = file.path(DIRS$INTERMED, "L0.CEDS_raw.csv"),
              row.names = FALSE)


output %>%
    filter(variable == "CH4_Extension") %>%
    summarise(value = sum(value), .by = "year") %>%
    ggplot(aes(year, value)) +
    geom_line()
