# Description: Write the ini file!

# 0. Set Up --------------------------------------------------------------------
# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# actually this does not work since it won't overwrite the natural emissions
# Read in a default infile which does not directly point to any input
# csv table but the place holder "TABLE_NAME"
get_default_ini <- function(){

    fname <- file.path(DIRS$RAW_DATA, "hector_TABLE_NAME.ini")
    out   <- readLines(fname)
    return(out)
}

# 1. Main Chunk ----------------------------------------------------------------

# Ensure that the two csv tables exsit
nongcam_input_file <- "default_emissions.csv"
stopifnot(file.exists(file.path(DIRS$INPUTS, nongcam_input_file)))

gcam_input_file <- "gcam_emissions.csv"
stopifnot(file.exists(file.path(DIRS$INPUTS, gcam_input_file)))


# Load the ini files
ini <- get_default_ini()

# TODO consider writing a function?
nongcam_indx <- which(grepl(x = ini, pattern = paste0(NON_GCAM_EMISS, collapse = "|")))
new_lines <- gsub(x = ini[nongcam_indx], pattern = "TABLE_NAME", replacement = nongcam_input_file)
ini[nongcam_indx] <- new_lines

gcam_indx <- which(grepl(x = ini, pattern = paste0(GCAM_EMISS, collapse = "|")))
new_lines <- gsub(x = ini[gcam_indx], pattern = "TABLE_NAME", replacement = gcam_input_file)
ini[gcam_indx] <- new_lines

# 2. Save ini file -------------------------------------------------------------
# This is just a draft, because Hector parameters still need to be calibrated
# before it can be be used.
writeLines(ini, con = file.path(DIRS$INTERMED, "hector-gcam_draft.ini"))

