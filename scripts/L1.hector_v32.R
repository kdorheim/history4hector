# Description: Format materials from the Hector V3.2 release to be used as
# fill in data if/when needed.

# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))



# 1. Main Chunk ----------------------------------------------------------------

# Read in the ssp245 scenario data
system.file("input/tables/ssp245_emiss-constraints_rf.csv", package = "hector") %>%
    read.csv(comment.char = ";") ->
    wide_results

# Save only a copy of the emissions and the dates
to_keep <- c("Date", colnames(wide_results)[grepl(x = colnames(wide_results), pattern = "emissions")])

# For only the emissions change the formatting of Hector form wide to long,
# for easy plotting comparisons
wide_results[, to_keep] %>%
    pivot_longer(-Date) %>%
    select(year = Date, variable = name, value) %>%
    filter(year <= FINAL_HIST_YEAR) %>%
    # Add the required information for L1
    mutate(source = "HectorV3",
           sector = "ssp245",
           units = getunits(variable)) ->
    output

# 2. Save Output ---------------------------------------------------------------

output %>%
    check_req_names(req_cols = HEADERS$L1) %>%
    write.csv(file = file.path(DIRS$INTERMED, "L1.hectorV32_emiss.csv"),
              row.names = FALSE)


