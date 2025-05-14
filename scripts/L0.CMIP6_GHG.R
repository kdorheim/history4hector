# Description: Read in the CMIP6 era GHG historical concentrations.


# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# 1. Main Chunk ----------------------------------------------------------------

system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv") %>%
    read.csv(comment.char = ";") %>%
    # The cut off of present day during the CMIP6 era.
    filter(Date <= 2014) %>%
    select(year = Date, N2O_concentration = N2O_constrain, CH4_concentration = CH4_constrain) %>%
    pivot_longer(-year, names_to = "variable") %>%
    mutate(units = "ppbv") ->
    output

# 2. Save Output ---------------------------------------------------------------

output %>%
    write.csv(file = file.path(DIRS$INTERMED, "L0.CMIP6_ghgs.csv"),
              row.names = FALSE)
