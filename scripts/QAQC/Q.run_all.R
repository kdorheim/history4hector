# Run all of the QAQC scripts in order

# 0. Set Up --------------------------------------------------------------------

library(here)

# 1. Main Chunk ----------------------------------------------------------------

# Prep raw data
QAQC_files <- here("scripts", "QAQC", c("Q0A.get_gcam8.8-hectorv3.2.R",
                                        "Q0B.get_dev.R",
                                        "Q1.benchmarking.R",
                                        "Q1.inputs.R"))


for(f in QAQC_files){
    print(f)
    source(f)
}
