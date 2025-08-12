# Final script!
# Confirm that the ini and input materials work for Hector. Compute the
# GMAT_ADJUST and GMSAT_ADJUST values that are needed by GCAM.

# 0. Set Up --------------------------------------------------------------------

source("scripts/constants.R")

# 1. Run historical hector -----------------------------------------------------


ini_path <- list.files(path = DIRS$INPUTS, pattern = "ini", full.names = TRUE)
hc <- newcore(ini_path)
run(hc, runtodate = FINAL_HIST_YEAR)

# 2. Get the reference values --------------------------------------------------

ref_period <- 1850:1900

fetchvars(hc, ref_period, c(GMST(),  GLOBAL_TAS())) %>%
    summarise(value = mean(value), .by = c("variable")) ->
    ref_values

write.csv(ref_values, file = file.path(DIRS$INPUTS, "gcam-hector_temp_ref.csv"),
          row.names = FALSE)


print("all done!")

