# Run all of the scripts! In order to launch this workflow the raw-data must
# be installed. See history4hector/data/raw-data/A.README.md for details.

# 0. Set Up --------------------------------------------------------------------

library(here)

# 2. Main Chunk ----------------------------------------------------------------

# Prep raw data
L0_files <- here("scripts", "L0", c("L0.CEDS.R",
                                    "L0.BB4CMIP.R",
                                    "L0.GCP.R",
                                    "L0.RCMIP.R",
                                    "L0.format_calibration_data.R"))

# Convert to Hector units
L1_files <- here("scripts", "L1", c("L1.CEDS_hector.R",
                                    "L1.BB4CMIP_hector.R",
                                    "L1.GCP_hector.R",
                                    "L1.hector_v32.R",
                                    "L1.RCMIP_hector.R"))

# Create the input csv tables
L2_files <- here("scripts", "L2", c("L2A.gcam_input_csv.R",
                                    "L2B.nongcam_input.R"))

# Finalize gcam-hector inputs, calibrate hector & natural CH4 emissions.
CALIBRATION <- FALSE # Skip over calibration during development.
L3_files    <- here("scripts", "L3", c("L3A.write_draft_ini.R",
                                       "L3B.calibration.R",
                                       "L3C.natural_ch4.R"))

files <- c(L0_files, L1_files, L2_files, L3_files)

for(f in files){
    print(f)
    source(f)
}

