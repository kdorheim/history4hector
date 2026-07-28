# Run all of the scripts!
# 0. Set Up --------------------------------------------------------------------
# Prep raw data
L0_files <- c("L0.CEDS.R",
              "L0.BB4CMIP.R",
              "L0.GCP.R",
              "L0.RCMIP.R",
              "L0.format_calibration_data.R")

# Convert to Hector units
L1_files <- c("L1.CEDS_hector.R",
              "L1.BB4CMIP_hector.R",
              "L1.GCP_hector.R",
              "L1.hector_v32.R",
              "L1.RCMIP_hector.R")

# Create the input csv tables
L2_files <- c("L2A.gcam_input_csv.R",
              "L2B.nongcam_input.R")

# Finalize gcam-hector inputs, calibrate hector & natural CH4 emissions.
CALIBRATION <- FALSE # Skip over calibration during development.
L3_files    <- c("L3A.write_draft_ini.R",
                 "L3B.calibration.R",
                 "L3C.natural_ch4.R")

L4_files <- c("L4.get_adjustments.R")

files <- c(L0_files, L1_files, L2_files, L3_files, L4_files)

# 1. Run -----------------------------------------------------------------------

for(f in files){
    print(f)
    source(here::here("scripts", f))
}
