# Run all of the scripts!


L0_files <- c("L0.CEDS.R",
              "L0.BB4CMIP.R",
              "L0.climate_indicators.R",
              "L0.GCP.R",
              "L0.RCMIP.R")

L1_files <- c("L1.CEDS_hector.R",
              "L1.BB4CMIP_hector.R",
              "L1.GCP_hector.R")

L2_files <- c("L2A.gcam_input_csv.R",
              "L2B.nongcam_input.R")

files <- c(L0_files, L1_files, L2_files)

for(f in files){
    print(f)
    source(here::here("scripts", f))
}
