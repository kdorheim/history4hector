# Run all of the scripts!


files <- here::here("scripts", c("L0.CEDS.R",
                                 "L0.BB4CMIP.R",
                                 "L1.CEDS_hector.R",
                                 "L1.BB4CMIP_hector.R",
                                 "L2.aggregate_hector_emissions.R"))

for(f in files){
    print(f)
    source(f)
}
