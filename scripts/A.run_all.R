# Run all of the scripts!


files <- here::here("scripts", c("L0.CEDS.R",
                                 "L1.CEDS_hector.R",
                                 "L2.aggregate_hector_emissions.R"))

for(f in files){
    print(f)
    source(f)
}
