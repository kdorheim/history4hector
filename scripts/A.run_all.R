# Run all of the scripts!


L0_scripts <-here::here("scripts", c("L0.BB4CMIP.R",
                                     "L0.CEDS.R",
                                     "L0.Global_Carbon_Project.R",
                                     "L0.PRIMAP.R"))

lapply(L0_scripts, FUN = source)
