# Description: Scrape information from a Hector ini file.


# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))


# 1. Halocarbon parameters -----------------------------------------------------
ini <- system.file(package = "hector", "input/hector_ssp245.ini")
ini_lines <- readLines(ini)

# Narrow down the ini lines to the section that contains halocarbon data.
halo_index <- which(grepl(pattern = "; Halocarbons", x = ini_lines))
halo_lines <- ini_lines[halo_index:length(ini_lines)]

# Extract the halocarbon name and get the emissions name.
new_block <- which(grepl(pattern = "_halocarbon]", x = halo_lines))
halo_name <- gsub(x = halo_lines[new_block],
                  pattern = "\\[|_halocarbon\\]|\t\t\t\t",
                  replacement = "")
hector_variable <- paste0(halo_name, "_emissions")

# Extract and format tau.
tau_index <- which(grepl(pattern = "tau=", halo_lines))
tau_unformat <- halo_lines[tau_index]
tau1 <- gsub(pattern = "\t.*", replacement = "", x = tau_unformat)
tau2 <- gsub(x = tau1, pattern = "tau=", replacement = "")
tau  <- as.numeric(gsub(x = tau2, pattern = " ", replacement = ""))

# Extract and format the molar mass.
molarMass_unformat <- halo_lines[grepl(pattern = "molarMass=", halo_lines)]
molM1 <- gsub(pattern = "\t.*", replacement = "", x = molarMass_unformat)
molM2 <- gsub(x = molM1, pattern = "molarMass=", replacement = "")
molarMass <- as.numeric(gsub(x = molM2, pattern = " ", replacement = ""))


# Extract the preindustrial concentration, it is assumed to be 0
# unless otherwise specified in Hector's ini file.
H0 <- rep(0, length(molarMass))
H0_unformat <- halo_lines[tau_index+3]
H0_index <- which(grepl(pattern = "H0=", x = H0_unformat))
H01 <- gsub(pattern = "\t.*", replacement = "", x = H0_unformat)
H02 <- gsub(pattern = "H0=", replacement = "", x = H01)
H0[H0_index] <- H02[H0_index]
H0 <- as.numeric(gsub(x = H0, pattern = " ", replacement = ""))

# Save as a data frame.
data.frame(name = halo_name,
           hector_variable = hector_variable,
           tau = tau,
           molarMass = molarMass,
           H0 = H0) ->
    halo_output

# 2. Save Output ---------------------------------------------------------------

halo_output %>%
    write.csv(file = file.path(DIRS$INTERMED, "L0.hector_halocarbon_params.csv"),
              row.names = FALSE)

