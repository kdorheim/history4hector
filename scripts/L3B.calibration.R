# Calibrate free Hector parameters to observational data sets.

# 0. Set Up --------------------------------------------------------------------
source("scripts/fxns_calibration.R")


# Update a Hector ini with new parameter values
# Args
#   ini_lines: an imported ini hector file
#   params: vector of the new parameter values
# Returns: the updated ini_lines
update_ini <- function(ini_lines, params){

    current_date <- Sys.Date()

    # Should run with no error
    stopifnot(!is.na(getunits(names(params))))
    stopifnot(length(ini_lines) > 1)
    stopifnot(is.character(ini_lines))

    for(i in seq_along(params)){

        # Identify the ini line where the parameter is set.
        p <- params[i]
        pattern <- paste0("^", names(p), "=.*;")
        indx    <- which(grepl(pattern = pattern, x = ini_lines))

        # There should have only been one ini line identified.
        stopifnot(length(indx) == 1)

        # Update the ini lines
        new_line <- paste0(names(p), "=", p[[1]], "          ; calibrated using histroy4hector on ", current_date)
        ini_lines[indx] <- new_line
    }

    return(ini_lines)
}

# 1. Diff, Beta, Q10, MSE  -----------------------------------------------------
if (CALIBRATION){


    # In the first calibration only vary the diff parameter
    ini <- "inputs/hector-gcam.ini"
    core <- newcore_CO2_CH4_N2O(ini, name = "constrainted")

    # Since we are only calibrating an EBM related parameter,
    # only use temp & ohc.
    comparison_data %>%
        filter(variable %in% c(GLOBAL_TAS(), "OHC")) ->
        comp_data


    fxn  <- internal_fn(p = c("diff" = 1),
                        err_fn = obj_E4_unc,
                        obs = comp_data,
                        core = core)

    fit1 <- optim(par = c("diff" = 2), fn = fxn,
                  lower = c(0.5),
                  upper = c(3),
                  method = "L-BFGS-B")



    # Set up the hector core
    ini <- "inputs/hector-gcam.ini"
    core <- newcore_CH4_N2O(ini, name = "contrs")
    name <-  names(fit1$par)
    setvar(core, NA, name, values = fit1$par, unit = getunits(name))
    reset(core)


    # Now calibrate the carbon cycle parameters
    comparison_data %>%
        filter(variable == CONCENTRATIONS_CO2()) ->
        comp_data

    fxn  <- internal_fn(p = c("beta" = 0.1, "q10_rh" = 2),
                        err_fn = obj_MSE,
                        obs = comp_data,
                        core = core)

    fit2 <- optim(par = c("beta" = .53, "q10_rh" = 1.99), fn = fxn,
                  lower = c(0, 1.5),
                  upper = c(1.9, 3),
                  method = "L-BFGS-B")



    # Run Hector
    params_to_use <- round(c(fit1$par, fit2$par), digits = 3)
    write.csv(data.frame(t(params_to_use)), file = file.path(DIRS$INTERMED, "hector_params.csv"), row.names = FALSE)

} else {

    params_to_use <- read.csv(file.path(DIRS$INTERMED, "hector_params.csv"))

}



# 2. Update the ini file -------------------------------------------------------

# Update the ini file with the new parameter values.
ini_path <- file.path(DIRS$INTERMED, "hector-gcam_draft.ini")
stopifnot(file.exists(ini_path))

# Update the Hector ini!
ini_lines <- readLines(con = ini_path)
new_ini   <- update_ini(ini_lines = ini_lines, params = params_to_use)
writeLines(new_ini, con = file.path(DIRS$INPUTS, "hector-gcam.ini"))







