# Read in R packages and define project constants

# Start from a clean environment
# TODO this would be dropped if written as a function like gcamdata
remove(list = ls())


# 0. Load packages -------------------------------------------------------------
library(assertthat)
library(data.table)
library(dplyr)
library(here)
library(tidyr)
library(zoo)
library(hector)
library(readxl)


# During development let's not set these versions in stone
if (FALSE) {
    # TODO probably use a package manager but for now this is probably good enough
    stopifnot(packageVersion("dplyr") == "1.1.4")
    stopifnot(packageVersion("tidyr") == "1.3.1")
    stopifnot(packageVersion("here") == "1.0.1")
    stopifnot(packageVersion("zoo") == "1.8.12")
}

# packages that are probably not going to be required but could be helpful during
# the developmental stage.
library(ggplot2)

# 1. Set Up Directories --------------------------------------------------------
BASE <-  here::here()
DIRS <- list(
    DATA = file.path(BASE, "data"),
    RAW_DATA = file.path(BASE, "data", "raw-data"),
    MAPPING = file.path(BASE, "data", "mapping"),
    INTERMED = file.path(BASE, "data", "intermed"),
    INPUTS = file.path(BASE, "inputs"),
    TABLES = file.path(BASE, "inputs", "tables")
)

sapply(DIRS,
       dir.create,
       showWarnings = FALSE,
       recursive = TRUE)


# 2. Define Helper Functions ---------------------------------------------------


# Write a hector input table Save the hector csv files into the proper hector format
# Args
#   x: data table containing Hector input values
#   required: str vector of the required variables that must be included in the table
#   write_to: str directory to write the hector csv output to
#   save_as: string of the csv file name to save
# Return: str file name
write_hector_csv <- function(x,
                             required=NULL,
                             write_to,
                             save_as){

    # Format and save the emissions and concentration constraints in the csv files
    # in the proper Hector table input file.
    assert_that(dir.exists(write_to))
    assert_that(has_name(x, c("year", "variable", "units", "value")))
    x <- as.data.table(x)

    # Create the file name
    fname <- file.path(write_to, save_as)

    if(!is.null(required)){
        missing <- !required %in% unique(x[["variable"]])
        assert_that(all(!missing), msg = paste("Missing required variable(s):", paste0(required[missing], collapse = ", ")))
    }

    # Transform the data frame into the wide format that Hector expects.
    input_data <- dcast(as.data.table(x)[, list(Date = year, variable, value)], Date ~ variable)

    # Throw an error if any NAs are included in the data frame.
    #stopifnot(any(is.na(input_data)))

    # Add the header information to the csv table.
    # TODO look into a more efficient way to do this, one that does not
    # require intermediate products to be written out to the disk.
    readr::write_csv(input_data, fname, append = FALSE, col_names = TRUE)
    lines <- readLines(fname)

    # Format a list of units that will be used in the header.
    vars <- names(input_data)
    var_units <- getunits(vars[-1])
    units_list <- paste(c('; UNITS:', var_units), collapse = ', ')

    git_tag <- substr(system("git rev-parse HEAD", intern=TRUE), start = 1, stop = 15)
    create_info <-  c(paste0('; created by history4hector on ', date(),
                             " commit ", git_tag))
    final_lines <- append(c(paste0('; TODO add documenation'),
                            paste0("; commit ", git_tag),
                            paste0("; date ", date()),
                            units_list),
                          lines)
    writeLines(final_lines, fname)
    return(fname)

}







# Format the units string
# Args
#   str: unit string
# Return: units string that should be compatible with the mapping units
format_units_fxn <- function(str) {
    tolower(gsub(
        pattern = " ",
        replacement = "",
        x = str
    ))
}


# Check to make sure the data frame has the required columns
# Args
#   df: data.frame
#   req_cols: str vector of the required column names
# Return: throws an error if missing a required column and returns a
# data frame with only the required columns of data.
check_req_names <- function(df, req_cols) {
    missing <- setdiff(req_cols, names(df))

    if (!length(missing) == 0) {
        x <-
            paste0("df missing required columns: ", missing, collapse = ", ")
        stop(x)

    }


    df %>%
        select(all_of(req_cols)) ->
        df

    return(df)
}

# TODO should generalize to make more useful
# Extend the emissions until 1745
# Args
#   df: data.frame emissions that start in 1750 and need to be extended until 1745
# Return: data.frame that has extended the emissions until 1745
extend_to_1745 <- function(df) {
    # Throw an error if any of the required columns are missing
    req_cols <- c("year", "value")
    stopifnot(all(req_cols %in% names(df)))

    # Determine which of the columns that need to be replicated
    group_cols <- setdiff(names(df), req_cols)

    # Determine which variables do not need to be extended
    df %>%
        filter(year < 1750) %>%
        distinct(variable) ->
        complete_early_yrs

    df %>%
        filter(year == 1750) %>%
        filter(!variable %in% complete_early_yrs$variable) ->
        values_1750

    current_start_yr <- 1750
    yrs <- 1745:(current_start_yr - 1)
    n <- length(yrs)
    rows <- nrow(values_1750)


    # Duplicate the data frame contents and add the years.
    do.call(rbind, replicate(n, values_1750, simplify = FALSE))  %>%
        mutate(year = rep(yrs, each = rows)) %>%
        select(names(df)) ->
        missing_data

    # Add the missing data to the data frame.
    missing_data %>%
        rbind(df) %>%
        arrange(variable, year) ->
        out

    return(out)

}





# Add data for missing years
# Args
#   df: data.frame emissions
#   expected_years: vector of the expected years, that might be missing
#   fill: code used to fill in the missing years, NA, 1 (linear)
# Return: data.frame including entries of all years
add_missing_data <- function(df, expected_years, fill = NA) {
    missing_yrs <- setdiff(expected_years, df$year)

    if (length(missing_yrs) == 0) {
        return(df)
    }


    ids <- setdiff(names(df), c("year", "value"))
    meta_data <- distinct(df[, ids])
    n <- nrow(meta_data)

    replicate(length(missing_yrs), meta_data, simplify = FALSE) %>%
        do.call(what = "rbind") %>%
        mutate(year = rep(missing_yrs, each = n),
               value = NA) ->
        missing_data

    df %>%
        rbind(missing_data) %>%
        arrange(variable, year) ->
        data_w_NAs

    if (is.na(fill)) {
        # If the fill rule is set to NA return the data
        # frame with the NAs.
        return(data_w_NAs)

    } else if (fill == 1) {
        # Following the fill rule of 1 use a linear regression.

        # Categorize by the unique meta data information.
        times <- nrow(data_w_NAs) / nrow(meta_data)
        group_vec <- rep(LETTERS[1:n], each = times)
        df_split <- split(data_w_NAs, group_vec)

        # Linear regression
        lapply(df_split, function(d) {
            to_replace <- d$year[which(is.na(d$value))]
            out <-
                approx(x = d$year,
                       y = d$value,
                       xout = to_replace)
            d$value[which(is.na(d$value))] <- out$y

            return(d)

        }) %>%
            do.call(what = "rbind") ->
            out

        return(out)

    } else {
        stop("capability not implemented yet")
    }

}
# 3. Constants -----------------------------------------------------------------

# The required names for csv written out at different points.
HEADERS <- list()
HEADERS[["L0"]] <- c("source", "variable", "sector", "year", "value", "units")
HEADERS[["L1"]] <- c("variable", "sector", "year", "value", "source")
HEADERS[["L2"]] <- c("variable", "year", "value", "units")



# First year of the input time series.
FIRST_YEAR <- 1745

# Final year of emissions that marks the transition from historical
# to future period
FINAL_HIST_YEAR <- 2022

# Final future year.
FINAL_FUT_YEAR <- 2300

# Historical emissions required for GCAM's historical period before they
# end up being
GCAM_EMISS <- c("ffi_emissions",
                "luc_emissions",
                "daccs_uptake",
                "luc_uptake",
                "BC_emissions",
                "C2F6_emissions",
                "CF4_emissions",
                "CH4_emissions",
                "CO_emissions",
                "HFC125_emissions",
                "HFC134a_emissions",
                "HFC143a_emissions",
                "HFC227ea_emissions",
                "HFC23_emissions",
                "HFC245fa_emissions",
                "HFC32_emissions",
                "N2O_emissions",
                "NH3_emissions",
                "NMVOC_emissions",
                "NOX_emissions",
                "OC_emissions",
                "SF6_emissions",
                "SO2_emissions")

# The emissions/inputs not modeled by GCAM that will need to be defined for
# both the historical and future periods.
NON_GCAM_EMISS <- c("CCl4_emissions",
                    "CFC113_emissions",
                    "CFC114_emissions",
                    "CFC115_emissions",
                    "CFC11_emissions",
                    "CFC12_emissions",
                    "CH3Br_emissions",
                    "CH3CCl3_emissions",
                    "CH3Cl_emissions",
                    "HCFC141b_emissions",
                    "HCFC142b_emissions",
                    "HCFC22_emissions",
                    "HFC4310_emissions",
                    "RF_albedo",
                    "SV",
                    "halon1211_emissions",
                    "halon2402_emissions",
                    "halon1301_emissions",
                    "N2O_natural_emissions")
