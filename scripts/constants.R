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
if(FALSE){
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
DIRS <- list(DATA = file.path(BASE, "data"),
             RAW_DATA = file.path(BASE, "data", "raw-data"),
             MAPPING = file.path(BASE, "data", "mapping"),
             INTERMED = file.path(BASE, "data", "intermed"),
             TABLES = file.path(BASE, "inputs", "tables"))

sapply(DIRS, dir.create, showWarnings = FALSE, recursive = TRUE)


# 2. Define Helper Functions ---------------------------------------------------

# Format the units string
# Args
#   str: unit string
# Return: units string that should be compatible with the mapping units
format_units_fxn <- function(str){

    tolower(gsub(pattern = " ", replacement = "", x = str))
}


# Check to make sure the data frame has the required columns
# Args
#   df: data.frame
#   req_cols: str vector of the required column names
# Return: throws an error if missing a required column and returns a
# data frame with only the required columns of data.
check_req_names <- function(df, req_cols){

    missing <- setdiff(req_cols, names(df))

    if(!length(missing) == 0){

        x <- paste0("df missing required columns: ", missing, collapse = ", ")
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
extend_to_1745 <- function(df){

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
    yrs <- 1745:(current_start_yr-1)
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
add_missing_data <- function(df, expected_years, fill = NA){

    missing_yrs <- setdiff(expected_years, df$year)

    if(length(missing_yrs) == 0){
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

    if(is.na(fill)){

        # If the fill rule is set to NA return the data
        # frame with the NAs.
        return(data_w_NAs)

    } else if (fill == 1){

        # Following the fill rule of 1 use a linear regression.

        # Categorize by the unique meta data information.
        times <- nrow(data_w_NAs) / nrow(meta_data)
        group_vec <- rep(LETTERS[1:n], each = times)
        df_split <- split(data_w_NAs, group_vec)

        # Linear regression
        lapply(df_split, function(d){

            to_replace <- d$year[which(is.na(d$value))]
            out <- approx(x = d$year, y = d$value, xout = to_replace)
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

# Final year of emissions data
FINAL_YEAR <- 2022


