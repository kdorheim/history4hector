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
