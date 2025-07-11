# Description: Format RCMIP inputs.

# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))

# 1. Main Chunk ----------------------------------------------------------------
file <- list.files(DIRS$RAW_DATA,
                   pattern = "rcmip-radiative-forcing-annual-means",
                   full.names = TRUE)
stopifnot(file.exists(file))


# --- Forcing ------------------------------------------------------------------
# Results to save - its unclear how we are going to save the future scenarios...
REGN <- c("World")


read.csv(file) %>%
    filter(Region %in% REGN) ->
    wide_data_forcing

wide_data_forcing %>%
    pivot_longer(names_to = "year", cols = starts_with("X")) %>%
    mutate(year = as.integer(gsub(replacement = "", x = year, pattern = "X"))) %>%
    rename(variable = Variable, units = Unit, sector = Scenario) %>%
    mutate(source = "RCMIP") %>%
    na.omit ->
    forcing_output

output <- forcing_output

# 2. Save Output ---------------------------------------------------------------

output %>%
    check_req_names(req_cols = HEADERS$L0) %>%
    write.csv(file = file.path(DIRS$INTERMED, "L0.RCMIP_raw.csv"),
              row.names = FALSE)


# Z. Quality -------------------------------------------------------------------

output %>%
    filter(variable == "Effective Radiative Forcing|Anthropogenic|Albedo Change") %>%
    ggplot(aes(year, value, color = sector)) +
    geom_line()

