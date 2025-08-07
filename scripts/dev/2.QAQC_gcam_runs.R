# Quick extract data from GCAM output database.
# This is not a reproducible piece of code.

# 0. Set Up --------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(rgcam)


# May need to change the paths!
DIR <- "~/Documents/GCAM-WD/gcam-core/output/"
queryFile <- "/Users/dorh012/Documents/2025/GCAM2Hector/hector-emissions-queries.xml"

# 0. Set Up --------------------------------------------------------------------

# Set up the connection to the output data base.
dir <- file.path(BASE_DIR, "gcam_output")
file <- "database_basexdb"
conn <- localDBConn(dbPath = DIR, file)

# Extract data / create the project.
name <- listScenariosInDB(conn)$name
gcam_data <- addScenario(conn = conn, proj = 'gcam_db.dat', scenario = name, queryFile = queryFile)

# Run the queries
qnames <- listQueries(gcam_data)
queries <- qnames[grepl(pattern = "RF", x = qnames)]

# Save the queries in a single data frame.
lapply(queries, FUN = getQuery, projData = gcam_data) %>%
    rbind ->
    out

total <- getQuery(projData = gcam_data, query = "Climate forcing")

lapply(queries, function(x){

    getQuery(projData = gcam_data, query = x) %>%
        mutate(variable = x)

}) %>%
    do.call(what = "rbind") ->
    out

# Quick visualize the results.
ggplot(out) +
    geom_line(aes(year, value, color = variable))


