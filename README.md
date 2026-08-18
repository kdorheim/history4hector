# history4hector

This R project can be used to prepare the hector input files (both .ini and csv tables) used by gcam during 
the historical period. Note that this was designed on a mac and has not been tested on other machines. 

## Getting started

1. **Clone the Repository** Clone a copy of this repository to your local machine.
2. **Download Raw Data Files** Some of raw data files are too large to be committed to the Git repository and must be downloaded. From your terminal run:

```
# Assuming starting from the ceds4hector root directory
cd data/raw-data 
sh A2.get-raw-data.sh
```

3. **R** 

Other than the bash script to download the raw-data files which only has to be done once this 
work flow is written entirely in R using standard R packages and one custom JGCRI package. 

I've set up a renv file to help with setup, `renv::status()` and/or `renv::restore()` to load the package 
environment.

But I've found renv to sometimes be finicky, so here are some explicit notes on my R version 
the JGCRI package used in history4hector. 

```
R version 4.3.3 (2024-02-29)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS 26.6.1
```

I have also saved a copy of my session information (session_info.txt) if helpful.  

4. **hector** 

Install the correct version of hector by running the following in R

```
remotes::install_github("jgcri/hector@v3.5.0"")
```

This only needs to be run once then `library(hector)` will be calling the correct 
hector version. 


#  Work Flow Description 

To launch the entire work flow source the `scripts/A.run_all.R` file. Note that 
there is a Boolean setting to skip the calibration step (use past parameter values)
or to rerun the calibration protocol. 

The scripts were designed to be able to run individually if needed, this is often 
times helpful during the deg bug/development life cycle. 

## ./scripts/ 

A brief description of the purpose behind each of the script levels. 

| Level|                                                              Description|
|----:|------------------------------------------------------------------------:|
|L0 | Import and format emissions data, (L0 csv files will be in native output units).|
|L1 | Convert from native units to Hector units. | 
|L2 | Use mapping files to aggregate from individual emissions to global totals needed for Hector run. Also calculate and format natural emissions (N2O & CH4), volcanic, and lucc forcing.| 
|L3 | Generate the Hector ini and input csv tables | 


