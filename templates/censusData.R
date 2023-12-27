#Census API---------------------------------------------
library(tidycensus)     #census API functions including get_decennial, and getting/setting key

#census and IPUMS APIs require keys
#get a new census key here: https://api.census.gov/data/key_signup.html
#get a new IPUMS key here: https://account.ipums.org/api_keys
#census_api_key("[APIKEY]", install=TRUE)   #Sets census api key and places it in ~/.Renviron for future use
#set_ipums_api_key("[APIKEY]", save = TRUE)  #Same as above, but for IPUMS
readRenviron("~/.Renviron")     #set key from .Renviron file

#create an array of desired variables; possible variables found using load_variables()
dec_vars <- c("P1_003N", "P1_004N", "P1_005N", "P1_006N", "P1_007N", "P1_008N")  #counts of each single race response
acs5_vars <- c("B02001_002", "B02001_003", "B02001_004", "B02001_005", "B02001_006", "B02001_007")

#decennial: results of 10 year census of entire US
#   provides data down to the census tract
#   earliest year through tidycensus is 2000; use IPUMS for earlier
# dec_table <- "p1"  #can request the whole table and manipulate out variable data in code
decennial_data <- get_decennial(
    "tract",            #geography of data (e.g., "tract", "county", "state"),
    variables=dec_vars, #use table="" to get the whole table instead
    cache_table=TRUE,   #cache table names
    year=2020,
    geometry=TRUE,      #response includes simple feature geometry data
    state="wisconsin",  #can use FIPS codes: "55" in place of "wisconsin"
    county="milwaukee" #also FIPS codes: "079" == "milwaukee"
)
head(decennial_data)

#ACS: annual survey of a sample of US households; data availability varies for ACS surveys
#   earliest year through tidycensus is 2005; use IPUMS for earlier
#   acs1: 1-year estimates
#       available for areas where population > 65000
#       useful for short term changes on dense populations
#   acs3: 3-year estimates
#       2007-2013 ONLY
#       available for areas where population > 20000
#       not much use case due to small timeframe that it was in use
#   acs5: 5-year estimates
#       Data down to the block group (level between tract and group)
acs5_data <- get_acs(
    "tract",
    variables=acs5_vars,
    cache_table=TRUE,
    year=2018,
    geometry=TRUE,
    state="wisconsin",
    county="milwaukee"
)
head(acs5_data)

#IPUMS-------------------------------------------
library(ipumsr)

#Get data using API
nhgis_dataset <- ds_spec("1960_tPH", data_tables="NBT5", geog_levels = c("tract"))
nhgis1960 <- define_extract_nhgis(  #First define the data that will be requested
        datasets=nhgis_dataset,
        shapefiles = "us_county_1960_tl2000") %>%
    submit_extract() %>%    #Next submit the request
    wait_for_extract() %>%  #Then wait for the request to comlete; This will halt the program until complete, but only needs to be done once per request
    download_extract(download_dir=file.path(getwd(), "data"))   #Finally, download the data

#NOTE: NHGIS and census use different coordinate systems
#transform coordinate system of NHGIS to match census by piping data through %>% st_transform(4269) %>%
