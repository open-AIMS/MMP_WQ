#####################################################################################
## GET LOGGER DATA:                                                                ##
##   - extract from database when alwaysExtract=TRUE or data files don't yet exist ##
##   - otherwise extract from MMP_WQ/data/primary/loggers                          ##
#####################################################################################

## ---- Start procedure
source("MMP_functions.R")
if (MMP_isParent()) { # if calling application has landed on this script first
    MMP_startMatter() # start initialisations
}
## ----end

## ---- Setup logger data items
load_stage <- paste0("STAGE", CURRENT_STAGE) # STATUS (list) item corresponding to CURRENT_STAGE (numeric)
LOGGER_PATH <- paste0(DATA_PATH, "/primary/loggers/") # where logger data is to be saved

# Logger datasets
logger_items <- c(
    "flntu",
    "waterTemp",
    "salinity"
)
item_properties <- list(
    flntu = list(
        name = "flntu",
        label = "AIMS FLNTU loggers",
        db_user = "reef rwqpp_user",
        sql = "select * from ENV_LOGGER.daily_stats
 where VERSION_TAG LIKE 'Final%' and STATION_ID not like 'AIMS'
 and mmp_site_name is not null order by station_id, sample_day"
    ),
    waterTemp = list(
        name = "waterTempW",
        label = "Water temperature loggers",
        db_user = "rtds rtdsread",
        sql = "select reefmon_station, location_name, c.parameter_name, a.year, a.weekofyear, a.level1_avg, c.mmp_site_name
  from rtds.v_channels c, rtds.mv_weekly_averages a
  where c.channel_id = a.channel_id
  and reefmon_station is not null
  and c.parameter_name = 'Water Temperature'"
    ),
    salinity = list(
        name = "waterSalinity",
        label = "Salinity loggers",
        db_user = "reef wq_nut2",
        sql = "select station_name, to_char(sample_day_QAQC, 'YYYY-MM-DD') as sample_day,
 parameter, avg_value_qaqc
  from wq_nut2.mv_ctd_time_series_dailies"
    )
)
## ----end

## ---- Get data
for (item in logger_items) {
    # Update status and banner
    mmp__change_status(load_stage, item, status = "progress")
    MMP_openning_banner()
    # Get current item properties
    item_name <- item_properties[[item]]$name
    item_label <- item_properties[[item]]$label
    item_db_user <- item_properties[[item]]$db_user
    item_sql <- item_properties[[item]]$sql
    item_sql_file <- paste0(LOGGER_PATH, item_name, ".sql")
    item_data_file <- paste0(LOGGER_PATH, item_name, ".csv")
    # If alwaysExtract or data file doesn't yet exist --> write sql to file and extract data
    if (alwaysExtract | !file.exists(item_data_file)) {
        writeLines(item_sql, item_sql_file)
        MMP_tryCatch_db(
            name = item_name,
            stage = load_stage,
            item = item,
            label = item_label,
            PATH = LOGGER_PATH,
            db_user = item_db_user,
            progressive = FALSE
        )
    } # Otherwise ! alwaysExtract AND data file exists --> update log & status, append file size to banner
    else {
        existing_data_msg <- paste("Using existing", item_label, "data in", item_data_file)
        MMP_log("SUCCESS", LOG_FILE, Category = existing_data_msg)
        mmp__change_status(load_stage, item, status = "success")
        mmp__append_filesize(load_stage, item, item_label, item_data_file)
    }
    # Update banner
    MMP_openning_banner()
}
## ----end

## FLNTU data is currently being moved from the AIMS oracle database
## to an external threds server. However, for the 2024 report, these
## data are housed on pearl. Geoff has kindly made these available on
## the HPC as an NFS share mounted at:
## /net/cluster1-prod-hpcnfs.aims.gov.au/rwqpp-field-data/FLNTU_QAQCd/NetCDF/
## In order to access this location within the singularity image, we
## will need to bind it to a mount point within the image.

## Unfortunately, the new netcdf files do not contain as much context
## (site names) as the database extracts did. Hence, in order to
## standardise all the fields, it is necessary to bring in additional
## fields from some of the lookup tables

## ---- Get data FLNTU 2024
## flntu_path <- "/net/cluster1-prod-hpcnfs.aims.gov.au/rwqpp-field-data/FLNTU_QAQCd/NetCDF/"
flntu <- read_csv(paste0(LOGGER_PATH, 'flntu.csv')) %>%
            suppressMessages()
flntu_path <- "/home/logger_data/FLNTU_QAQCd/NetCDF"
flntu_files <- list.files(path = flntu_path, pattern = ".*nc", full.names = TRUE)
## flntu_2 <- do.call("rbind", lapply(flntu_files, MMP_read_flntu_nc))
flntu_2 <- MMP_read_flntu_nc(flntu_files)
flntu <- bind_rows(flntu, flntu_2)

data_file <- paste0(LOGGER_PATH, "flntu", ".csv") # save data here
write_csv(flntu, data_file)
## ----

## ---- Get data Salinity 2024
waterSalinity <- read_csv(paste0(LOGGER_PATH, 'waterSalinity.csv')) %>%
            suppressMessages()
salinity_path <- "/home/logger_data/SBE37_QAQCd/NetCDF"
salinity_files <- list.files(path = salinity_path, pattern = ".*nc", full.names = TRUE)
salinity_files <- str_subset(salinity_files, "CSTZ")
salinity_2 <- MMP_read_salinity_nc(salinity_files)
## salinity_2 <- do.call("rbind", lapply(salinity_files, MMP_read_salinity_nc))
waterSalinity <- bind_rows(waterSalinity, salinity_2)
## # **The following is only needed because there are some new entries in the database that
## # should not be there and this is the easiest place to filter them out at this stage**
## waterSalinity <- waterSalinity %>%
##   filter(str_detect(STATION_NAME, "test_file", negate = TRUE))
## # exclude duplicate STATION_NAME/SAMPLE_DAY/PARAMETER values
## waterSalinity <-
##   waterSalinity %>%
##   group_by(STATION_NAME, SAMPLE_DAY, PARAMETER) %>%
##   mutate(n = 1:n()) %>%
##   filter(n == 1) %>%
##   dplyr::select(-n) %>%
##   ungroup()

data_file <- paste0(LOGGER_PATH, "waterSalinity", ".csv") # save data here
write_csv(waterSalinity, data_file)
## ----

## ---- Get data Water Temperature 2024
## flntu_path <- "/net/cluster1-prod-hpcnfs.aims.gov.au/rwqpp-field-data/FLNTU_QAQCd/NetCDF/"
## waterTempW <- read_csv(paste0(LOGGER_PATH, 'waterTempW.csv')) %>%
##             suppressMessages()
## ## temperature data obtained from the Salinity loggers above
## waterSalinity <- bind_rows(waterSalinity, salinity_2)

## data_file <- paste0(LOGGER_PATH, "waterSalinity", ".csv") # save data here
## write_csv(waterSalinity, data_file)
## ----
