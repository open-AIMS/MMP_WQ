source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

LOGGER_PATH <- paste0(DATA_PATH, "/primary/loggers/")
## ---- AIMS FLNTU loggers
CURRENT_ITEM <- "flntu"
current_label <- "AIMS FLNTU loggers"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()
if (alwaysExtract | !file.exists(paste0(LOGGER_PATH, "flntu", ".csv"))) {
    writeLines("select * from ENV_LOGGER.daily_stats
 where VERSION_TAG LIKE 'Final%' and STATION_ID not like 'AIMS'
 and mmp_site_name is not null order by station_id, sample_day",
 paste0(LOGGER_PATH, "flntu.sql"))

    MMP_tryCatch_db(name = 'flntu',
                    stage = paste0("STAGE", CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = current_label,
                    PATH = LOGGER_PATH,
                    db_user = "reef rwqpp_user", 
                    progressive=FALSE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing ", current_label, " data (no extraction performed)"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}

MMP_openning_banner()
## ----end
## ---- AIMS Water temperature loggers
CURRENT_ITEM <- "waterTemp"
current_label <- "Water temperature loggers"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()
if (alwaysExtract | !file.exists(paste0(LOGGER_PATH, "waterTempW", ".csv"))) {
    writeLines("select reefmon_station, location_name, c.parameter_name, a.year, a.weekofyear, a.level1_avg, c.mmp_site_name
  from rtds.v_channels c, rtds.mv_weekly_averages a  
  where c.channel_id = a.channel_id
  and reefmon_station is not null
  and c.parameter_name = 'Water Temperature'"
, paste0(LOGGER_PATH, "waterTempW.sql"))

    MMP_tryCatch_db(name = 'waterTempW',
                    stage = paste0("STAGE", CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = current_label,
                    PATH = LOGGER_PATH,
                    db_user = "rtds rtdsread", 
                    progressive=FALSE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing ", current_label, " data (no extraction performed)"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}

MMP_openning_banner()
## ----end
## ---- AIMS Water salinity loggers
CURRENT_ITEM <- "salinity"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()
if (alwaysExtract | !file.exists(paste0(LOGGER_PATH, "waterSalinity", ".csv"))) {
    writeLines("select station_name, to_char(sample_day_QAQC, 'YYYY-MM-DD') as sample_day,
 parameter, avg_value_qaqc
  from wq_nut2.mv_ctd_time_series_dailies",
 paste0(LOGGER_PATH, "waterSalinity.sql"))

    MMP_tryCatch_db(name = 'waterSalinity',
                    stage = paste0("STAGE", CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = "Salinity loggers",
                    PATH = LOGGER_PATH,
                    db_user = "reef wq_nut2", 
                    progressive=FALSE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing ", current_label, " data (no extraction performed)"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}

MMP_openning_banner()
## ----end