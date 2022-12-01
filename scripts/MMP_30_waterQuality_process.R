source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

assign("CURRENT_STAGE", 3, env = globalenv())

## assign("DOC_REPORT_LIST", list(), env = globalenv())

## - AIMS niskin
## - AIMS Cairns transect
## - JCU niskin
## - JCU CY niskin
## - JCU Event niskin
## - JCU CY Event niskin
source("MMP_31_waterQuality_process_niskin.R")

## - AIMS FLNTU logggers
## - Water temperature loggers
## - Salinity loggers
source("MMP_32_waterQuality_process_loggers.R")

## - Degree heating weeks
## - Disturbance table
## - tides
## - BOM weather
source("MMP_33_waterQuality_load_other.R")


source("MMP_35_processedData_report.R")
