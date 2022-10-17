source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

assign("CURRENT_STAGE", 2, env = globalenv())

## - AIMS niskin
## - AIMS Cairns transect
## - JCU niskin
## - JCU CY niskin
## - JCU Event niskin
## - JCU CY Event niskin
source("MMP_21_waterQuality_load_niskin.R")

## - AIMS FLNTU logggers
## - Water temperature loggers
## - Salinity loggers
## source("MMP_22_waterQuality_load_loggers.R")

## - Degree heating weeks
## - Disturbance table
## - tides
## - BOM weather



