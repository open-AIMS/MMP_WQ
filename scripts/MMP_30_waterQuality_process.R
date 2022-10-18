source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

assign("CURRENT_STAGE", 3, env = globalenv())

## - AIMS niskin
## - AIMS Cairns transect
## - JCU niskin
## - JCU CY niskin
## - JCU Event niskin
## - JCU CY Event niskin
source("MMP_31_waterQuality_process_niskin.R")
