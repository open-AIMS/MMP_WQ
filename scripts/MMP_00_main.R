source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

if (2 %in% runStage) {
    source("MMP_20_waterQuality_load.R")
}

## MMP_test()
