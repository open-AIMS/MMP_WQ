source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

if (2 %in% runStage) {
    source("MMP_20_waterQuality_load.R")
}

if (3 %in% runStage) {
    source("MMP_30_waterQuality_process.R")
}
## MMP_test()
