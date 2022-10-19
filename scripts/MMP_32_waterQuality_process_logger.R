source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

NISKIN_INPUT_PATH <- paste0(DATA_PATH, "/primary/logger/")
NISKIN_OUTPUT_PATH <- paste0(DATA_PATH, "/processed/logger/")
