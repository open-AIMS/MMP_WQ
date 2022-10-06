source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

STATUS[["STAGE2"]][["status"]][1] <- "success"
assign("STATUS", STATUS, env = globalenv())

MMP_openning_banner()
