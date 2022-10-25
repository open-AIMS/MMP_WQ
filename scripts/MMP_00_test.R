########### ENABLES DEBUGGING VIA browser() ############
## Usage:
##   1. open R console
##   2. source("MMP_00_test.R")
##   3. exit console before running again
##
########## TEST PARAMETERS : ###########################
## **EDIT THESE (keep as strings)**
test_CLAs <- c(
    "--reportYear=2022",
    "--runStage=1:2",
    "--alwaysExtract=TRUE"
)
########################################################

## Redefine commandArgs() to return dummy CLAs with test parameters
dummy_args <- c(
    commandArgs()[!grepl('reportYear|runStage|alwaysExtract', commandArgs())],
    test_CLAs
)
commandArgs <- function() dummy_args
print(paste("testing_command> Rscript MMP_00_test.R", paste(test_CLAs, collapse = " ")))

# Run normal operations with test parameters:
source("MMP_functions.R")

# if (MMP_isParent()) { 
    MMP_startMatter()
# }
if (2 %in% runStage) {
    source("MMP_20_waterQuality_load.R")
}

if (3 %in% runStage) {
    source("MMP_30_waterQuality_process.R")
}
