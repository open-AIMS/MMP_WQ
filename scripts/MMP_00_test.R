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
    "--alwaysExtract=FALSE"
)
########################################################

## Redefine commandArgs() to return dummy CLAs with test parameters
dummy_args <- c(
    commandArgs()[!grepl('reportYear|runStage|alwaysExtract', commandArgs())],
    test_CLAs
)
commandArgs <- function() dummy_args
print(paste("testing_command> Rscript MMP_00_test.R", paste(test_CLAs, collapse = " ")))

#########################################################

# Run normal operations with test parameters:
source("MMP_functions.R")

# Replace MMP_isParent() ***** NOT 100% CONFIDENT THIS WILL ALWAYS WORK *****
if (sys.nframe() == 4) { # added
# if (MMP_isParent()) {  # removed
    MMP_startMatter()
# }
} else { # added
    print("WARNING: UNEXPECTED BHEAVIOUR > MMP_00_test.R > sys.nframe != 4. This value is set to ensure the script is being called directly from the console.")
    print("To continue edit script at 'if(sys.nframe()==4){...}'")
    stop()
}

if (2 %in% runStage) {
    source("MMP_20_waterQuality_load.R")
}

if (3 %in% runStage) {
    source("MMP_30_waterQuality_process.R")
}
