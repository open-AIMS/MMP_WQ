## Starting instructions

## 1. start a terminal (shell)
## 2. load the singularity module
##    module load singularity
## 3. run the singularity container (interactively)
##    singularity exec -B .:/home/Project -B /net/cluster1-prod-hpcnfs.aims.gov.au/rwqpp-field-data:/home/logger_data ../mmp.sif R
## 4. source the functions
source("MMP_functions.R")
## 5. run fakeArgs
##    MMP_fakeArgs(stage = 1, always_extract = TRUE, reportYear = 2024) 

## If calling this application interactively, you will need to mimic
## the use of command line arguments.  To do so, call the following:
## MMP_fakeArgs(stage = 1, always_extract = TRUE, reportYear = 2024)

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

if (4 %in% runStage) {
    source("MMP_40_fit_gams.R")
}

if (5 %in% runStage) {
    source("MMP_50_indices.R")
}

if (6 %in% runStage) {
    source("MMP_60_compilation_plots.R")
    ## Dont need the following, they are called from MMP_60_compilation_plots.R
    ## source("MMP_61_compilation_plots_gams.R")
    ## source("MMP_62_compilation_plots_gam_pages.R")
    ## source("MMP_63_compilation_plots_zips.R")
}

if (7 %in% runStage) {
    source("MMP_70_transect_plots.R")
}

if (8 %in% runStage) {
    source("MMP_80_excel_exports.R")
}
if (9 %in% runStage) {
    source("MMP_81_word_exports.R")
}
## MMP_test()
