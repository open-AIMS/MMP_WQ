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

if (4 %in% runStage) {
    source("MMP_40_fit_gams.R")
}

if (5 %in% runStage) {
    source("MMP_50_indices.R")
}

if (6 %in% runStage) {
    source("MMP_60_compilation_plots.R")
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
