source("MMP_functions.R")
## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

assign("CURRENT_STAGE", 6, env = globalenv())
CURRENT_ITEM <- "compilations"

MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                       SECTION = paste0("# ", str_to_title(CURRENT_ITEM), "\n\n"),
                       TABSET = paste0("::: panel-tabset \n\n"),
                       TABSET_END = paste0("::: \n\n")
                       )

source("MMP_61_compilation_plots_gams.R")

source("MMP_62_compilation_plots_gam_pages.R")

source("MMP_63_compilation_plots_zips.R")

source("MMP_35_processedData_report.R")
