source("MMP_functions.R")
source("MMP_functions_models.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}


GAM_OUTPUT_PATH <- paste0(DATA_PATH, "/models/")
FIGURE_OUTPUT_PATH <- paste0(OUTPUT_PATH, "/figures/models/")

CURRENT_ITEM <- "gam pages zip"
## mmp__add_status(stage = paste0("STAGE", CURRENT_STAGE),
##                 item = CURRENT_ITEM,
##                 name = "Zip",
##                 status = "progress")
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()


MMP_add_to_report_list(CURRENT_STAGE, "Zips",
                       SUBSECTION_0 = structure(paste0("## Type 0\n"),
                                                parent = 'TABSET'),
                       TABSET_0 = structure(paste0("\n:::: panel-tabset\n"),
                                            parent = 'SUBSECTION_0'),
                       TABSET_0_END = structure(paste0("\n:::: \n"),
                                                parent = 'SUBSECTION_0')
                       )

## 1. Add plots to zip for Angus 
## ----Add plots to zip for Angus
MMP_tryCatch(
{
    if(file.exists(paste0(OUTPUT_PATH, "/figures/Plots4Angus.zip")))
        file.remove(paste0(OUTPUT_PATH, "/figures/Plots4Angus.zip"))
    for (s in c('Daintree','Johnstone','Tully','Burdekin','Mackay','Fitzroy')) {
        for (m in c('chl','tss')) {
            system(paste0("zip -rjo '", OUTPUT_PATH, "/figures/Plots4Angus.zip' '",
                          GAM_OUTPUT_PATH, s,"_",m,"_gam.RData'"))
        }
    }

    for (i in c('Barron_Daintree','Johnstone_Russell_Mulgrave',
                'Tully_Herbert','Burdekin','Mackay_Whitsunday','Fitzroy')) {
        system(paste0("zip -rjo '", OUTPUT_PATH, "/figures/Plots4Angus.zip' '",
                      FIGURE_OUTPUT_PATH, "gamm_",i,"_summary.pdf'"))
        system(paste0("zip -rjo '", OUTPUT_PATH, "/figures/Plots4Angus.zip' '",
                      FIGURE_OUTPUT_PATH, "gamm_",i,"_summary.png'"))
        system(paste0("zip -rjo '", OUTPUT_PATH, "/figures/Plots4Angus.zip' '",
                      FIGURE_OUTPUT_PATH, "gamm_",i,".AIMS_JCU_summary.pdf'"))
        system(paste0("zip -rjo '", OUTPUT_PATH, "/figures/Plots4Angus.zip' '",
                      FIGURE_OUTPUT_PATH, "gamm_",i,".AIMS_JCU_summary.png'"))
        system(paste0("zip -rjo '", OUTPUT_PATH, "/figures/Plots4Angus.zip' '",
                      FIGURE_OUTPUT_PATH, "gamm_",i,".AIMS_JCU_OMO_summary.pdf'"))
        system(paste0("zip -rjo '", OUTPUT_PATH, "/figures/Plots4Angus.zip' '",
                      FIGURE_OUTPUT_PATH, "gamm_",i,".AIMS_JCU_OMO_summary.png'"))    
    }
},
LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations:', msg='zips for Angus', return=TRUE)
## ----end

## 2. Add plots to zip for Renee 
## ----Add plots to zip for Renee
MMP_tryCatch(
{
    if(file.exists(paste0(OUTPUT_PATH, "/figures/Plots4Renee.zip")))
        file.remove(paste0(OUTPUT_PATH, "/figures/Plots4Renee.zip"))
    ## BOM weather
    files <- c(
        '../outputs/figures/processed/bom.pdf', 
        '../outputs/figures/processed/bom.png', 
        '../outputs/figures/processed/bom_large.png', 
        '../outputs/figures/processed/bom1.pdf', 
        '../outputs/figures/processed/bom1.png', 
        '../outputs/figures/processed/bom1_large.png'
        )
    ## Time series
    tfiles <- list.files(path = paste0("../outputs/figures/processed"),
                         pattern = "timeseries.*", full.names = TRUE)
    tfiles <- paste0("'", tfiles, "'")
    files <- c(files, tfiles)
    ## Water temperature and salinity
    tfiles <- list.files(path = paste0("../outputs/figures/processed"),
                         pattern = "waterTempSalinity.*|tempSalinity.*", full.names = TRUE)
    tfiles <- paste0("'", tfiles, "'")
    files <- c(files, tfiles)
    ## Dischargey
    tfiles <- list.files(path = paste0("../outputs/figures/processed"),
                         pattern = "discharge.*", full.names = TRUE)
    tfiles <- paste0("'", tfiles, "'")
    files <- c(files, tfiles)
    ## GAMS
    tfiles <- list.files(path = paste0("../outputs/figures/models"),
                         pattern = "gamm.*AIMS_JCU.*summary.*large.*", full.names = TRUE)
    files <- c(files, tfiles)
    ## Indicators
    files <- c(files, paste0("../outputs/figures/indices/",
                    c(
                        'wq_historic_idx_region.pdf',
                        'wq_historic_idx_region_large.png',
                        'wq_historic_idx_region1.pdf',
                        'wq_historic_idx_region1_large.png',
                        'wq_historic_idx_subregion.pdf',
                        'wq_historic_idx_subregion_large.png',
                        'wq_historic_idx_subregion1.pdf',
                        'wq_historic_idx_subregion1_large.png',
                        'wq_old_idx_region.pdf',
                        'wq_old_idx_region_large.png',
                        'wq_old_idx_region1.pdf',
                        'wq_old_idx_region1_large.png',
                        'wq_old_idx_subregion.pdf',
                        'wq_old_idx_subregion_large.png',
                        'wq_old_idx_subregion1.pdf',
                        'wq_old_idx_subregion1_large.png',
                        'wq_alt5_qaqc.pdf',
                        'wq_alt5_qaqc_large.png',
                        'wq_alt5_qaqc1.pdf',
                        'wq_alt5_qaqc1_large.png',
                        'wq_alt5_qaqc2.pdf',
                        'wq_alt5_qaqc2_large.png',
                        'wq_alt5_idx_region.pdf',
                        'wq_alt5_idx_region_large.png',
                        'wq_alt5_idx_region1.pdf',
                        'wq_alt5_idx_region1_large.png',
                        'wq_alt5_idx_subregion.pdf',
                        'wq_alt5_idx_subregion_large.png',
                        'wq_alt5_idx_subregion1.pdf',
                        'wq_alt5_idx_subregion1_large.png',
                        'wq_alt6_qaqc.pdf',
                        'wq_alt6_qaqc_large.png',
                        'wq_alt6_qaqc1.pdf',
                        'wq_alt6_qaqc1_large.png',
                        'wq_alt6_qaqc2.pdf',
                        'wq_alt6_qaqc2_large.png',
                        'wq_alt6_idx_site_measure.pdf',
                        'wq_alt6_idx_site_measure_large.png',
                        'wq_alt6_idx_subregion_measure.pdf',
                        'wq_alt6_idx_subregion_measure_large.png',
                        'wq_alt6_idx_subregion_measure_CI.pdf',
                        'wq_alt6_idx_subregion_measure_CI_large.png',
                        'wq_alt6_idx_region_measure.pdf',
                        'wq_alt6_idx_region_measure_large.png',
                        'wq_alt6_idx_region_measure_CI.pdf',
                        'wq_alt6_idx_region_measure_CI_large.png',
                        'wq_alt6_idx_subregion_subindicator.pdf',
                        'wq_alt6_idx_subregion_subindicator_large.png',
                        'wq_alt6_idx_subregion_subindicator_CI.pdf',
                        'wq_alt6_idx_subregion_subindicator_CI_large.png',
                        'wq_alt6_idx_subregion_indicator.pdf',
                        'wq_alt6_idx_subregion_indicator_large.png',
                        'wq_alt6_idx_subregion_indicator_CI.pdf',
                        'wq_alt6_idx_subregion_indicator_CI_large.png',
                        'wq_alt6_idx_region_indicator.pdf',
                        'wq_alt6_idx_region_indicator_large.png',
                        'wq_alt6_idx_region_indicator_CI.pdf',
                        'wq_alt6_idx_region_indicator_CI_large.png',
                        "wq_worms_comb_region_NoFitzroy.pdf",
                        "wq_worms_comb_region_NoFitzroy_large.png",
                        "wq_worms_comb_region_large.png"
                    )
                    )
               )
    ## Transects
    tfiles <- list.files(path = paste0("../outputs/figures/models"),
                         pattern = "transect.*", full.names = TRUE)
    tfiles <- paste0("'", tfiles, "'")
    files <- c(files, tfiles)
    
    
    files <- paste(files, collapse =' ')
    system(paste0("zip -FSrj -o '", OUTPUT_PATH, "/figures/Plots4Renee.zip' ",files))

},
LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations:', msg='zips for Renee', return=TRUE)
## ----end


## MMP_checkData(name = "Plots4Angus.zip",
##               stage = paste0("STAGE", CURRENT_STAGE),
##               item = CURRENT_ITEM,
##               label.prefix = "Processed",
##               PATH = GAM_OUTPUT_PATH,
##               progressive = FALSE)
MMP_openning_banner()
