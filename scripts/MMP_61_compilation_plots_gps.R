source("MMP_functions.R")
source("MMP_functions_models.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}


GAM_OUTPUT_PATH <- paste0(DATA_PATH, "/models/")
PARAMS_INPUT_PATH <- paste0(DATA_PATH, "/primary/other/")

#####
## This script could be drastically reduced with the help of a function
## that takes Subregion and Measure and wq.gam/wq.aims.jcu.gams/etc
#####


## Type 0 index (Full old, historic spatial and temporal formulation) - wq.alt1
## ---- Type 0
CURRENT_ITEM <- "gps"
## mmp__add_status(stage = paste0("STAGE", CURRENT_STAGE),
##                 item = CURRENT_ITEM,
##                 name = "Type 0",
##                 status = "progress")
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"Plots4Angus"))) &
    file.exists(paste0(GAM_OUTPUT_PATH, 'wq_gp.rds')) &
    file.exists(paste0(DATA_PATH, '/indices/wq.alt7.idx.subregion.subindicator.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, 'names_lookup.RData')) 
    ) {

    ## 1. Read in data
    ## ---- Read in data
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.units.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.alt7.idx.subregion.subindicator.RData'))
        wq_gp <- readRDS(file = paste0(GAM_OUTPUT_PATH, "wq_gp.rds"))
        wq.aims.jcu_gp <- readRDS(file = paste0(GAM_OUTPUT_PATH, "wq.aims.jcu_gp.rds"))
        wq.aims.jcu.omo_gp <- readRDS(file = paste0(GAM_OUTPUT_PATH, "wq.aims.jcu.omo_gp.rds"))
        wq.flntu_gp <- readRDS(file = paste0(GAM_OUTPUT_PATH, "wq.flntu_gp.rds"))

        reneeYear.subregion.subindicator.worm <- wq.alt7.idx.subregion.subindicator


    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GPS):', msg='Reading in data.', return=TRUE)
    ## ----end

    ## 2. Plots for Angus
    ## ---- Plots for Angus
    MMP_tryCatch(
    {
        daintree_chl_gp <- wq_gp |>
          filter(Subregion == "Barron Daintree", Measure == "DRIFTCHL") |>
          pull(plot)
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GPS):', msg='Plots for Angus.', return=TRUE)
    ## ----end

    ## 3. Plots for Angus (AIMS + JCU data)
    ## ---- Plots for Angus
    MMP_tryCatch(
    {
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GPS):', msg='AIMS + JCU data.', return=TRUE)
    ## ----end

    ## 4. Plots for Angus (AIMS AND JCU DATA BUT WITH ONLY OPEN COASTAL, MIDSHELF AND OFFSHORE)
    ## ---- Plots for Angus
    MMP_tryCatch(
    {
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GPS):', msg='AIMS + JCU data.', return=TRUE)
    ## ----end
  
    MMP_checkData(name = "Plots4Angus.zip",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.prefix = "Processed",
                  PATH = GAM_OUTPUT_PATH,
                  progressive = FALSE)
    MMP_openning_banner()
}
