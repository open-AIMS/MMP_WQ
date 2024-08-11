source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

CURRENT_ITEM <- "package"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

MMP_tryCatch(
{
    PACKAGE_OUTPUT_PATH <- paste0(DATA_PATH, "/final/") 
    ## ---- niskin data
    NISKIN_INPUT_PATH <- paste0(DATA_PATH, "/processed/niskin/")
    wq.historic <- get(load(file=paste0(NISKIN_INPUT_PATH, 'wq.historic.RData')))
    write_csv(wq.historic, file = paste0(PACKAGE_OUTPUT_PATH, 'wq.historic.csv')) 
    
    wq.all.reef <- get(load(file=paste0(NISKIN_INPUT_PATH, 'wq.all.reef.RData')))
    write_csv(wq.all.reef, file = paste0(PACKAGE_OUTPUT_PATH, 'wq.all.reef.csv')) 
    
    cairns.reef <- get(load(file=paste0(NISKIN_INPUT_PATH, 'cairns.reef.RData')))
    write_csv(cairns.reef, file = paste0(PACKAGE_OUTPUT_PATH, 'cairns.reef.csv')) 
    
    LOGGER_INPUT_PATH <- paste0(DATA_PATH, "/processed/loggers/")
    flntu.all <- get(load(file=paste0(LOGGER_INPUT_PATH, 'flntu.all.RData')))
    write_csv(flntu.all, file = paste0(PACKAGE_OUTPUT_PATH, 'flntu.all.csv')) 

    waterSalinityAll <- get(load(file=paste0(LOGGER_INPUT_PATH, 'waterSalinityAll.RData')))
    write_csv(waterSalinityAll, file = paste0(PACKAGE_OUTPUT_PATH, 'waterSalinityAll.csv')) 

    waterTempWAll <- get(load(file=paste0(LOGGER_INPUT_PATH, 'waterTempWAll.RData')))
    write_csv(waterTempWAll, file = paste0(PACKAGE_OUTPUT_PATH, 'waterTempWAll.csv')) 

    OTHER_INPUT_PATH <- paste0(DATA_PATH, "/processed/other/")
    cyclones.reef <- get(load(file=paste0(OTHER_INPUT_PATH, 'cyclones.reef.RData')))
    write_csv(cyclones.reef, file = paste0(PACKAGE_OUTPUT_PATH, 'cyclones.reef.csv')) 
    
    discharge <- get(load(file=paste0(OTHER_INPUT_PATH, 'discharge.RData')))
    write_csv(discharge, file = paste0(PACKAGE_OUTPUT_PATH, 'discharge.csv')) 

    tides.daily <- get(load(file=paste0(OTHER_INPUT_PATH, 'tides.daily.RData')))
    nms <- names(tides.daily)
    nms[is.na(nms)] <- "Unknown"
    tides.daily <- bind_rows(tides.daily, .id = "MMP_SITE_NAME")
    write_csv(tides.daily, file = paste0(PACKAGE_OUTPUT_PATH, 'tides.daily.csv')) 

    bom <- read_csv(paste0(DATA_PATH, "/primary/other/bom.csv")) 
    bom2 <- read_csv(paste0(DATA_PATH, "/primary/other/bom2.csv")) 
    bom <- bom %>% bind_rows(bom2)
    write_csv(bom, file = paste0(PACKAGE_OUTPUT_PATH, 'bom.csv')) 
    ## write to zip
    
    zip(zipfile=paste0(PACKAGE_OUTPUT_PATH,'processed_data.zip'),
        files=c(paste0(PACKAGE_OUTPUT_PATH, c('wq.historic.csv',
                                              'wq.all.reef.csv',
                                              'cairns.reef.csv',
                                              'flntu.all.csv',
                                              'waterSalinityAll.csv',
                                              'waterTempWAll.csv',
                                              'cyclones.reef.csv',
                                              'discharge.csv',
                                              'tides.daily.csv',
                                              'bom.csv'))
                ## paste0(DATA_PATH, '/primary/other/', c('bom.csv'))
               ),
        flags='-rjoFS')
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Packaging data:', msg='Packaging up all the processed data', return=TRUE)
## ----end

FLNTU_INPUT_PATH <- paste0(DATA_PATH, "/processed/loggers/")
INDICES_OUTPUT_PATH <- paste0(DATA_PATH, "/indices/")
FIGURE_OUTPUT_PATH <- paste0(OUTPUT_PATH, "/figures/indices/")
