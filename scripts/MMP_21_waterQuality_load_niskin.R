source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

NISKIN_PATH <- paste0(DATA_PATH, "/primary/niskin/")

## ---- AIMS niskin
writeLines("select l.PROJECT, l.STATION_NAME, l.LOCATION_NAME,
 l.STATION_CLASS, l.LATITUDE, l.LONGITUDE, l.COLLECTION_START_DATE,
 l.AREA_CODE, r.DEPTH_CODE, r.DUPLICATE, r.DEPTH, l.SECCHI_DEPTH, l.ACOUSTIC_DEPTH, 
 r.DIP_QAQC AS DIP_UM, r.SI_QAQC AS SI_UM, r.NH4_QAQC AS NH4_UM, r.NO2_QAQC AS NO2_UM,
 r.NO3_QAQC AS NO3_UM, r.DOC_QAQC AS DOC_UM, r.NH4_INSITU_QAQC AS HAND_NH4_UM, 
 r.PN_QAQC AS PN_UM, r.PP_QAQC AS PP_UM, r.POC_QAQC AS POC_UM, r.SS_QAQC AS TSS_MGPERL,
 r.TDN_PER_QAQC AS TDN_UM, r.TDP_PER_QAQC AS TDP_UM, 
 r.CHL_QAQC AS DRIFTCHL_UGPERL, r.PHAEO_QAQC AS DRIFTPHAE_UGPERL,
 r.PN_SHIM_QAQC AS PN_SHIM_UM, r.DIC_QAQC AS DIC_UMPERKG,
 l.SWELL_HEIGHT, l.WIND_SPEED, l.WIND_DIR, m.mmp_site_name, m.SHORT_NAME, nrm.NRM_REGION
 from MV_SAMPLE_REPLICATE_HORIZ r join STATION_DETAILS l on r.STATION_NAME = l.STATION_NAME 
 join MMP.MMP_SITES m on l.MMP_SITE_ID = m.MMP_SITE_ID
 left join MMP.NRM_REGIONS nrm on nrm.nrm_region_id = m.nrm_region_id
 where l.PROJECT like 'MMP-AIMS'
 AND m.SHORT_NAME is not NULL
 order by r.STATION_NAME, r.DEPTH_CODE, r.DUPLICATE",
 paste0(NISKIN_PATH, "niskin.sql"))

## ## MMP_tryCatch({
##     try({status<-system(paste0("java -jar dbExport.jar ", NISKIN_PATH, "niskin.sql ", NISKIN_PATH, "niskin.csv reef wq_nut2"), intern=TRUE)})

##     ## handle errors
##     ## err <- switch(status,
##     ##               grepl('^Error running query',status)
## print(status) 
## ##     stop('Something is missing')
## ## }, logFile=LOG_FILE, Category='Extracting AIMS nisking data', msg=paste0(''), return=NULL)


## error <- 'no'
## tryCatch({
##     status<-system(paste0("java -jar dbExport.jar ", NISKIN_PATH, "niskin.sql ", NISKIN_PATH, "niskin.csv reef wq_nut2"), intern=TRUE);
##     print(status)
## },
## error = function(e) {'error'},
## warning = function(x) {'warning'}
## )

writeLines("select PROJECTA from STATION_DETAILS where rownum < 10",
 paste0(NISKIN_PATH, "niskin.sql"))
error <- 'no'
tryCatch({
                                        #status<-
                                        #    system(paste0("java -jar dbExport.jar ", NISKIN_PATH, "niskin.sql ", NISKIN_PATH, "niskin.csv reef wq_nut2"), intern=TRUE);
    status <- system2("java",
                      args = paste0("-jar dbExport.jar ", NISKIN_PATH, "niskin.sql ", NISKIN_PATH, "niskin.csv reef wq_nut2"),
                      stdout = TRUE, stderr = TRUE)
    #print(status)
    if (stringr::str_detect(status[6], 'Error')) {
        msg <- stringr::str_replace(status[8], "java.sql.SQLSyntaxErrorException: (.*)", "\\1") 
        stop(msg, call. = FALSE)
        }
                      
                      ## warning('Be')
},
error = function(e) {print(paste0(e))},
warning = function(w) {print(paste0(paste(w, collapse = ''), ' This is a warning warning'))}
)
## print(error)
## error <- 'no'
## withCallingHandlers({
##     status<-system(paste0("java -jar dbExport.jar ", NISKIN_PATH, "niskin.sql ", NISKIN_PATH, "niskin.csv reef wq_nut2"), intern=TRUE);
##     print(status)
## },
## error = function(e) {'error'},
## warning = function(x) {'warning'}
## )

mmp__change_status(stage = "STAGE2", item = "aimsNiskin", status = "success")
## MMP_openning_banner()
## ----end

