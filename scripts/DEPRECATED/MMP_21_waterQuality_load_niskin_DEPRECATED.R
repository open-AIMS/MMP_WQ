source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

#################################################
## Data extraction logic:                      ##
##                                             ##
## update status and banner                    ##
## if alwaysExtract or data file doesnt exist: ##
##     --> extract data with MMP_tryCatch_db   ##
##         (updates log and status, append     ##
##          filesize)                          ##
## else !alwaysExtract and data file exists:   ##
##     --> update log and status               ##
##     --> append filesize                     ##
## update banner                               ##
#################################################


NISKIN_PATH <- paste0(DATA_PATH, "/primary/niskin/")

## ---- AIMS niskin
CURRENT_ITEM <- "aimsNiskin"
label <- "AIMS niskin"
mmp__change_status(stage = paste0("STAGE",CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()
if (alwaysExtract | !file.exists(paste0(NISKIN_PATH, "niskin", ".csv"))) {
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

    MMP_tryCatch_db(name = 'niskin',
                    stage = paste0("STAGE",CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = current_label,
                    PATH = NISKIN_PATH,
                    db_user = "reef wq_nut2", 
                    progressive = FALSE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing data in ", NISKIN_PATH, "niskin", ".csv"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}

MMP_openning_banner()
## ----end
    
## ---- AIMS Cairns Transect
CURRENT_ITEM <- 'cairnsTransect'
current_label <- "Cairns transect"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()
if (alwaysExtract | !file.exists(paste0(NISKIN_PATH, "cairns", ".csv"))) {
    writeLines("select l.STATION_NAME, l.LOCATION_NAME, l.STATION_CLASS, l.LATITUDE, l.LONGITUDE,
 l.COLLECTION_START_DATE, l.AREA_CODE, r.DEPTH_CODE, r.DUPLICATE, r.DEPTH, l.SECCHI_DEPTH,
 l.ACOUSTIC_DEPTH, 
 r.DIP_QAQC AS DIP_UM, r.SI_QAQC AS SI_UM, r.NH4_QAQC AS NH4_UM, r.NO2_QAQC AS NO2_UM,
 r.NO3_QAQC AS NO3_UM,
 r.DOC_QAQC AS DOC_UM, r.NH4_INSITU AS HAND_NH4_UM, 
 r.PN_QAQC AS PN_UM, r.PP_QAQC AS PP_UM, r.POC_QAQC AS POC_UM, r.SS_QAQC AS TSS_MGPERL,
 r.TDN_PER_QAQC AS TDN_UM, r.TDP_PER_QAQC AS TDP_UM, r.CHL_QAQC AS DRIFTCHL_UGPERL,
 r.PHAEO_QAQC AS DRIFTPHAE_UGPERL, r.PN_SHIM_QAQC AS PN_SHIM_UM, r.DIC_QAQC AS DIC_UMPERKG,
 l.SWELL_HEIGHT, l.WIND_SPEED, l.WIND_DIR, m.mmp_site_name, m.SHORT_NAME, nrm.NRM_REGION
 from MV_SAMPLE_REPLICATE_HORIZ r join STATION_DETAILS l on r.STATION_NAME = l.STATION_NAME 
 join MMP.MMP_SITES m on l.mmp_site_id = m.mmp_site_id
 left join mmp.nrm_regions nrm on m.nrm_region_id = nrm.nrm_region_id
 where l.STATION_CLASS in ('C1','C2','C3','C4','C5','C6','C7','C8','C9','C10','C11')
 order by r.STATION_NAME, r.DEPTH_CODE, r.DUPLICATE",
 paste0(NISKIN_PATH, "cairns.sql"))

    MMP_tryCatch_db(name = 'cairns',
                    stage = paste0("STAGE", CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = current_label,
                    PATH = NISKIN_PATH,
                    db_user = "reef wq_nut2", 
                    progressive = FALSE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing ", current_label, " data (no extraction performed)"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}

MMP_openning_banner()
## ----end

## ---- JCU niskin
CURRENT_ITEM <- "jcuNiskin"
current_label <- "JCU niskin"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()
if (alwaysExtract | !file.exists(paste0(NISKIN_PATH, "jcu", ".csv"))) {
    writeLines("select l.STATION_NAME, l.LOCATION_NAME, l.STATION_CLASS, l.LATITUDE, l.LONGITUDE,
 l.COLLECTION_START_DATE,
 l.AREA_CODE, r.DEPTH_CODE, r.DUPLICATE, r.DEPTH, l.SECCHI_DEPTH, l.ACOUSTIC_DEPTH,
 r.DIP_QAQC AS DIP_UM, r.SI_QAQC AS SI_UM, r.NH4_QAQC AS NH4_UM, r.NO2_QAQC AS NO2_UM,
 r.NO3_QAQC AS NO3_UM, r.DOC_QAQC AS DOC_UM, r.NH4_INSITU AS HAND_NH4_UM, 
 r.PN_QAQC AS PN_UM, r.PP_QAQC AS PP_UM, r.POC_QAQC AS POC_UM, r.SS_QAQC AS TSS_MGPERL,
 r.TDN_PER_QAQC AS TDN_UM, r.TDP_PER_QAQC AS TDP_UM,
 r.CHL_QAQC AS DRIFTCHL_UGPERL, r.PHAEO_QAQC AS DRIFTPHAE_UGPERL, r.PN_SHIM_QAQC AS PN_SHIM_UM,
 r.DIC_QAQC AS DIC_UMPERKG,
 l.SWELL_HEIGHT, l.WIND_SPEED, l.WIND_DIR, m.mmp_site_name, m.SHORT_NAME, nrm.NRM_REGION
 from MV_SAMPLE_REPLICATE_HORIZ r join STATION_DETAILS l on r.STATION_NAME = l.STATION_NAME 
 join MMP.MMP_SITES m on l.MMP_SITE_ID = m.MMP_SITE_ID
 left join MMP.NRM_REGIONS nrm on nrm.nrm_region_id = m.nrm_region_id
 where l.PROJECT in ('MMP-JCU')
  and l.STATION_CLASS in ('JR') 
 order by r.STATION_NAME, r.DEPTH_CODE, r.DUPLICATE",
 paste0(NISKIN_PATH, "jcu.sql"))

    MMP_tryCatch_db(name = 'jcu',
                    stage = paste0("STAGE", CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = current_label,
                    PATH = NISKIN_PATH,
                    db_user = "reef wq_nut2", 
                    progressive = FALSE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing ", current_label, " data (no extraction performed)"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}

MMP_openning_banner()
## ----end

## ---- JCU CY niskin
CURRENT_ITEM <- "jcuCYNiskin"
current_label <- "JCU CY niskin"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()
if (alwaysExtract | !file.exists(paste0(NISKIN_PATH, "cy", ".csv"))) {
    writeLines("select l.STATION_NAME, l.LOCATION_NAME, l.STATION_CLASS, l.LATITUDE, l.LONGITUDE,
 l.COLLECTION_START_DATE,
 l.AREA_CODE, r.DEPTH_CODE, r.DUPLICATE, r.DEPTH, l.SECCHI_DEPTH, l.ACOUSTIC_DEPTH, 
 r.DIP_QAQC AS DIP_UM, r.SI_QAQC AS SI_UM, r.NH4_QAQC AS NH4_UM, r.NO2_QAQC AS NO2_UM,
 r.NO3_QAQC AS NO3_UM, r.DOC_QAQC AS DOC_UM, r.NH4_INSITU AS HAND_NH4_UM, 
 r.PN_QAQC AS PN_UM, r.PN_SHIM_QAQC as PN_SHIM_UM, r.PP_QAQC AS PP_UM, r.POC_QAQC AS POC_UM, r.SS_QAQC AS TSS_MGPERL,
 r.TDN_PER_QAQC AS TDN_UM, r.TDP_PER_QAQC AS TDP_UM,
 r.CHL_QAQC AS DRIFTCHL_UGPERL, r.PHAEO_QAQC AS DRIFTPHAE_UGPERL, 
 r.DIC_QAQC AS DIC_UMPERKG,
 l.SWELL_HEIGHT, l.WIND_SPEED, l.WIND_DIR, m.mmp_site_name, m.SHORT_NAME, nrm.NRM_REGION
 from MV_SAMPLE_REPLICATE_HORIZ r join STATION_DETAILS l on r.STATION_NAME = l.STATION_NAME
 left join MMP.MMP_SITES m on l.MMP_SITE_ID = m.MMP_SITE_ID
 left join MMP.NRM_REGIONS nrm on nrm.nrm_region_id = m.nrm_region_id
 where l.STATION_CLASS in ('JR')
 AND l.PROJECT in ('MMP-CY')
 order by r.STATION_NAME, r.DEPTH_CODE, r.DUPLICATE",
 paste0(NISKIN_PATH, "cy.sql"))

    MMP_tryCatch_db(name = 'cy',
                    stage = paste0("STAGE", CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = current_label,
                    PATH = NISKIN_PATH,
                    db_user = "reef wq_nut2", 
                    progressive = FALSE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing ", current_label, " data (no extraction performed)"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}

MMP_openning_banner()
## ----end

## ---- JCU Event niskin
CURRENT_ITEM <- "jcuEventNiskin"
current_label <- "JCU Event niskin"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()
if (alwaysExtract | !file.exists(paste0(NISKIN_PATH, "jcuEvent", ".csv"))) {
    writeLines("select l.STATION_NAME, l.LOCATION_NAME, l.STATION_CLASS, l.LATITUDE, l.LONGITUDE, l.COLLECTION_START_DATE,
       l.AREA_CODE, r.DEPTH_CODE, r.DUPLICATE, r.DEPTH, l.SECCHI_DEPTH, l.ACOUSTIC_DEPTH,
       r.DIP_QAQC AS DIP_UM, r.SI_QAQC AS SI_UM, r.NH4_QAQC AS NH4_UM, r.NO2_QAQC AS NO2_UM, r.NO3_QAQC AS NO3_UM, r.DOC_QAQC AS DOC_UM, r.NH4_INSITU AS HAND_NH4_UM, 
       r.PN_QAQC AS PN_UM, r.PP_QAQC AS PP_UM, r.POC_QAQC AS POC_UM, r.SS_QAQC AS TSS_MGPERL, r.TDN_PER_QAQC AS TDN_UM, r.TDP_PER_QAQC AS TDP_UM, 
     r.CHL_QAQC AS DRIFTCHL_UGPERL, r.PHAEO_QAQC AS DRIFTPHAE_UGPERL, r.PN_SHIM_QAQC AS PN_SHIM_UM, r.DIC_QAQC AS DIC_UMPERKG,
              l.SWELL_HEIGHT, l.WIND_SPEED, l.WIND_DIR, m.mmp_site_name, m.SHORT_NAME, nrm.NRM_REGION
    from MV_SAMPLE_REPLICATE_HORIZ r join STATION_DETAILS l on r.STATION_NAME = l.STATION_NAME 
        join MMP.MMP_SITES m on l.MMP_SITE_ID = m.MMP_SITE_ID
        left join MMP.NRM_REGIONS nrm on nrm.nrm_region_id = m.nrm_region_id
        where l.STATION_CLASS in ('JE')
        AND l.PROJECT in ('MMP-JCU')
    order by r.STATION_NAME, r.DEPTH_CODE, r.DUPLICATE",
    paste0(NISKIN_PATH, "jcuEvent.sql"))

    MMP_tryCatch_db(name = 'jcuEvent',
                    stage = paste0("STAGE", CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = current_label,
                    PATH = NISKIN_PATH,
                    db_user = "reef wq_nut2", 
                    progressive = FALSE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing ", current_label, " data (no extraction performed)"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}

MMP_openning_banner()
## ----end

## ---- JCU CY Event niskin
CURRENT_ITEM <- "jcuCYEventNiskin"
current_label <- "JCU CY Event niskin"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()
if (alwaysExtract | !file.exists(paste0(NISKIN_PATH, "cyEvent", ".csv"))) {
    writeLines("select l.STATION_NAME, l.LOCATION_NAME, l.STATION_CLASS, l.LATITUDE, l.LONGITUDE, l.COLLECTION_START_DATE,
       l.AREA_CODE, r.DEPTH_CODE, r.DUPLICATE, r.DEPTH, l.SECCHI_DEPTH, l.ACOUSTIC_DEPTH,
       r.DIP_QAQC AS DIP_UM, r.SI_QAQC AS SI_UM, r.NH4_QAQC AS NH4_UM, r.NO2_QAQC AS NO2_UM, r.NO3_QAQC AS NO3_UM, r.DOC_QAQC AS DOC_UM, r.NH4_INSITU AS HAND_NH4_UM, 
       r.PN_QAQC AS PN_UM, r.PP_QAQC AS PP_UM, r.POC_QAQC AS POC_UM, r.SS_QAQC AS TSS_MGPERL, r.TDN_PER_QAQC AS TDN_UM, r.TDP_PER_QAQC AS TDP_UM, 
     r.CHL_QAQC AS DRIFTCHL_UGPERL, r.PHAEO_QAQC AS DRIFTPHAE_UGPERL, r.PN_SHIM_QAQC AS PN_SHIM_UM, r.DIC_QAQC AS DIC_UMPERKG,
              l.SWELL_HEIGHT, l.WIND_SPEED, l.WIND_DIR, m.mmp_site_name, m.SHORT_NAME, nrm.NRM_REGION
    from MV_SAMPLE_REPLICATE_HORIZ r join STATION_DETAILS l on r.STATION_NAME = l.STATION_NAME 
        join MMP.MMP_SITES m on l.MMP_SITE_ID = m.MMP_SITE_ID
        left join MMP.NRM_REGIONS nrm on nrm.nrm_region_id = m.nrm_region_id
        where l.STATION_CLASS in ('JE')
        AND l.PROJECT in ('MMP-CY')
    order by r.STATION_NAME, r.DEPTH_CODE, r.DUPLICATE",
    paste0(NISKIN_PATH, "cyEvent.sql"))

    MMP_tryCatch_db(name = 'cyEvent',
                    stage = paste0("STAGE", CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = current_label,
                    PATH = NISKIN_PATH,
                    db_user = "reef wq_nut2", 
                    progressive = FALSE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing ", current_label, " data (no extraction performed)"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}
MMP_openning_banner()
## ----end
