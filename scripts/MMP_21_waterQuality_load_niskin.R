#################################################
## DATA EXTRACTION LOGIC:                      ##
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

## ---- Start procedure
source("MMP_functions.R")
if (MMP_isParent()) { # if calling application has landed on this script first
    MMP_startMatter() # start initialisations
}
## ----end

## ---- Setup data items
load_stage <- paste0("STAGE", CURRENT_STAGE)
NISKIN_PATH <- paste0(DATA_PATH, "/primary/niskin/")
niskin_items <- c("aimsNiskin", "cairnsTransect", "jcuNiskin", "jcuCYNiskin", "jcuEventNiskin", "jcuCYEventNiskin")
item_properties <- list(
    aimsNiskin = list(
        name = "niskin", # to name files
        label = "AIMS niskin", # for output to console
        db_user = "reef wq_nut2",
        sql = "select l.PROJECT, l.STATION_NAME, l.LOCATION_NAME,
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
 order by r.STATION_NAME, r.DEPTH_CODE, r.DUPLICATE"
    ),
    cairnsTransect = list(
        name = "cairns",
        label = "Cairns transect",
        db_user = "reef wq_nut2",
        sql = "select l.STATION_NAME, l.LOCATION_NAME, l.STATION_CLASS, l.LATITUDE, l.LONGITUDE,
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
 order by r.STATION_NAME, r.DEPTH_CODE, r.DUPLICATE"
    ),
    jcuNiskin = list(
        name = "jcu",
        label = "JCU niskin",
        db_user = "reef wq_nut2",
        sql = "select l.STATION_NAME, l.LOCATION_NAME, l.STATION_CLASS, l.LATITUDE, l.LONGITUDE,
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
 order by r.STATION_NAME, r.DEPTH_CODE, r.DUPLICATE"
    ),
    jcuCYNiskin = list(
        name = "cy",
        label = "JCU CY niskin",
        db_user = "reef wq_nut2",
        sql = "select l.STATION_NAME, l.LOCATION_NAME, l.STATION_CLASS, l.LATITUDE, l.LONGITUDE,
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
 order by r.STATION_NAME, r.DEPTH_CODE, r.DUPLICATE"
    ),
    jcuEventNiskin = list(
        name = "jcuEvent",
        label = "JCU Event niskin",
        db_user = "reef wq_nut2",
        sql = "select l.STATION_NAME, l.LOCATION_NAME, l.STATION_CLASS, l.LATITUDE, l.LONGITUDE, l.COLLECTION_START_DATE,
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
    order by r.STATION_NAME, r.DEPTH_CODE, r.DUPLICATE"
    ),
    jcuCYEventNiskin = list(
        name = "cyEvent",
        label = "JCU CY Event niskin",
        db_user = "reef wq_nut2",
        sql = "select l.STATION_NAME, l.LOCATION_NAME, l.STATION_CLASS, l.LATITUDE, l.LONGITUDE, l.COLLECTION_START_DATE,
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
    order by r.STATION_NAME, r.DEPTH_CODE, r.DUPLICATE"
    )
)
## ----end

## ---- Get data
for (item in niskin_items) {
    # Update status and banner
    mmp__change_status(load_stage, item, status = "progress")
    MMP_openning_banner
    # Get current item properties
    item_name <- item_properties[[item]]$name
    item_label <- item_properties[[item]]$label
    item_db_user <- item_properties[[item]]$db_user
    item_sql <- item_properties[[item]]$sql
    item_sql_file <- paste0(NISKIN_PATH, item_name, ".sql")
    item_data_file <- paste0(NISKIN_PATH, item_name, ".csv")
    # If alwaysExtract or data file doesn't yet exist --> write sql to file and extract data
    if (alwaysExtract | !file.exists(item_data_file)) {
        writeLines(item_sql, item_sql_file)
        MMP_tryCatch_db(
            name = item_name,
            stage = load_stage,
            item = item,
            label = item_label,
            PATH = NISKIN_PATH,
            db_user = item_db_user,
            progressive = FALSE
        )
    } # Otherwise ! alwaysExtract AND data file exists --> update log & status, append file size to banner
    else {
        MMP_log(
            status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste("Using existing", item_label,"data in", item_data_file),
            msg = NULL
        )
        mmp__change_status(load_stage, item, status = "success")
        mmp__append_filesize(load_stage, item, item_label, item_data_file)
    }
    # Update banner
    MMP_openning_banner()
}
## ----end

stop()