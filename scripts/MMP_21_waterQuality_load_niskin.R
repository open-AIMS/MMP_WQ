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

MMP_tryCatch_db(name = 'niskin',
                stage = "STAGE2",
                item = "aimsNiskin",
                label = "AIMS niskin")

MMP_openning_banner()
## ----end

