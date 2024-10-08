source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

OTHER_PATH <- paste0(DATA_PATH, "/primary/other/")

## ---- AIMS Disturbance table 
CURRENT_ITEM <- "disturbances"
current_label <- "AIMS Disturbance table"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

## ---- Disturbance MMP
if (alwaysExtract | !file.exists(paste0(OTHER_PATH, "disturbance_mmp", ".csv"))) {
    writeLines("select inshore_mon_reef_name, MMP_SITE_NAME, depth, visit_no, disturbance, storm_date
                    from in_disturbance
                    where inshore_mon_reef_name not like 'Cape%'",
               paste0(OTHER_PATH, "disturbance_mmp.sql"))

    MMP_tryCatch_db(name = 'disturbance_mmp',
                    stage = paste0("STAGE", CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = current_label,
                    PATH = OTHER_PATH,
                    db_user = "reef reefmon", 
                    progressive=FALSE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing ", current_label, " data (no extraction performed)"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}
## ----end

## ---- Disturbance LTMP
if (alwaysExtract | !file.exists(paste0(OTHER_PATH, "disturbance_ltmp", ".csv"))) {
    writeLines("select s.mmp_site_name, d.visit_no, disturbance,p_code, MIN(sample_date) sdate
           from disturbance.disturbances d, v_in_sample s
           where s.fullreef_id = d.fullreef_id
            and p_code ='RM'
           and s.visit_no = d.visit_no
            and d.visit_no>0
           and s.shelf = 'I'
            and s.a_sector in ('CA','TO','WH')
            and sample_type = 'PPOINT'
           group by mmp_site_name, d.visit_no, disturbance,p_code",
               paste0(OTHER_PATH, "disturbance_ltmp.sql"))

    MMP_tryCatch_db(name = 'disturbance_ltmp',
                    stage = paste0("STAGE", CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = current_label,
                    PATH = OTHER_PATH,
                    db_user = "reef reefmon", 
                    progressive=FALSE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing ", current_label, " data (no extraction performed)"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}
## ----end

## ---- Disturbance LTMP Cyclones
if (alwaysExtract | !file.exists(paste0(OTHER_PATH, "cyclones", ".csv"))) {
    writeLines("select MMP_SITE_NAME, d.visit_no, disturbance, name as storm_name, tm as STORM_DATE
           from disturbance.disturbances d, v_in_sample s, disturbance.cyclones c
           where s.fullreef_id = d.fullreef_id
            and d.fullreef_id = c.fullreef_id
            and p_code ='RM'
           and s.visit_no = d.visit_no
            and d.visit_no = c.visit_no
           and d.visit_no>0
           and s.shelf = 'I'
            and s.a_sector in ('CA','TO','WH')
           group by MMP_SITE_NAME, d.visit_no,disturbance, name,tm",
               paste0(OTHER_PATH, "cyclones.sql"))

    MMP_tryCatch_db(name = 'cyclones',
                    stage = paste0("STAGE", CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = current_label,
                    PATH = OTHER_PATH,
                    db_user = "reef reefmon", 
                    progressive=FALSE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing ", current_label, " data (no extraction performed)"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}

## ----end

## ---- Disturbance for plotting
if (alwaysExtract | !file.exists(paste0(OTHER_PATH, "disturbanceForPlots", ".csv"))) {
    writeLines("select catchment, nrm_region, inshore_mon_reef_name, MMP_SITE_NAME, depth, visit_no, disturbance, description, storm_date
  from in_disturbance
  where inshore_mon_reef_name not like 'Cape%'",
               paste0(OTHER_PATH, "disturbanceForPlots.sql"))

    MMP_tryCatch_db(name = 'disturbanceForPlots',
                    stage = paste0("STAGE", CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = current_label,
                    PATH = OTHER_PATH,
                    db_user = "reef reefmon", 
                    progressive=FALSE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing ", current_label, " data (no extraction performed)"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}
## ----end

MMP_openning_banner()
## ----end

## ---- tides 
CURRENT_ITEM <- "tides"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()
if (alwaysExtract | !file.exists(paste0(OTHER_PATH, "tides", ".RData"))) {
    ## ---- tides - generate tide locations
    MMP_tryCatch(
    {
        if (alwaysExtract | !file.exists(paste0(OTHER_PATH, "tidelocations", ".csv"))) {

            file <- paste0(OTHER_PATH, 'tidelocations.csv')
            system(paste("export HFILE_PATH=../parameters/harmonics-2004-06-14.tcd; tide -m l | grep 'Australia' > ",file, sep=""), intern = TRUE)
            tidelocations <-read.fwf(file, widths=c(51,4,8,5,8))  
            tidelocations <- tidelocations %>% dplyr:::select(V1,V3,V5) %>%
                mutate(V3=-1*V3) %>%
                dplyr:::filter(V3 < - 10.61 & V3 > -24.42 & V5 > 142.69 & V5 < 152.3) %>%
                arrange(V3,V5) %>%
                rename(LOCATION=V1, LATITUDE=V3, LONGITUDE=V5)
            save(tidelocations, file=paste0(OTHER_PATH, 'tidelocations.RData'))
        }

        ## MMP_checkData(name = "tidelocations",
        ##               stage = CURRENT_STAGE,
        ##               item = "tides",
        ##               label = "Harmonic tides",
        ##               PATH = OTHER_PATH)
    }, LOG_FILE, Category = "Harmonic tides", msg=' - tide locations generated', return=TRUE)

    if(!file.exists(paste0(OTHER_PATH, "tidelocations", ".csv"))) {
        MMP_log(status = "FAILURE",
                logFile = LOG_FILE,
                Category = paste0("Harmonic tides locations", " data does not exist"),
                msg=NULL) 
        mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "failure")
    }

    ## ----end

    ## ---- tides - select tide locations
    MMP_tryCatch(
    {
        names_lookup <- read_csv(file='../parameters/names_lookup.csv', trim_ws = TRUE) %>%
            suppressMessages()
        tideLookup <- read_csv('../parameters/lookup.csv', trim_ws = TRUE) %>%
            suppressMessages()  %>%
            left_join(names_lookup) %>%
            filter(!is.na(SHORT_NAME) & SHORT_NAME!='') %>%
            dplyr:::select(Location=MMP_SITE_NAME, Reference=Tides) %>%
            as.data.frame() %>%
            suppressMessages()
    }, LOG_FILE, Category = "Harmonic tides", msg=' - tide locations selected', return=TRUE)

    if(!exists("tideLookup")) {
        mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "failure")
    }
    ## ----end

    ## ---- tides - generate harmonic tides 
    MMP_tryCatch(
    {
        ## Establish start and end dates for tidal estimations
        minDate <- "2005-09-01"
        maxDate <- paste0(reportYear, "-08-31")
        t.start <- format(as.POSIXct(minDate)-(10*60*60), '%Y-%m-%d %H:%M') #'2002-01-01 00:00'       
        t.end <- format(as.POSIXct(maxDate)+(2*60*60), '%Y-%m-%d %H:%M')
        tides=list()
        mmp__change_name(CURRENT_STAGE, "tides", paste0("Harmonic tides [", 0, "/ ", length(tideLookup[,'Location']), "]"))
        MMP_openning_banner()
        for (i in 1:length(tideLookup[,'Location'])) { #1:length(tideLookup[,'Location'])) {
            l=gsub(" |\'|\\(|\\)|\\/","_",tideLookup[i,'Location'])
            tides[[as.character(tideLookup[i,1])]]<-MMP_getTides(ref=tideLookup[i,'Reference'], loc=tideLookup[i,'Location'],
                                                                 path=paste0(OTHER_PATH),
                                                                 file=paste0("tides.",l,"A.csv"),    
                                                                 t.start=t.start,
                                                                 t.end=t.end)

            mmp__change_name(paste0("STAGE", CURRENT_STAGE), CURRENT_ITEM, paste0("Harmonic tides [", i, "/ ", length(tideLookup[,'Location']), "]"))
            MMP_openning_banner()
        } 
        mmp__change_name(paste0("STAGE", CURRENT_STAGE), "tides", paste0("Harmonic tides [saving compliation]"))
        save(tides,file=paste0(OTHER_PATH, "tides.RData")) 
        ##load(file=paste0(OTHER_PATH, "tides.RData")) 
        rm(tides)
        gc()
    }, LOG_FILE, Category = "Harmonic tides", msg=' - generate harmonic tides', return=TRUE)

    if(!file.exists(paste0(OTHER_PATH, "tides.RData"))) {
        mmp__change_status(stage = CURRENT_STAGE, item = "tides", status = "failure")
    } else {
        mmp__change_status(stage = CURRENT_STAGE, item = "tides", status = "success")
    }
    ## ----end
    ## ---- tides - delete temporary files
    files <- list.files(path = OTHER_PATH, pattern = "tides.*A.csv", full.names = TRUE)
    unlink(files)
    ## ----end

} else {
    if(!file.exists(paste0(OTHER_PATH, "tides.RData"))) {
        mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "failure")
    } else {
        filesize <- R.utils::hsize(file.size(paste0(OTHER_PATH, "tides.RData")))
        mmp__change_name(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, name = paste0("Harmonic tides", "  [",filesize, "]"))
        mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
    }
}

MMP_openning_banner()
## ----end

## ---- BOM weather data 
CURRENT_ITEM <- "BOM"
current_label <- "BOM weather"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if (alwaysExtract | !file.exists(paste0(OTHER_PATH, "bom", ".csv"))) {
    writeLines("select STATION_NUMBER,TO_CHAR(SAMPLE_DATE,'dd/mm/yyyy') as Dt,
  AVG(TO_CHAR(SAMPLE_DATE, 'yyyy')) as YEAR,
  AVG(AIR_TEMP) as TEMPERATURE,
  AVG(WIND_SPEED_KMH) as WIND_SPEED
  from 
   BOM_CLIMATE.BOM_CLIMATE_DATA_REQUEST where DATA_REQUEST_TYPE like '%HOURLY'
     GROUP BY STATION_NUMBER,TO_CHAR(SAMPLE_DATE,'dd/mm/yyyy')
     ORDER BY STATION_NUMBER,TO_CHAR(SAMPLE_DATE,'dd/mm/yyyy')"
, paste0(OTHER_PATH, "bom.sql"))

    MMP_tryCatch_db(name = 'bom',
                    stage = paste0("STAGE", CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = current_label,
                    PATH = OTHER_PATH,
                    db_user = "reef rwqpp_user", 
                    progressive=TRUE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing ", current_label, " data (no extraction performed)"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}

MMP_openning_banner()
## ----end

## ---- BOM weather data 2
CURRENT_ITEM <- "BOM 2"
current_label <- "BOM weather 2"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if (alwaysExtract | !file.exists(paste0(OTHER_PATH, "bom2", ".csv"))) {
    writeLines("select station_number, station_name, TO_CHAR(sample_day,'YYYY-MM-DD') as SAMPLE_DAY_ISO,
  to_char(sample_day, 'YYYY') as YEAR, parameter, avg_value
 from bom_climate.mv_daily_data_generic
 where (parameter like 'Wind speed%' or parameter like 'Wind direction%' or parameter like 'WIND_SPD_KMH' 
 or parameter like 'WIND_DIR')
 order by station_number, sample_day, parameter"
, paste0(OTHER_PATH, "bom2.sql"))

    MMP_tryCatch_db(name = 'bom2',
                    stage = paste0("STAGE", CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = current_label,
                    PATH = OTHER_PATH,
                    db_user = "reef rwqpp_user", 
                    progressive=TRUE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing ", current_label, " data (no extraction performed)"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}

MMP_openning_banner()
## ----end

## ---- Discharge data 
CURRENT_ITEM <- "discharge"
current_label <- "River discharge"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if (alwaysExtract | !file.exists(paste0(OTHER_PATH, "discharge", ".csv"))) {
    writeLines("SELECT sample_time, param_name,param_value, station_id, station_name, river_name, QUAL_CODE
from nrm_flow.dnrm_data_record r, nrm_flow.dnrm_station s 
where r.dnrm_station_id = s.id
 and s.station_id in ('102102A','104001A','105107A','107003A',
        '108002A','109001A','110001D',
        '111007A','111101D','112004A','112101B',
        '113006A','114001A','116001F','116001E',
        '117002A','119003A','120006B','121003A',
        '122004A','124001B','125007A','126001A','126003A',
        '129001A','130005A')
  and QUAL_CODE < 151
  and PARAM_NAME = 'Discharge (ML/day)'
  and PARAM_AG = 'Mean (Daily)'",
paste0(OTHER_PATH, "discharge.sql"))

    MMP_tryCatch_db(name = 'discharge',
                    stage = paste0("STAGE", CURRENT_STAGE),
                    item = CURRENT_ITEM,
                    label = current_label,
                    PATH = OTHER_PATH,
                    db_user = "reef rwqpp_user", 
                    progressive=FALSE)
} else {
    MMP_log(status = "SUCCESS",
            logFile = LOG_FILE,
            Category = paste0("Using existing ", current_label, " data (no extraction performed)"),
            msg=NULL)
    mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "success")
}

MMP_openning_banner()
## ----end
