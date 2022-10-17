source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

OTHER_PATH <- paste0(DATA_PATH, "/primary/other/")

## ---- AIMS Disturbance table 
mmp__change_status(stage = "STAGE2", item = "disturbances", status = "progress")
MMP_openning_banner()

if (alwaysExtract | !file.exists(paste0(OTHER_PATH, "disturbance", ".csv"))) {
    writeLines("select inshore_mon_reef_name, MMP_SITE_NAME, depth, visit_no, disturbance, storm_date
                    from in_disturbance
                    where inshore_mon_reef_name not like 'Cape%'",
               paste0(OTHER_PATH, "disturbance.sql"))

    MMP_tryCatch_db(name = 'disturbance',
                    stage = "STAGE2",
                    item = "disturbances",
                    label = "AIMS Disturbance table",
                    PATH = OTHER_PATH,
                    db_user = "reef reefmon")
} else {
    MMP_checkData(name = "disturbance",
                  stage = "STAGE2",
                  item = "disturbances",
                  label = "AIMS Disturbance table",
                  PATH = OTHER_PATH)
}

MMP_openning_banner()
## ----end

## ---- tides 
mmp__change_status(stage = "STAGE2", item = "tides", status = "progress")
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
        ##               stage = "STAGE2",
        ##               item = "tides",
        ##               label = "Harmonic tides",
        ##               PATH = OTHER_PATH)
    }, LOG_FILE, Category = "Harmonic tides", msg=' - tide locations generated', return=TRUE)

    if(!file.exists(paste0(OTHER_PATH, "tidelocations", ".csv"))) {
        MMP_log(status = "FAILURE",
                logFile = LOG_FILE,
                Category = paste0("Harmonic tides locations", " data does not exist"),
                msg=NULL) 
        mmp__change_status(stage = STAGE2, item = "tides", status = "failure")
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
        mmp__change_status(stage = STAGE2, item = "tides", status = "failure")
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
        mmp__change_name("STAGE2", "tides", paste0("Harmonic tides [", 0, "/ ", length(tideLookup[,'Location']), "]"))
        MMP_openning_banner()
        for (i in 1:length(tideLookup[,'Location'])) { #1:length(tideLookup[,'Location'])) {
            l=gsub(" |\'|\\(|\\)|\\/","_",tideLookup[i,'Location'])
            tides[[as.character(tideLookup[i,1])]]<-MMP_getTides(ref=tideLookup[i,'Reference'], loc=tideLookup[i,'Location'],
                                                                 path=paste0(OTHER_PATH),
                                                                 file=paste0("tides.",l,"A.csv"),    
                                                                 t.start=t.start,
                                                                 t.end=t.end)

            mmp__change_name("STAGE2", "tides", paste0("Harmonic tides [", i, "/ ", length(tideLookup[,'Location']), "]"))
            MMP_openning_banner()
        } 
        mmp__change_name("STAGE2", "tides", paste0("Harmonic tides [saving compliation]"))
        save(tides,file=paste0(OTHER_PATH, "tides.RData")) 
        ##load(file=paste0(OTHER_PATH, "tides.RData")) 
        rm(tides)
        gc()
    }, LOG_FILE, Category = "Harmonic tides", msg=' - generate harmonic tides', return=TRUE)

    if(!file.exists(paste0(OTHER_PATH, "tides.RData"))) {
        mmp__change_status(stage = "STAGE2", item = "tides", status = "failure")
    } else {
        mmp__change_status(stage = "STAGE2", item = "tides", status = "success")
    }
    ## ----end
    ## ---- tides - delete temporary files
    files <- list.files(path = OTHER_PATH, pattern = "tides.*A.csv", full.names = TRUE)
    unlink(files)
    ## ----end

} else {
    if(!file.exists(paste0(OTHER_PATH, "tides.RData"))) {
        mmp__change_status(stage = "STAGE2", item = "tides", status = "failure")
    } else {
        filesize <- R.utils::hsize(file.size(paste0(OTHER_PATH, "tides.RData")))
        mmp__change_name(stage = "STAGE2", item = "tides", name = paste0("Harmonic tides", "  [",filesize, "]"))
        mmp__change_status(stage = "STAGE2", item = "tides", status = "success")
    }
}

MMP_openning_banner()
## ----end

## ---- BOM weather data 
mmp__change_status(stage = "STAGE2", item = "BOM", status = "progress")
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
                    stage = "STAGE2",
                    item = "BOM",
                    label = "BOM weather",
                    PATH = OTHER_PATH,
                    db_user = "reef rwqpp_user")
} else {
    MMP_checkData(name = "bom",
                  stage = "STAGE2",
                  item = "BOM",
                  label = "BOM weather",
                  PATH = OTHER_PATH)
}

MMP_openning_banner()
## ----end
