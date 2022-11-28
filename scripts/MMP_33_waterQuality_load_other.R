source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

OTHER_INPUT_PATH <- paste0(DATA_PATH, "/primary/other/")
OTHER_OUTPUT_PATH <- paste0(DATA_PATH, "/processed/other/")


## ---- AIMS Disturbance table 
CURRENT_ITEM <- "disturbances"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(OTHER_OUTPUT_PATH,"flntu.all.daily.RData"))) &
    file.exists(paste0(OTHER_INPUT_PATH, 'disturbance.csv'))) {
    MMP_tryCatch(disturbance <- read_csv(paste0(OTHER_INPUT_PATH, 'disturbance.csv')) %>%
                     suppressMessages(),
                 LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing', msg='Reading in disturbance data', return=TRUE)
    
    ## 1. First level of data processing
    ## ---- AIMS disturbance process level 1
    MMP_tryCatch(
    {

        disturbance.reef.mmp = disturbance %>%
            mutate(Date=as.Date(paste(2004+VISIT_NO, '-08-30', sep='')))
        ## reef.alias=MMP_SITE_NAME) #%>%
                                        #mutate(reef.alias=MMP_reefAlias(.))
        wch <- !is.na(as.Date(disturbance.reef.mmp$STORM_DATE, '%d-%b-%Y %H:%M:%S'))
        disturbance.reef.mmp$Date[wch] <-as.Date(disturbance.reef.mmp$STORM_DATE[wch], '%d-%b-%Y %H:%M:%S')
        disturbance.reef.mmp$STORM_DATE <- as.Date(disturbance.reef.mmp$STORM_DATE, '%d-%b-%Y %H:%M:%S')
        
        
        save(disturbance.reef.mmp, file=paste0(OTHER_OUTPUT_PATH, 'disturbance.reef.mmp.RData'))
    }, LOG_FILE, Category = 'Data processing', msg='Initial parsing of disturbance (MMP) data', return=TRUE)

    MMP_checkData(name = "disturbance.reef.mmp.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "AIMS disturbance",
                  PATH = LOGGER_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end

} else {
}

MMP_checkData(name = "disturbance.reef.mmp.RData",
              stage = paste0("STAGE", CURRENT_STAGE),
              item = CURRENT_ITEM,
              label = "Processed AIMS disturbance",
              PATH = OTHER_OUTPUT_PATH)
MMP_openning_banner()

## ----end
## ---- DHW 
## CURRENT_ITEM <- "dhw"
## mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
## MMP_openning_banner()
## ----end
## ---- tides 
CURRENT_ITEM <- "tides"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(OTHER_OUTPUT_PATH,"tides.daily.RData"))) &
    file.exists(paste0(OTHER_INPUT_PATH, 'tides.RData'))) {
    MMP_tryCatch({
        load(file=paste0(OTHER_INPUT_PATH,"tides.RData"))
    }, LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing', msg='Reading in tidal data', return=TRUE)

    ## 1. First level of data processing
    ## ---- tides process level 1
    MMP_tryCatch(
    {
        tides.daily <- MMP_processTides(tides)
        save(tides.daily, file=paste0(OTHER_OUTPUT_PATH, 'tides.daily.RData'))
        rm(tides, tides.daily)
        gc()
    }, LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing', msg='Process tidal data', return=TRUE)
   ## ----end 
} else {
}

MMP_checkData(name = "tides.daily.RData",
              stage = paste0("STAGE", CURRENT_STAGE),
              item = CURRENT_ITEM,
              label = "Processed tidal",
              PATH = OTHER_OUTPUT_PATH)
MMP_openning_banner()

## ----end
## ---- BOM weather data
CURRENT_ITEM <- "BOM"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(OTHER_OUTPUT_PATH,"bom.weather.RData"))) &
    file.exists(paste0(OTHER_INPUT_PATH, 'bom.csv'))) {
    MMP_tryCatch(bom <- read_csv(paste0(OTHER_INPUT_PATH, 'bom.csv')) %>%
                     suppressMessages(),
                 LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing', msg='Reading in BOM weather data', return=TRUE)
    
    ## 1. First level of data processing
    ## ---- BOM weather process level 1
    MMP_tryCatch(
    {
        bom <- bom %>%
            mutate(TEMPERATURE = as.numeric(as.character(TEMPERATURE)), 
                   WIND_SPEED = as.numeric(as.character(WIND_SPEED)), 
                   Date = as.Date(DT, format='%d/%m/%Y')) %>%
            MMP_bomStations() %>%
            mutate(LOCATION = factor(LOCATION))
        
        bom.weather <- bom %>% 
            mutate(waterYear = MMP_waterYear(Date),
                   cwaterYear = MMP_categoricalWaterYear(Date),
                   Dt.num = MMP_decimalDate(Date)
                   ) %>%
            mutate(waterYear = MMP_waterYear(Date),
                   cwaterYear = MMP_categoricalWaterYear(Date), 
                   financialYear = MMP_waterYear(Date), 
                   cfinancialYear = MMP_categoricalWaterYear(Date), 
                   Dt.num <- MMP_decimalDate(Date))
    
        save(bom.weather, file=paste0(OTHER_OUTPUT_PATH, 'bom.weather.RData'))
    }, LOG_FILE, Category = 'Data processing', msg='Initial parsing of BOM (historical weather) data', return=TRUE)
    ## ----end

    
} else {
}

MMP_checkData(name = "bom.weather.RData",
              stage = paste0("STAGE", CURRENT_STAGE),
              item = CURRENT_ITEM,
              label = "Processed BOM weather",
              PATH = OTHER_OUTPUT_PATH)
MMP_openning_banner()

## ----end
## ---- Discharge data 
CURRENT_ITEM <- "discharge"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(OTHER_OUTPUT_PATH,"discharge.annual.RData"))) &
    file.exists(paste0(OTHER_INPUT_PATH, 'discharge.csv'))) {
    MMP_tryCatch(discharge <- read_csv(paste0(OTHER_INPUT_PATH, 'discharge.csv')) %>%
                     suppressMessages(),
                 LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing', msg='Reading in discharge data', return=TRUE)

    ## 1. First level of data processing
    ## ---- discharge process level 1
    MMP_tryCatch(
    {
        discharge <- discharge %>%
            mutate(Date = as.Date(SAMPLE_TIME, '%d-%b-%Y'),
                   Time = as.POSIXct(SAMPLE_TIME, format="%d-%b-%Y %H:%M:%S"), 
                   waterYear = MMP_waterYear(Date), 
                   reneeYear = MMP_reneeYear(Date), 
                   cwaterYear = MMP_categoricalWaterYear(Date), 
                   Dt.num = MMP_decimalDate(Date))
        ## g1 <-ggplot(discharge %>% filter(Date>as.Date('2005-01-01')), aes(y=RIVER_NAME, x=Date))+
        ##     geom_blank() +
        ##     geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=maxDate-years(1)+days(1), xmax=maxDate), fill='grey')  +
        ##     geom_point()+ggtitle('River discharge data') + theme_mmp
        ##                                 #print(g1)
        ## pdf(file=paste0('../output/figures/discharge/discharge.pdf'), width=10, height=10)
        ## print(g1)
        ## dev.off()
        ## png(file=paste0('../output/figures/discharge/discharge.png'), width=10, height=10,units='in',res=300)
        ## print(g1)
        ## dev.off()
        ##                                 #textplot(capture.output(head(discharge)))
        ##                                 #title("River discharge data")

    }, LOG_FILE, Category = 'Data processing', msg='Initial parsing of discharge data', return=TRUE)
    ## ----end

    ## 2. Extract the historic river discharge data
    ## ---- discharge process level 2
    MMP_tryCatch(
    {
        river.lookup<-read.csv(paste0(PARAMS_PATH, "/river.gauge.correction.factors.csv"),
                               strip.white = TRUE)
        discharge<-discharge %>% left_join(river.lookup) %>%
            mutate(Year=ifelse(month(Date) > 9, year(Date)+1, year(Date)))
    }, LOG_FILE, Category = 'Data processing', msg='Discharge gauge correction factors', return=TRUE)

    ## ----end


    ## 3. Further processing 
    ## ---- discharge process level 3
    MMP_tryCatch(
    {
        discharge <- discharge %>%
            mutate(Subregion=
                       ifelse(subregion=='Cape York', 'Cape York',
                       ifelse(subregion=='Daintree', 'Barron Daintree',
                       ifelse(subregion=='Johnstone', 'Johnstone Russell Mulgrave',
                       ifelse(subregion=='Tully', 'Tully Herbert',
                       ifelse(subregion=='Burdekin','Burdekin',
                       ifelse(subregion=='Proserpine','Mackay Whitsunday','Fitzroy')))))))
        discharge <- discharge %>%
            mutate(Region = ifelse(Subregion %in% c('Barron Daintree','Johnstone Russell Mulgrave', 'Tully Herbert'), 'Wet Tropics', as.character(Subregion)))        
        
        joinRiverIDs <- function(id) {
            id <- as.character(id)
            if (length(id)==1) i=as.character(id)
            if (length(id)==2) {
                seq.a<-unlist(strsplit(id[1],split=""))
                seq.b<-unlist(strsplit(id[2],split=""))
                diff.d<-rbind(seq.a,seq.b)
                only.diff<-diff.d[,diff.d[1,]!=diff.d[2,]]
                pos<-which(diff.d[1,]!=diff.d[2,])
                i=paste0(id[1],'/',seq.b[pos])
            }
            i
        }
        discharge.baseLine <- read.csv(paste0(PARAMS_PATH, "/LTmedian.discharge.river.csv"),
                                       strip.white = TRUE)
        discharge.baseLine <- discharge.baseLine %>%
            mutate(River = ifelse(River=="O'Connell River", 'OConnell River',as.character(River)))
        discharge <- discharge %>%
            left_join(discharge.baseLine)
        discharge <- discharge %>%
            filter(waterYear<=reportYear)
        save(discharge, file=paste0(OTHER_OUTPUT_PATH, 'discharge.RData'))
        
        ##generate a version of annual discharge that is aggregated to Subregion for the discharge plots
        discharge.annual <- discharge %>%
            group_by(RIVER_NAME,Subregion,waterYear,correction.factor) %>%
            summarise(discharge=sum(PARAM_VALUE)) %>%
            mutate(discharge.c=discharge*correction.factor) %>% 
            ungroup %>%
            group_by(Subregion,waterYear) %>%
            summarise( discharge.c.annual=sum(discharge.c)) %>%
            ungroup %>%
            mutate(Year=waterYear)
        
        save(discharge.annual, file=paste0(OTHER_OUTPUT_PATH, 'discharge.annual.RData'))
    }, LOG_FILE, Category = 'Data processing', msg='Processing discharge data', return=TRUE)

    ## ----end    
} else {
}

MMP_checkData(name = "discharge.annual.RData",
              stage = paste0("STAGE", CURRENT_STAGE),
              item = CURRENT_ITEM,
              label = "Processed discharge",
              PATH = OTHER_OUTPUT_PATH)
MMP_openning_banner()

## ----end
