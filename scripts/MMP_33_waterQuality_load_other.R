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

if ((alwaysExtract | !file.exists(paste0(OTHER_OUTPUT_PATH,"disturbance.reef.RData"))) &
    file.exists(paste0(OTHER_INPUT_PATH, 'disturbance_mmp.csv')))
{
    ## 1. Read in data
    ## ---- disturbance data (MMP)
    MMP_tryCatch(
    {
        disturbance_mmp <- read_csv(paste0(OTHER_INPUT_PATH, 'disturbance_mmp.csv')) %>%
            suppressMessages()
        save(disturbance_mmp, file=paste0(OTHER_OUTPUT_PATH, 'disturbance_mmp.RData'))
        unlink(paste0(DATA_PATH, "/reports/STAGE",CURRENT_STAGE, "_", CURRENT_ITEM, "_.RData")) 
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SECTION = paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                                    item = CURRENT_ITEM),"\n\n"),
                               TABSET = paste0("::: panel-tabset \n\n"),
                               TABSET_END = paste0("::: \n\n"),
                               SUBSECTION_SQL_MMP = structure(paste0("## SQL syntax (MMP)\n"),
                                                          parent = 'TABSET'),
                               SQL_MMP = structure(mmp__sql(paste0(OTHER_INPUT_PATH, 'disturbance_mmp.sql')),
                                               parent = 'SUBSECTION_SQL_MMP')
                               )

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_GLIMPSE_MMP = structure(paste0("## Data glimpse (MMP)\n"),
                                                              parent = 'TABSET'),
                               TAB_MMP = structure(mmp__add_table(mmp__glimpse_like(disturbance_mmp)),
                                               parent = 'SUBSECTION_GLIMPSE_MMP'),
                               TAB.CAP_MMP = structure(paste0("\n:Extraction of the first five records in each field from the disturbance (MMP) data. {#tbl-sql-disturbance_mmp}\n\n"),
                                                   parent = 'SUBSECTION_GLIMPSE_MMP')
                              )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing', msg='Reading in disturbance (MMP) data', return=TRUE)
    ## ----end
    
    ## ---- disturbance data (LTMP)
    MMP_tryCatch(
    {
        disturbance_ltmp <- read_csv(paste0(OTHER_INPUT_PATH, 'disturbance_ltmp.csv')) %>%
            suppressMessages()
        save(disturbance_ltmp, file=paste0(OTHER_OUTPUT_PATH, 'disturbance_ltmp.RData'))
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_SQL_LTMP = structure(paste0("## SQL syntax (LTMP)\n"),
                                                          parent = 'TABSET'),
                               SQL_LTMP = structure(mmp__sql(paste0(OTHER_INPUT_PATH, 'disturbance_ltmp.sql')),
                                               parent = 'SUBSECTION_SQL_LTMP'),
                               SUBSECTION_GLIMPSE_LTMP = structure(paste0("## Data glimpse (LTMP)\n"),
                                                              parent = 'TABSET'),
                               TAB_LTMP = structure(mmp__add_table(mmp__glimpse_like(disturbance_ltmp)),
                                               parent = 'SUBSECTION_GLIMPSE_LTMP'),
                               TAB.CAP_LTMP = structure(paste0("\n:Extraction of the first five records in each field from the disturbance (LTMP) data. {#tbl-sql-disturbance_ltmp}\n\n"),
                                                   parent = 'SUBSECTION_GLIMPSE_LTMP')
                              )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing', msg='Reading in disturbance (LTMP) data', return=TRUE)
    ## ----end

    ## ---- disturbance data (cyclones)
    MMP_tryCatch(
    {
        cyclones <- read_csv(paste0(OTHER_INPUT_PATH, 'cyclones.csv')) %>%
            suppressMessages()
        save(cyclones, file=paste0(OTHER_OUTPUT_PATH, 'cyclones.RData'))
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_SQL_CYCLONES = structure(paste0("## SQL syntax (Cyclones)\n"),
                                                          parent = 'TABSET'),
                               SQL_CYCLONES = structure(mmp__sql(paste0(OTHER_INPUT_PATH, 'cyclones.sql')),
                                               parent = 'SUBSECTION_SQL_CYCLONES'),
                               SUBSECTION_GLIMPSE_CYCLONES = structure(paste0("## Data glimpse (Cyclones)\n"),
                                                              parent = 'TABSET'),
                               TAB_CYCLONES = structure(mmp__add_table(mmp__glimpse_like(cyclones)),
                                               parent = 'SUBSECTION_GLIMPSE_CYCLONES'),
                               TAB.CAP_CYCLONES = structure(paste0("\n:Extraction of the first five records in each field from the disturbance (Cyclones) data. {#tbl-sql-cyclones}\n\n"),
                                                   parent = 'SUBSECTION_GLIMPSE_CYCLONES')
                              )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing', msg='Reading in disturbance (Cyclones) data', return=TRUE)
    ## ----end

    ## ---- disturbance data (for plotting)
    MMP_tryCatch(
    {
        disturbanceForPlots <- read_csv(paste0(OTHER_INPUT_PATH, 'disturbanceForPlots.csv')) %>%
            suppressMessages()
        save(disturbanceForPlots, file=paste0(OTHER_OUTPUT_PATH, 'disturbanceForPlots.RData'))
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_SQL_DIST4PLOTS = structure(paste0("## SQL syntax (Disturbances for Plots)\n"),
                                                          parent = 'TABSET'),
                               SQL_DIST4PLOTS = structure(mmp__sql(paste0(OTHER_INPUT_PATH, 'disturbanceForPlots.sql')),
                                               parent = 'SUBSECTION_SQL_DIST4PLOTS'),
                               SUBSECTION_GLIMPSE_DIST4PLOTS = structure(paste0("## Data glimpse (Disturbances for Plots)\n"),
                                                              parent = 'TABSET'),
                               TAB_DIST4PLOTS = structure(mmp__add_table(mmp__glimpse_like(disturbanceForPlots)),
                                               parent = 'SUBSECTION_GLIMPSE_DIST4PLOTS'),
                               TAB.CAP_DIST4PLOTS = structure(paste0("\n:Extraction of the first five records in each field from the disturbance (Disturbances for Plots) data. {#tbl-sql-disturbanceForPlots}\n\n"),
                                                   parent = 'SUBSECTION_GLIMPSE_DIST4PLOTS')
                              )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing', msg='Reading in disturbance (Disturbances for Plots) data', return=TRUE)
    ## ----end

    
    ## 1. First level of data processing
    ## ---- AIMS disturbance (MMP) process level 1
    MMP_tryCatch(
    {
        disturbance.reef.mmp <- disturbance_mmp %>%
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
                  label = "AIMS MMP disturbance",
                  PATH = OTHER_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end

    ## ---- AIMS disturbance (LTMP) process level 1
    MMP_tryCatch(
    {
        disturbance.reef.ltmp <- disturbance_ltmp %>%
            mutate(Date=as.Date(SDATE,format='%d-%b-%Y %H:%M:%S'),
                   ## reef.alias=MMP_SITE_NAME,
                   DISTURBANCE=as.factor(tolower(DISTURBANCE)),
                   DISTURBANCE=dplyr:::recode(DISTURBANCE,u='unk')
                   ) #%>%
                                        #mutate(reef.alias=MMP_reefAlias(.))
        disturbance.reef.ltmp$MMP_SITE_NAME <- ifelse(disturbance.reef.ltmp$P_CODE=='RM' & disturbance.reef.ltmp$MMP_SITE_NAME=='Pandora', 'Pandora North',as.character(disturbance.reef.ltmp$MMP_SITE_NAME))
        disturbance.reef.ltmp$MMP_SITE_NAME <- ifelse(disturbance.reef.ltmp$P_CODE=='RM' & disturbance.reef.ltmp$MMP_SITE_NAME=='Havannah', 'Havannah North',as.character(disturbance.reef.ltmp$MMP_SITE_NAME))

        save(disturbance.reef.ltmp, file=paste0(OTHER_OUTPUT_PATH, 'disturbance.reef.ltmp.RData'))
    }, LOG_FILE, Category = 'Data processing', msg='Initial parsing of disturbance (LTMP) data', return=TRUE)

    MMP_checkData(name = "disturbance.reef.ltmp.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "AIMS disturbance",
                  PATH = OTHER_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end

    ## ---- AIMS disturbance (Cyclones) process level 1
    MMP_tryCatch(
    {
        cyclones.reef <- cyclones %>%
            mutate(STORM_DATE=as.Date(STORM_DATE,format='%d-%b-%Y %H:%M:%S'),
                   ## reef.alias=MMP_SITE_NAME,
                   DISTURBANCE=as.factor(tolower(DISTURBANCE)),
                   DISTURBANCE=dplyr:::recode(DISTURBANCE,u='unk')
                   )# %>%

        save(cyclones.reef, file=paste0(OTHER_OUTPUT_PATH, 'cyclones.reef.RData'))
    }, LOG_FILE, Category = 'Data processing', msg='Initial parsing of disturbance (Cyclones) data', return=TRUE)

    MMP_checkData(name = "cyclones.reef.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "AIMS disturbance",
                  PATH = OTHER_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end
    

    ## 2. Second level of data processing
    ## ---- Disturbance process level 2
    MMP_tryCatch(
    {
        disturbance.reef.ltmp <- disturbance.reef.ltmp %>%
            full_join(cyclones.reef %>% dplyr:::select(-DISTURBANCE))
        ## Combine mmp and ltmp disturbances
        disturbance.reef <- disturbance.reef.mmp %>%
            ## dplyr:::select(-MMP_SITE_NAME) %>%
            full_join(disturbance.reef.ltmp)
        save(disturbance.reef, file=paste0(OTHER_OUTPUT_PATH, 'disturbance.reef.RData'))
        disturbance.reef.core = disturbance.reef %>%
            mutate(MMP_SITE_NAME=gsub(' Island','',MMP_SITE_NAME),
                   MMP_SITE_NAME=gsub(' Reef',' Rf',MMP_SITE_NAME),
                   MMP_SITE_NAME=ifelse(MMP_SITE_NAME=='Fitzroy','Fitzroy West',MMP_SITE_NAME)) %>%
            MMP_region_subregion(Source='disturbance') 
        save(disturbance.reef.core,file=paste0(OTHER_OUTPUT_PATH, "disturbance.reef.core.RData"))
    }, LOG_FILE, Category = 'Data processing', msg='Combining disturbance data', return=TRUE)
             
    MMP_checkData(name = "disturbance.reef.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "AIMS disturbance",
                  PATH = OTHER_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end
    
    ## ---- disturbance outputs
    MMP_tryCatch(
    {
        load(file=paste0(OTHER_OUTPUT_PATH, 'disturbance.reef.RData'))

        p <-
            disturbance.reef %>%
            MMP_region_subregion(Source = 'disturbance') %>%
            filter(!is.na(Subregion),
                   DISTURBANCE %in% c('s','b','c','u','unk','m','f','d')) %>%
            mutate(DISTURBANCE = case_when(
                       DISTURBANCE == 's' ~ 'Storm',
                       DISTURBANCE == 'b' ~ 'Bleaching',
                       DISTURBANCE == 'c' ~ 'COTS',
                       DISTURBANCE == 'f' ~ 'Flooding',
                       DISTURBANCE %in% c('u','unk') ~ 'Unknown',
                       DISTURBANCE == 'd' ~ 'Disease')
                   ) %>%
            mutate(Subregion = factor(Subregion, levels = c('Barron Daintree', 'Johnstone Russell Mulgrave',
                                                           'Tully Herbert', 'Burdekin',
                                                           'Mackay Whitsunday', 'Fitzroy'))) %>%
            ggplot(aes(y = MMP_SITE_NAME, x = Date)) +
            geom_rect(data = NULL, aes(ymin=-Inf,ymax=Inf,xmin=as.Date(paste0(reportYear,'-10-01'))-years(1)+days(1), xmax=as.Date(paste0(reportYear,'-10-01'))), fill='grey', color=NA) +
            geom_point(aes(colour = DISTURBANCE)) +
            facet_grid(Subregion~., space = 'free', scales = 'free') +
            scale_y_discrete('') +
            scale_x_date('',date_breaks='2 years', date_labels='%Y') +
            scale_colour_discrete('Disturbance') +
            theme(strip.background=element_rect(fill=NA,color='black',size=0.5),
                  strip.text.x=element_blank(),
                  panel.border=element_rect(fill=NA,color='black',size=0.5))
        
        ggsave(file=paste0(OUTPUT_PATH, '/figures/processed/disturbance.reef.png'),
               p,
               width=12, height=10, dpi = 100)

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_DESIGN = structure(paste0("## Disturbances recorded\n"),
                                                             parent = 'TABSET'),
                               FIG_REF = structure(paste0("\n::: {#fig-sql-disturbances}\n"),
                                                   parent = 'SUBSECTION_DESIGN'),
                               FIG = structure(paste0("![](",OUTPUT_PATH,"/figures/processed/disturbance.reef.png)\n"),
                                               parent = "FIG_REF"),
                               FIG_CAP = structure(paste0("\nTemporal distribution of recorded disturbances coloured according to disturbance type. Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                                                   parent = 'FIG_REF'),
                               FIG_REF_END = structure(paste0("\n::: \n"),
                                                       parent = 'SUBSECTION_DESIGN')
                              )
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM)
        ## ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% str()
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% unlist() %>% paste(collapse = '')

        
    }, LOG_FILE, Category = "Data processing:", msg='Preparing report outputs for disturbance data', return=TRUE)

    ## ----end
} else {
}

MMP_checkData(name = "disturbance.reef.RData",
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
