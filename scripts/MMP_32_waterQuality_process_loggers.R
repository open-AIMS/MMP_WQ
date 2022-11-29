source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

LOGGER_INPUT_PATH <- paste0(DATA_PATH, "/primary/loggers/")
LOGGER_OUTPUT_PATH <- paste0(DATA_PATH, "/processed/loggers/")

names_lookup <- read_csv(file=paste0(PARAMS_PATH, '/names_lookup.csv')) %>%
    suppressMessages()

## ---- AIMS flntu process
CURRENT_ITEM <- "flntu"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(LOGGER_OUTPUT_PATH,"flntu.all.daily.RData"))) &
    file.exists(paste0(LOGGER_INPUT_PATH, 'flntu.csv'))) {
    MMP_tryCatch(flntu <- read_csv(paste0(LOGGER_INPUT_PATH, 'flntu.csv')) %>%
                     suppressMessages(),
                 LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing', msg='Reading in Water Quality (flntu) data', return=TRUE)
    
    ## 1. First level of data processing
    ## ---- AIMS flntu process level 1
    MMP_tryCatch(
    {
        MAXDATE=as.Date(paste0(reportYear,'-08-31'))
        MINDATE=MAXDATE-years(1)+days(1)
    
        wq.sites <- read.csv(paste0(PARAMS_PATH, '/wq.sites.csv'), strip.white=TRUE)
                                        #flntu<-read.csv(paste0("data/waterQuality/primary/flntu.csv"))
        flntu.all <- flntu %>%
            filter(!is.na(CHL_QA_AVG), !is.na(NTU_QA_AVG)) %>%
            mutate(
                Date=as.Date(SAMPLE_DAY),
                oldSamplingYear=MMP_oldSamplingYear(Date),
                waterYear = MMP_waterYear(Date),
                reneeYear = MMP_reneeYear(Date),
                cwaterYear = MMP_categoricalWaterYear(Date),       
                financialYear = MMP_financialYear(Date),
                cfinancialYear = MMP_categoricalFinancialYear(Date),
                Dt.num = MMP_decimalDate(Date)
            ) %>%
            mutate(reef.alias=ifelse(MMP_SITE_NAME!='',as.character(MMP_SITE_NAME),as.character(STATION_ID))) %>%
            ## dplyr::select(-MMP_SITE_NAME) %>%
                                        #mutate(reef.alias=MMP_reefAlias(.)) %>%  # should not be needed as of 2019
            filter(Date<MAXDATE) %>%
            left_join(wq.sites %>% dplyr::select(SHORT_NAME, GBRMPA_group,LONGITUDE=Longitude,LATITUDE=Latitude)) %>%
            mutate(Month = format(Date,"%b"), Season= factor(ifelse(Month %in% c("Nov","Dec","Jan","Feb","Mar","Apr"), "Wet","Dry"))) %>%
            MMP_designLatest(WQ=TRUE) %>%
            MMP_GBRMPA_specs(WQ=TRUE) %>%
            MMP_region_subregion(Source='FLNTU') %>%
            MMP_reorderReefs() %>%
            MMP_selectReefs(source='flntu') %>% 
            mutate(HistoricReef=MMP_HistoricReef(MMP_SITE_NAME)) %>%
            suppressWarnings() %>%
            suppressMessages()
        
        save(flntu.all, file=paste0(LOGGER_OUTPUT_PATH, 'flntu.all.RData'))
        save(flntu, file=paste0(LOGGER_OUTPUT_PATH, 'flntu.RData'))

        ## print(ggplot(flntu.all %>% mutate(nms=paste0(MMP_SITE_NAME, ' (', SHORT_NAME,')'),
        ##                                   nms=forcats::fct_reorder(nms, LATITUDE)),
        ##              aes(y=(nms), x=Date))+
        ##       geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=MINDATE,xmax=MAXDATE), fill='grey') +
        ##       geom_point()+ggtitle('Water quality FLNTU data')+
        ##       scale_y_discrete('',limits = rev(levels(flntu.all$nms))) +
        ##       scale_x_date('')+
        ##       theme_mmp
        ##       )
        ## textplot(capture.output(head(flntu.all)))
        ## title("FLNTU data")
    }, LOG_FILE, Category = 'Data processing:', msg='Initial parsing of Water Quality (FLNTU) data', return=TRUE)

    MMP_checkData(name = "flntu.all.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "",
                  PATH = LOGGER_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end

    ## 2. Further processing
    ## ---- AIMS flntu process level 2
    MMP_tryCatch(
    {
      load(paste0(LOGGER_OUTPUT_PATH, 'flntu.all.RData'))
      flntu.all.daily <- flntu.all %>%
          group_by(SHORT_NAME, MMP_SITE_NAME, GBRMPA_group,STATION_ID, LATITUDE,Date) %>%
          summarise(NTU_QA_AVG=mean(NTU_QA_AVG, na.rm=TRUE),
                    CHL_QA_AVG=mean(CHL_QA_AVG, na.rm=TRUE)) %>%
          as.data.frame() %>%
          mutate(Month = format(Date,"%b"),
                 Season= factor(ifelse(Month %in% c("Nov","Dec","Jan","Feb","Mar","Apr"), "Wet","Dry"))) %>%
          MMP_designLatest(WQ=TRUE) %>% 
          MMP_GBRMPA_specs(WQ=TRUE) %>%
          MMP_region_subregion() %>% arrange(desc(LATITUDE)) %>% droplevels %>%
          mutate(Label = MMP_locationLabels(MMP_SITE_NAME),
                 Dt.num = as.integer(Date),
                 Mnth = as.integer(format(Date, format='%m')),
                 Subregion=factor(Subregion, levels=unique(Subregion))
                 ) %>%
          mutate(HistoricReef=MMP_HistoricReef(MMP_SITE_NAME))
      
      
      ## pdf(file=paste0('../output/figures/waterQuality/flntu/flntu_all_daily.pdf'), width=12, height=10)
      ## print(p)
      ## dev.off()
      ## png(file=paste0('../output/figures/waterQuality/flntu/flntu_all_daily.png'), width=12, height=10,units='in',res=300)
      ## print(p)
      ## dev.off()
      save(flntu.all.daily, file=paste0(LOGGER_OUTPUT_PATH, 'flntu.all.daily.RData'))
      rm(flntu.all, flntu.all.daily)
      gc()
    }, LOG_FILE, Category = 'Data processing:', msg='Process Water Quality (FLNTU) data', return=TRUE)

    MMP_checkData(name = "flntu.all.daily.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "",
                  PATH = LOGGER_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end

} else {
}

## ---- outputs
MMP_tryCatch(
{
    load(file=paste0(LOGGER_OUTPUT_PATH, 'flntu.RData'))
    load(file=paste0(LOGGER_OUTPUT_PATH, 'flntu.all.daily.RData'))
    
    p=ggplot(flntu.all.daily %>%
             dplyr:::select(SHORT_NAME, MMP_SITE_NAME,Date,Subregion,Season, LATITUDE) %>%
             distinct %>%
             mutate(MMP_SITE_NAME=factor(MMP_SITE_NAME, levels=rev(unique(MMP_SITE_NAME))),
                    nms = paste(MMP_SITE_NAME, '(',SHORT_NAME, ')'),
                    nms = forcats::fct_reorder(nms, LATITUDE)),
             aes(y=(nms), x=Date))+
        geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=as.Date(paste0(reportYear,'-10-01'))-years(1)+days(1), xmax=as.Date(paste0(reportYear,'-10-01'))), fill='grey', color=NA) +
        geom_point(aes(color=Season), shape=16, size=0.1,show.legend=FALSE,position=position_jitter(height=0.5))+
        ggtitle('Water quality FLNTU data')+
        scale_y_discrete('') +
        scale_x_date('',date_breaks='2 years', date_labels='%Y')+
        scale_color_manual('',values=c('red','blue')) +
        facet_grid(Subregion~., scales='free') +
        ggplot2:::theme_grey() +
        theme(strip.background=element_rect(fill=NA,color='black',size=0.5),
              strip.text.x=element_blank(),
              panel.border=element_rect(fill=NA,color='black',size=0.5))
    
    ggsave(file=paste0(OUTPUT_PATH, '/figures/processed/flntu.all.daily.png'),
           p,
           width=12, height=10, dpi = 100)

    MMP_add_to_report(report_list = DOC_REPORT_LIST,
                      content = list(
                          paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                     item = CURRENT_ITEM),"\n\n"),
                          paste0("::: panel-tabset \n\n"),
                          paste0("## SQL syntax\n"),
                          mmp__sql(paste0(LOGGER_INPUT_PATH, 'flntu.sql')),
                          paste0("## Data glimpse\n"),
                          mmp__add_table(mmp__glimpse_like(flntu)),
                          paste0("\n:Extraction of the first five records in each field from the FLNTU data. {#tbl-sql-flntu}\n\n"),
                          paste0("## Sampling design\n"),
                          paste0("\n::: {#fig-sql-flntu}\n"),
                          paste0("![](",OUTPUT_PATH,"/figures/processed/flntu.all.daily.png)\n"),
                          paste0("\nTemporal distribution of AIMS FLNTU water quality samples. Red and blue symbols signify Dry and Wet season samples respectively. Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                          paste0("::: \n"),
                          paste0("::: \n\n")
                      )
                      )
    
    save(DOC_REPORT_LIST, file = paste0(DATA_PATH, "/processed/DOC_REPORT_LIST.RData"))
}, LOG_FILE, Category = "Data processing:", msg='Preparing report outputs for Water Quality (FLNTU) data', return=TRUE)

## ----end

MMP_checkData(name = "flntu.all.daily.RData",
              stage = paste0("STAGE", CURRENT_STAGE),
              item = CURRENT_ITEM,
              label.prefix = "Processed",
              label = "",
              PATH = LOGGER_OUTPUT_PATH)
MMP_openning_banner()

## ----end

## ---- AIMS waterTemp process
CURRENT_ITEM <- "waterTemp"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(LOGGER_OUTPUT_PATH,"waterTempWAll.RData"))) &
    file.exists(paste0(LOGGER_INPUT_PATH, 'waterTempW.csv'))) {
    MMP_tryCatch(waterTempW <- read_csv(paste0(LOGGER_INPUT_PATH, 'waterTempW.csv')) %>%
                     suppressMessages(),
                 LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing:', msg='Reading in Water Temperature data', return=TRUE)

    ## 1. First level of data processing
    ## ---- AIMS waterTemp process level 1
    MMP_tryCatch(
    {
        MAXDATE=as.Date(paste0(reportYear,'-09-30'))
        MINDATE=MAXDATE-years(1)+days(1)
        
        save(waterTempW, file=paste0(LOGGER_OUTPUT_PATH, 'waterTempW_orig.RData'))
        waterTempW <- waterTempW %>%
            left_join(names_lookup %>% dplyr::select(SHORT_NAME, MMP_SITE_NAME) %>% distinct) %>% 
            mutate(Temp = as.numeric(as.character(LEVEL1_AVG)),
                   Date = as.Date(paste(YEAR,WEEKOFYEAR,4,sep='-'), format='%Y-%W-%u'),
                   waterYear = MMP_waterYear(Date),
                   reneeYear = MMP_reneeYear(Date),
                   cwaterYear = MMP_categoricalWaterYear(Date),
                   financialYear = MMP_financialYear(Date),
                   cfinancialYear = MMP_categoricalFinancialYear(Date),
                   Dt.num = MMP_decimalDate(Date)#,
                   ## reef.alias=ifelse(MMP_SITE_NAME!='',as.character(MMP_SITE_NAME),as.character(LOCATION_NAME))
                   ) %>%
            suppressMessages() %>%
            suppressWarnings()
        ## print(ggplot(waterTempW, aes(y=MMP_SITE_NAME, x=Date))+
        ##       geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=MINDATE,xmax=MAXDATE), fill='grey') +
        ##       geom_point()+ggtitle('Water temperature logger')+
        ##       scale_y_discrete('',limits = rev(levels(waterTempW$MMP_SITE_NAME))) +
        ##       scale_x_date('')+
        ##       theme_mmp)
        save(waterTempW, file=paste0(LOGGER_OUTPUT_PATH, 'waterTempW.RData'))
        ## textplot(capture.output(head(waterTempW)))
        ## title("Water temperature logger data (weekly averages)")
    }, LOG_FILE, Category = 'Data processing:', msg='Process Water Quality (weekly water temperature) data (stage 1)', return=TRUE)
    MMP_checkData(name = "waterTempW.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "",
                  PATH = LOGGER_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end

    ## 2. Further processing
    ## ---- AIMS waterTemp process level 2
    MMP_tryCatch(
    {
        ## CHECK THIS NEXT LINE
                                        #waterTempW$reef.alias <- MMP_replaceNames(factor(strtrim(waterTempW$reef.alias, width=str_length(waterTempW$reef.alias)-9)))
        waterTempWAll <-  waterTempW %>%
            MMP_selectReefs(source='WaterTemp') %>%
            MMP_region_subregion(Source='WaterTemp') %>%
            mutate(Month=format(Date,"%b"), Season=factor(ifelse(Month %in% c("Nov","Dec","Jan","Feb","Mar","Apr"), "Wet","Dry"))) %>%
            left_join(wq.sites %>% dplyr:::select(SHORT_NAME, LATITUDE=Latitude)) %>%
            suppressMessages() %>%
            suppressWarnings()
        ## pdf(file=paste0('../output/figures/waterQuality/waterTemperature/waterTempWAll.pdf'), width=10, height=10)
        ## print(p)
        ## dev.off()
        ## png(file=paste0('../output/figures/waterQuality/waterTemperature/waterTempWAll.png'), width=10, height=10,units='in',res=300)
        ## print(p)
        ## dev.off()
        ##                                 #        waterTempW <- MMP_selectReefs(waterTempW,source='WaterTemp')
        ##                                 #        waterTempW <- MMP_region_subregion(waterTempW)
        ##                                 #waterTempW <- MMP_coreReefsPlus(waterTempW, WQ=FALSE)
        ##                                 #waterTempW$Region <- MMP_regions(waterTempW$reef.alias)
        ##                                 #waterTempW$Subregion <- MMP_subregions(waterTempW$reef.alias)
                                        #       waterTempWAll <- MMP_region_subregion(waterTempWAll)
                                        #waterTempWAll$Region <- MMP_regions(waterTempWAll$reef.alias)
                                        #waterTempWAll$Subregion <- MMP_subregions(waterTempWAll$reef.alias)
        save(waterTempWAll, file=paste0(LOGGER_OUTPUT_PATH, 'waterTempWAll.RData'))      
        rm(waterTempW, waterTempWAll)
        gc()
    }, LOG_FILE, Category = 'Data processing:', msg='Process Water Quality (weekly water temperature) data (stage 2)', return=TRUE)

    MMP_checkData(name = "waterTempWAll.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "",
                  PATH = LOGGER_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    
    ## ----end
} else{
}

## ---- outputs
MMP_tryCatch(
{
    load(file=paste0(LOGGER_OUTPUT_PATH, 'waterTempW_orig.RData'))
    load(file=paste0(LOGGER_OUTPUT_PATH, 'waterTempWAll.RData'))
    
    p=ggplot(waterTempWAll %>% dplyr:::select(MMP_SITE_NAME,LATITUDE,Date,Subregion,Season) %>% distinct %>% arrange(desc(LATITUDE)) %>% mutate(Subregion=factor(Subregion, levels=unique(Subregion)), MMP_SITE_NAME=factor(MMP_SITE_NAME,levels=rev(unique(MMP_SITE_NAME)))), aes(y=MMP_SITE_NAME, x=Date))+
            geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=MINDATE,xmax=MAXDATE), fill='grey') +
            geom_point(aes(color=Season),position=position_jitter(height=0.5),size=0.1,show.legend=FALSE)+ggtitle('Water temperature logger')+
            scale_y_discrete('',limits = rev(levels(waterTempW$MMP_SITE_NAME))) +
            scale_x_date('',date_breaks='2 years', date_labels='%Y')+
            scale_color_manual('',values=c('red','blue')) +
            facet_grid(Subregion~., scales='free',space='free') +
            theme_mmp + theme(strip.background=element_rect(fill=NA,color='black',size=0.5),
                              strip.text.x=element_blank(),
                              panel.border=element_rect(fill=NA,color='black',size=0.5))
        
    ## p=ggplot(flntu.all.daily %>%
    ##          dplyr:::select(SHORT_NAME, MMP_SITE_NAME,Date,Subregion,Season, LATITUDE) %>%
    ##          distinct %>%
    ##          mutate(MMP_SITE_NAME=factor(MMP_SITE_NAME, levels=rev(unique(MMP_SITE_NAME))),
    ##                 nms = paste(MMP_SITE_NAME, '(',SHORT_NAME, ')'),
    ##                 nms = forcats::fct_reorder(nms, LATITUDE)),
    ##          aes(y=(nms), x=Date))+
    ##     geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=as.Date(paste0(reportYear,'-10-01'))-years(1)+days(1), xmax=as.Date(paste0(reportYear,'-10-01'))), fill='grey', color=NA) +
    ##     geom_point(aes(color=Season), shape=16, size=0.1,show.legend=FALSE,position=position_jitter(height=0.5))+
    ##     ggtitle('Water quality FLNTU data')+
    ##     scale_y_discrete('') +
    ##     scale_x_date('',date_breaks='2 years', date_labels='%Y')+
    ##     scale_color_manual('',values=c('red','blue')) +
    ##     facet_grid(Subregion~., scales='free') +
    ##     ggplot2:::theme_grey() +
    ##     theme(strip.background=element_rect(fill=NA,color='black',size=0.5),
    ##           strip.text.x=element_blank(),
    ##           panel.border=element_rect(fill=NA,color='black',size=0.5))
    
    ggsave(file=paste0(OUTPUT_PATH, '/figures/processed/waterTempWAll.png'),
           p,
           width=12, height=10, dpi = 100)

    MMP_add_to_report(report_list = DOC_REPORT_LIST,
                      content = list(
                          paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                     item = CURRENT_ITEM),"\n\n"),
                          paste0("::: panel-tabset \n\n"),
                          paste0("## SQL syntax\n"),
                          mmp__sql(paste0(LOGGER_INPUT_PATH, 'waterTemp.sql')),
                          paste0("## Data glimpse\n"),
                          mmp__add_table(mmp__glimpse_like(waterTempW_orig)),
                          paste0("\n:Extraction of the first five records in each field from the Water Temperature data. {#tbl-sql-waterTemp}\n\n"),
                          paste0("## Sampling design\n"),
                          paste0("\n::: {#fig-sql-waterTemp}\n"),
                          paste0("![](",OUTPUT_PATH,"/figures/processed/waterTempWAll.png)\n"),
                          paste0("\nTemporal distribution of AIMS Water Temperature samples. Red and blue symbols signify Dry and Wet season samples respectively. Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                          paste0("::: \n"),
                          paste0("::: \n\n")
                      )
                      )
    
    save(DOC_REPORT_LIST, file = paste0(DATA_PATH, "/processed/DOC_REPORT_LIST.RData"))
}, LOG_FILE, Category = "Data processing:", msg='Preparing report outputs for Water Quality (Water Temperature) data', return=TRUE)

## ----end

MMP_checkData(name = "waterTempWAll.RData",
              stage = paste0("STAGE", CURRENT_STAGE),
              item = CURRENT_ITEM,
              label.prefix = "Processed",
              label = "",
              PATH = LOGGER_OUTPUT_PATH)
MMP_openning_banner()
    
## ----end

## ---- AIMS waterSalinity process
CURRENT_ITEM <- "salinity"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(LOGGER_OUTPUT_PATH,"waterSalinityAll.RData"))) &
    file.exists(paste0(LOGGER_INPUT_PATH, 'waterSalinity.csv'))) {
    MMP_tryCatch(waterSalinity <- read_csv(paste0(LOGGER_INPUT_PATH, 'waterSalinity.csv')) %>%
                     suppressMessages(),
                 LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing', msg='Reading in Water Salinity data', return=TRUE)

    ## 1. First level of data processing
    ## ---- AIMS waterSalinity process level 1
    MMP_tryCatch(
    {
        MAXDATE=as.Date(paste0(reportYear,'-09-30'))
        MINDATE=MAXDATE-years(1)+days(1)
        
        waterSalinity <- waterSalinity %>% spread(key=PARAMETER,value=AVG_VALUE_QAQC) %>%
            mutate(new = coalesce(!!! dplyr::select(., contains('salinity')))) %>%  ##unfortunately, there are numerous salinity fields.  coalesce will combine them all into one
            dplyr::select(-contains('salinity')) %>% 
            dplyr::rename(`sal00: Salinity, Practical [PSU]`=new) %>%  ## the code from now on assumes the salinity field is called 'sal00: Salinity, Practical [PSU]'
            mutate(
                Date = as.Date(SAMPLE_DAY, format='%Y-%m-%d'),
                waterYear = MMP_waterYear(Date),
                reneeYear = MMP_reneeYear(Date),
                cwaterYear = MMP_categoricalWaterYear(Date),
                financialYear = MMP_financialYear(Date),
                cfinancialYear = MMP_categoricalFinancialYear(Date),
                Dt.num = MMP_decimalDate(Date),
                SHORT_NAME=gsub('([A-Z]*[0-9]*).*','\\1',STATION_NAME)
            ) %>% #mutate(reef.alias=MMP_reefAlias(.)) %>%
            left_join(wq.sites %>% dplyr::select(SHORT_NAME)) %>%
            left_join(names_lookup %>% dplyr::select(SHORT_NAME, MMP_SITE_NAME) %>% distinct) %>%
            droplevels() %>% 
                                        #dplyr:::select(-STATION_NAME,-START_TIME,-SAMPLE_DAY)
            dplyr:::select(-STATION_NAME,-SAMPLE_DAY) %>%
            suppressMessages() %>%
            suppressWarnings()
        
        save(waterSalinity, file=paste0(LOGGER_OUTPUT_PATH, 'waterSalinity.RData'))    
        ## print(ggplot(waterSalinity, aes(y=MMP_SITE_NAME, x=Date))+
        ##       geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=maxDate-years(1)+days(1), xmax=maxDate), fill='grey') +
        ##       geom_point()+ggtitle('Water salinity logger')+
        ##       scale_y_discrete('',limits = rev(levels(waterSalinity$MMP_SITE_NAME))) +
        ##       scale_x_date('')+
        ##       theme_mmp)
        ## textplot(capture.output(head(waterSalinity)))
        ## title("Water salinity logger data (daily averages)")
    }, LOG_FILE, Category = 'Data processing', msg='Process Water Quality (water salinity) data (stage 1)', return=TRUE)

    MMP_checkData(name = "waterSalinity.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "AIMS water salinity",
                  PATH = LOGGER_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end

    ## 2. Further processing
    ## ---- AIMS waterSalinity process level 2
    MMP_tryCatch(
    {
        load(file=paste0(LOGGER_OUTPUT_PATH, 'waterSalinity.RData'))
        wq.sites <- read.csv(paste0(PARAMS_PATH, '/wq.sites.csv'), strip.white=TRUE)
        #waterSalinity$reef.alias <- MMP_replaceNames(factor(strtrim(waterSalinity$reef.alias, width=str_length(waterSalinity$reef.alias)-9)))
        waterSalinityAll <- waterSalinity %>%
            mutate(WaterSalinity=T) %>%
            MMP_region_subregion(Source='WaterSalinity') %>%
            mutate(Month=format(Date,"%b"), Season=factor(ifelse(Month %in% c("Nov","Dec","Jan","Feb","Mar","Apr"), "Wet","Dry"))) %>%
            left_join(wq.sites %>% dplyr:::select(SHORT_NAME, LATITUDE=Latitude)) %>%
            droplevels() %>%
            suppressMessages() %>%
            suppressWarnings()
        ## p=ggplot(waterSalinityAll %>% dplyr:::select(MMP_SITE_NAME,LATITUDE,Date,Subregion,Season) %>% distinct %>% arrange(desc(LATITUDE)) %>% mutate(Subregion=factor(Subregion, levels=unique(Subregion)),MMP_SITE_NAME=factor(MMP_SITE_NAME,levels=rev(unique(MMP_SITE_NAME)))), aes(y=MMP_SITE_NAME, x=Date))+
        ##     geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=MINDATE,xmax=MAXDATE), fill='grey') +
        ##     geom_point(aes(color=Season),position=position_jitter(height=0.5),show.legend=FALSE)+ggtitle('Water salinity logger')+
        ##     scale_y_discrete('',limits = rev(levels(waterSalinityAll$MMP_SITE_NAME))) +
        ##     scale_x_date('',date_breaks='2 years', date_labels='%Y')+
        ##     scale_color_manual('',values=c('red','blue')) +
        ##     facet_grid(Subregion~., scales='free',space='free') +
        ##     theme_mmp + theme(strip.background=element_rect(fill=NA,color='black',size=0.5),
        ##                       strip.text.x=element_blank(),
        ##                       panel.border=element_rect(fill=NA,color='black',size=0.5))
        ## pdf(file=paste0('../output/figures/waterQuality/waterSalinity/waterSalinityAll.pdf'), width=10, height=10)
        ## print(p)
        ## dev.off()
        ## png(file=paste0('../output/figures/waterQuality/waterSalinity/waterSalinityAll.png'), width=10, height=10,units='in',res=300)
        ## print(p)
        ## dev.off()
        ## save(waterSalinity, file=paste0('../data/waterQuality/processed/stage1/waterSalinity.RData'))
        ## #waterSalinityAll <- MMP_region_subregion(waterSalinityAll)
        ## #waterSalinityAll$Region <- MMP_regions(waterSalinityAll$MMP_SITE_NAME)
        ## #waterSalinityAll$Subregion <- MMP_subregions(waterSalinityAll$MMP_SITE_NAME)
        save(waterSalinityAll, file=paste0(LOGGER_OUTPUT_PATH,'waterSalinityAll.RData'))
        rm(waterSalinity, waterSalinityAll)
        gc()
    }, LOG_FILE, Category = 'Data processing', msg='Process Water Quality (water salinity) data (stage 2)', return=TRUE)

    MMP_checkData(name = "waterSalinityAll.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "AIMS water salinity",
                  PATH = LOGGER_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end 
} else{
}

MMP_checkData(name = "waterSalinityAll.RData",
              stage = paste0("STAGE", CURRENT_STAGE),
              item = CURRENT_ITEM,
              label = "Processed water salinity",
              PATH = LOGGER_OUTPUT_PATH)
MMP_openning_banner()
  
## ----end
