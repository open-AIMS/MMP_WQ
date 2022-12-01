source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

NISKIN_INPUT_PATH <- paste0(DATA_PATH, "/primary/niskin/")
NISKIN_OUTPUT_PATH <- paste0(DATA_PATH, "/processed/niskin/")
MAXDATE=as.Date(paste0(reportYear,'-09-30'))
MINDATE=MAXDATE-years(1)+days(1)


## ---- AIMS niskin process
CURRENT_ITEM <- "aimsNiskin"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(NISKIN_OUTPUT_PATH,"niskin.aims.reef.RData"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'niskin.csv'))) {

    ## 1. Read in data
    ## ---- AIMS niskin read data
    MMP_tryCatch(
    {
        niskin.reef <- read_csv(paste0(NISKIN_INPUT_PATH, 'niskin.csv')) %>%
            suppressMessages()
        save(niskin.reef, file=paste0(NISKIN_OUTPUT_PATH, 'niskin.reef.RData'))
        unlink(paste0(DATA_PATH, "/reports/STAGE",CURRENT_STAGE, "_", CURRENT_ITEM, "_.RData")) 
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SECTION = paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                                    item = CURRENT_ITEM),"\n\n"),
                               TABSET = paste0("::: panel-tabset \n\n"),
                               TABSET_END = paste0("::: \n\n"),
                               SUBSECTION_SQL = structure(paste0("## SQL syntax\n"),
                                                          parent = 'TABSET'),
                               SQL = structure(mmp__sql(paste0(NISKIN_INPUT_PATH, 'niskin.sql')),
                                               parent = 'SUBSECTION_SQL')
                               )

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_GLIMPSE = structure(paste0("## Data glimpse\n"),
                                                              parent = 'TABSET'),
                               TAB = structure(mmp__add_table(mmp__glimpse_like(niskin.reef)),
                                               parent = 'SUBSECTION_GLIMPSE'),
                               TAB.CAP = structure(paste0("\n:Extraction of the first five records in each field from the niskin data. {#tbl-sql-niskin}\n\n"),
                                                   parent = 'SUBSECTION_GLIMPSE')
                              )

        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM)
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% str()
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% unlist() %>% paste(collapse = '')
    },
    LOG_FILE, item = CURRENT_NISKIN, Category = 'Data processing:', msg='Reading in Water Quality (Niskin) data', return=TRUE)
    ## ----end
    
    ## 1. First level of data processing
    ## ---- AIMS niskin process level 1
    MMP_tryCatch(
    {
        MAXDATE=as.Date(paste0(reportYear,'-08-31'))  #this used to be -06-30
        MINDATE=MAXDATE - lubridate::years(1) + lubridate::days(1)
        niskin.aims.reef = niskin.reef %>%
            mutate(
                COLLECTION_START_DATE = gsub('Sept','Sep', COLLECTION_START_DATE),
                Date = as.Date(COLLECTION_START_DATE, format='%d-%b-%Y'),           # convert to date
                oldSamplingYear=MMP_oldSamplingYear(Date),
                waterYear = MMP_waterYear(Date),
                reneeYear = MMP_reneeYear(Date),
                cwaterYear = MMP_categoricalWaterYear(Date),
                financialYear = MMP_financialYear(Date),                                    # add water year
                cfinancialYear = MMP_categoricalFinancialYear(Date),                        # categorical year
                Dt.num = MMP_decimalDate(Date)                                      # decimal year
            ) %>%
            MMP_correctLocations()  %>%   #correct the LOCATION_NAME of a couple of Reefs at a couple of times 
            ##In order to relate water quality measures to environmental covariates (particularly tidal flow)
            ## it is necessary that the time of sample collection be
            ## preserved in addition to the date.
            mutate(Time=as.POSIXct(COLLECTION_START_DATE, format='%d-%b-%Y %H:%M:%S')) %>%
            mutate(reef.alias=ifelse(MMP_SITE_NAME!='',
                                     as.character(MMP_SITE_NAME),
                                     as.character(LOCATION_NAME))) %>%   #get reef.alias from MMP_SITE_NAME
            filter(Date<MAXDATE) %>%
            MMP_reorderReefs() %>%            #reorder the reef.alias levels according to Latitude and then Longitude 
            MMP_selectReefs(source='niskin')  #select reefs according to the (lookup.csv) 'niskin' design (other options are 'JCU', 'flntu', 'coral', 'WaterTemp')
        save(niskin.aims.reef, file=paste0(NISKIN_OUTPUT_PATH, 'niskin.aims.reef.RData'))
    }, LOG_FILE, Category = 'Data processing:', msg='Initial parsing of Water Quality (Niskin) data', return=TRUE)

    MMP_checkData(name = "niskin.aims.reef.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.prefix = "Processed",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end
    ## 2. Correct the PN data for instrument transition
    ## ---- AIMS niskin process level 2
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_PATH, '/pn.lm.RData'))
        wch <- is.na(niskin.aims.reef$PN_UM) & !is.na(niskin.aims.reef$PN_SHIM_UM)
        xs <- niskin.aims.reef$PN_SHIM_UM[wch]
        niskin.aims.reef$PN_UM[wch] <- predict(pn.lm, newdata=data.frame(PN_SHIM_UM.wm=xs))
        save(niskin.aims.reef, file=paste0(NISKIN_OUTPUT_PATH, 'niskin.aims.reef.RData'))
    }, LOG_FILE, Category = 'Data processing:', msg='Correct the PN data for instrument transition for Water Quality (Niskin) data', return=TRUE)

    MMP_checkData(name = "niskin.aims.reef.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.prefix = "Processed",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end
    ## 3. Further processing
    ## ---- AIMS niskin process level 3
    MMP_tryCatch(
    {
        ## msg <- file("log/junk.msg", open = "wt"); sink(msg); sink(msg, type = "message")
        load(paste0(NISKIN_OUTPUT_PATH, 'niskin.aims.reef.RData'))
        niskin.aims.reef <- niskin.aims.reef %>%
            MMP_limitDetection() %>%                                                           #correct for limit detection for HAND_NH4
            MMP_convertUnits() %>%                                                         #convert units from those used to store data (micro mol) to those  for reporting (micro gram)
             MMP_derivedChem()                                                          #derive a bunch of chemical combinations and ratios
        niskin.aims.reef.av <-
            niskin.aims.reef %>%
            filter(STATION_CLASS != 'V') %>%                        #2020: remove this station class
            MMP_aggregateWQDuplicates() %>%                   #aggregate the duplicates
            MMP_consecutiveDays() %>%  #Note, this has changed (2018)... It is now based on LOCATION and Date and it also removes CR samples that are not aligned with R samples.
             ungroup %>% #select(-Col) %>%                                                           #I THINK THERE MIGHT HAVE BEEN A BUG IN THE PREVIOUS COE FOR WHEN DIFFERENT STATIONS ARE COLLECTED ON THE SAME DAY.generate a field that specifies a Collection of observations (perhaps collected over multiple days)
             MMP_depthWeightedAverages() %>%                                                #generate a few different depth weighted averages
             MMP_designLatest(WQ=TRUE) %>%
             MMP_GBRMPA_specs(WQ=TRUE) %>%
             MMP_region_subregion(Source='Niskin') %>%
             MMP_reorderReefs() %>% arrange(MMP_SITE_NAME) %>%
             mutate(Label = MMP_locationLabels(reef.alias),         #create names for consistency with forams data
                    Dtt.num = as.integer(Time),
                    Mnth = as.integer(format(Date, format='%m')),
                    Subregion=factor(Subregion, levels=unique(Subregion))) %>% droplevels %>%
             mutate(HistoricReef=MMP_HistoricReef(MMP_SITE_NAME))
        save(niskin.aims.reef.av, file=paste0(NISKIN_OUTPUT_PATH, 'niskin.aims.reef.av.RData'))
        ## rm(niskin.aims.reef.agg, niskin.aims.reef, niskin.aims.reef.av)
        ## gc()
    }, LOG_FILE, Category = "Data processing:", msg='Process Water Quality (Niskin) data', return=TRUE)

    MMP_checkData(name = "niskin.aims.reef.av.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "",
                  label.suffix = "processed averages",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()

    ## ----end

    ## ---- AIMS niskin outputs
    MMP_tryCatch(
    {
        load(file=paste0(NISKIN_OUTPUT_PATH, 'niskin.aims.reef.av.RData'))
        p <- ggplot(niskin.aims.reef.av %>% 
                    dplyr:::select(SHORT_NAME, MMP_SITE_NAME,LATITUDE,Date,Subregion,Season) %>%
                    distinct %>%
                    arrange(desc(LATITUDE)) %>%
                    mutate(MMP_SITE_NAME=factor(MMP_SITE_NAME,levels=rev(unique(MMP_SITE_NAME))),
                           nms = paste0(MMP_SITE_NAME, ' (', SHORT_NAME,')'), 
                           nms = forcats::fct_reorder(nms, LATITUDE)),
                    aes(y=(nms), x=Date))+
            geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=MINDATE,xmax=MAXDATE), fill='grey', color=NA) +
            geom_point(aes(color=Season),show.legend=FALSE)+ggtitle('Water quality niskin data (AIMS)')+
                                        #geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=minDate,xmax=maxDate), color='black', fill=NA) +    
            scale_y_discrete('') +
            scale_x_date('',date_breaks='2 years', date_labels='%Y')+
            scale_color_manual('',values=c('red','blue')) +
            facet_grid(Subregion~., scales='free') +
            ggplot2:::theme_grey() +
            theme(strip.background=element_rect(fill=NA,color='black',size=0.5),
                  strip.text.x=element_blank(),
                  panel.border=element_rect(fill=NA,color='black',size=0.5))
        
        ggsave(file=paste0(OUTPUT_PATH, '/figures/processed/niskin_aims_reef_av.png'),
               p,
               width=12, height=10, dpi = 100)

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_DESIGN = structure(paste0("## Sampling design\n"),
                                                             parent = 'TABSET'),
                               FIG_REF = structure(paste0("\n::: {#fig-sql-niskin}\n"),
                                                   parent = 'SUBSECTION_DESIGN'),
                               FIG = structure(paste0("![](",OUTPUT_PATH,"/figures/processed/niskin_aims_reef_av.png)\n"),
                                               parent = "FIG_REF"),
                               FIG_CAP = structure(paste0("\nTemporal distribution of AIMS Niskin water quality samples. Red and blue symbols signify Dry and Wet season samples respectively. Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                                                   parent = 'FIG_REF'),
                               FIG_REF_END = structure(paste0("\n::: \n"),
                                                       parent = 'SUBSECTION_DESIGN')
                              )
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM)
        ## ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% str()
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% unlist() %>% paste(collapse = '')

        
    }, LOG_FILE, Category = "Data processing:", msg='Preparing report outputs for Water Quality (Niskin) data', return=TRUE)

    ## ----end

} else {
}


MMP_checkData(name = "niskin.aims.reef.av.RData",
              stage = paste0("STAGE", CURRENT_STAGE),
              item = CURRENT_ITEM,
              label.prefix = "Processed AIMS",
              PATH = NISKIN_OUTPUT_PATH)
MMP_openning_banner()
## ----end

## ---- AIMS Cairns Transect process
CURRENT_ITEM <- "cairnsTransect"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(NISKIN_OUTPUT_PATH, "cairns.reef.av.RData"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'cairns.csv'))) {

    ## ---- AIMS Cairns niskin read data
    MMP_tryCatch(
    {
        cairns.reef <- read_csv(paste0(NISKIN_INPUT_PATH, 'cairns.csv')) %>%
            suppressMessages()
        save(cairns.reef, file=paste0(NISKIN_OUTPUT_PATH, 'cairns.reef.RData'))
        unlink(paste0(DATA_PATH, "/reports/STAGE",CURRENT_STAGE, "_", CURRENT_ITEM, "_.RData")) 
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SECTION = paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                                    item = CURRENT_ITEM),"\n\n"),
                               TABSET = paste0("::: panel-tabset \n\n"),
                               TABSET_END = paste0("::: \n\n"),
                               SUBSECTION_SQL = structure(paste0("## SQL syntax\n"),
                                                          parent = 'TABSET'),
                               SQL = structure(mmp__sql(paste0(NISKIN_INPUT_PATH, 'cairns.sql')),
                                               parent = 'SUBSECTION_SQL')
                               )

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_GLIMPSE = structure(paste0("## Data glimpse\n"),
                                                              parent = 'TABSET'),
                               TAB = structure(mmp__add_table(mmp__glimpse_like(cairns.reef)),
                                               parent = 'SUBSECTION_GLIMPSE'),
                               TAB.CAP = structure(paste0("\n:Extraction of the first five records in each field from the Cairns Transect data. {#tbl-sql-cairns}\n\n"),
                                                   parent = 'SUBSECTION_GLIMPSE')
                              )
    },
    LOG_FILE, item = CURRENT_NISKIN, Category = 'Data processing:', msg='Reading in Water Quality (Cairns) data', return=TRUE)
    
    ## ----end

    ## 1. First level of data processing
    ## ---- AIMS Cairns niskin process level 1
    MMP_tryCatch(
    {
        MAXDATE=as.Date(paste0(reportYear,'-08-31'))  #this used to be -06-30
        MINDATE=MAXDATE - lubridate::years(1) + lubridate::days(1)
        cairns.reef = cairns.reef %>%
            mutate(
                COLLECTION_START_DATE = gsub('Sept','Sep', COLLECTION_START_DATE),
                Date = as.Date(COLLECTION_START_DATE, format='%d-%b-%Y'),           # convert to date
                oldSamplingYear=MMP_oldSamplingYear(Date),
                waterYear = MMP_waterYear(Date),
                reneeYear = MMP_reneeYear(Date),
                cwaterYear = MMP_categoricalWaterYear(Date),
                financialYear = MMP_financialYear(Date),                                    # add water year
                cfinancialYear = MMP_categoricalFinancialYear(Date),                        # categorical year
                Dt.num = MMP_decimalDate(Date),                                      # decimal year
            ) %>%
            MMP_correctLocations()  %>%   #correct the LOCATION_NAME of a couple of Reefs at a couple of times 
            ##In order to relate water quality measures to environmental covariates (particularly tidal flow)
            ## it is necessary that the time of sample collection be
            ## preserved in addition to the date.
            mutate(Time=as.POSIXct(COLLECTION_START_DATE, format='%d-%b-%Y %H:%M:%S')) %>%
            mutate(reef.alias=ifelse(MMP_SITE_NAME!='',
                                     as.character(MMP_SITE_NAME),
                                     as.character(LOCATION_NAME))) %>%   #get reef.alias from MMP_SITE_NAME
            filter(Date<MAXDATE) %>%
            MMP_reorderReefs()             #reorder the reef.alias levels according to Latitude and then Longitude 
        save(cairns.reef, file=paste0(NISKIN_OUTPUT_PATH, 'cairns.reef.RData'))
    }, LOG_FILE, Category = 'Data processing:', msg='Initial parsing of Water Quality (Cairns) data', return=TRUE)

    MMP_checkData(name = "cairns.reef.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end
    ## 2. Correct the PN data for instrument transition
    ## ---- AIMS Cairns niskin process level 2
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_PATH, '/pn.lm.RData'))
        wch <- is.na(cairns.reef$PN_UM) & !is.na(cairns.reef$PN_SHIM_UM)
        xs <- cairns.reef$PN_SHIM_UM[wch]
        cairns.reef$PN_UM[wch] <- predict(pn.lm, newdata=data.frame(PN_SHIM_UM.wm=xs))
        save(cairns.reef, file=paste0(NISKIN_OUTPUT_PATH, 'cairns.reef.RData'))
    }, LOG_FILE, Category = 'Data processing:', msg='Correct the PN data for instrument transition for Water Quality (Cairns) data', return=TRUE)

    MMP_checkData(name = "cairns.reef.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end
    ## 3. Further processing
    ## ---- AIMS Cairns niskin process level 3
    MMP_tryCatch(
    {
        load(paste0(NISKIN_OUTPUT_PATH, 'cairns.reef.RData'))
        cairns.all.reef <- cairns.reef %>%
            MMP_limitDetection() %>%                                                           #correct for limit detection for HAND_NH4
            MMP_convertUnits() %>%                                                         #convert units from those used to store data (micro mol) to those  for reporting (micro gram)
             MMP_derivedChem(type = "Cairns")                                                          #derive a bunch of chemical combinations and ratios
        cairns.reef.av <-
            cairns.all.reef %>%
            filter(STATION_CLASS != 'V') %>%                        #2020: remove this station class
            MMP_aggregateWQDuplicates() %>%                   #aggregate the duplicates
            MMP_consecutiveDays() %>%  #Note, this has changed (2018)... It is now based on LOCATION and Date and it also removes CR samples that are not aligned with R samples.
             ungroup %>% #select(-Col) %>%                                                           #I THINK THERE MIGHT HAVE BEEN A BUG IN THE PREVIOUS COE FOR WHEN DIFFERENT STATIONS ARE COLLECTED ON THE SAME DAY.generate a field that specifies a Collection of observations (perhaps collected over multiple days)
             MMP_depthWeightedAverages() %>%                                                #generate a few different depth weighted averages
             MMP_designLatest(WQ=TRUE) %>%
             MMP_GBRMPA_specs(WQ=TRUE) %>%
             MMP_region_subregion(Source='Niskin') %>%
             MMP_reorderReefs() %>% arrange(desc(LATITUDE)) %>% droplevels() %>%
             mutate(Label = MMP_locationLabels(reef.alias),         #create names for consistency with forams data
                    Dtt.num = as.integer(Time),
                    Mnth = as.integer(format(Date, format='%m')),
                    Subregion="Cairns Transect") 
        save(cairns.reef.av, file=paste0(NISKIN_OUTPUT_PATH, 'cairns.reef.av.RData'))
        ## rm(niskin.aims.reef.agg, niskin.aims.reef, niskin.aims.reef.av)
        ## gc()
    }, LOG_FILE, Category = "Data processing:", msg='Process Water Quality (Cairns) data', return=TRUE)

    MMP_checkData(name = "cairns.reef.av.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "",
                  label.suffix = "processed averages",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()

    ## ----end

    ## ---- AIMS cairns niskin outputs
    MMP_tryCatch(
    {
        load(file=paste0(NISKIN_OUTPUT_PATH, 'cairns.reef.av.RData'))
        p <- ggplot(cairns.reef.av %>% 
                    dplyr:::select(SHORT_NAME, MMP_SITE_NAME,LATITUDE,Date,Subregion,Season) %>%
                    distinct %>%
                    arrange(desc(LATITUDE)) %>%
                    mutate(MMP_SITE_NAME=factor(MMP_SITE_NAME,levels=rev(unique(MMP_SITE_NAME))),
                           nms = paste0(MMP_SITE_NAME, ' (', SHORT_NAME,')'), 
                           nms = forcats::fct_reorder(nms, LATITUDE)),
                    aes(y=(nms), x=Date))+
            geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=MINDATE,xmax=MAXDATE), fill='grey', color=NA) +
            geom_point(aes(color=Season),show.legend=FALSE)+ggtitle('Water quality cairns data (AIMS)')+
                                        #geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=minDate,xmax=maxDate), color='black', fill=NA) +    
            scale_y_discrete('') +
            scale_x_date('',date_breaks='2 years', date_labels='%Y')+
            scale_color_manual('',values=c('red','blue')) +
            facet_grid(Subregion~., scales='free') +
            ggplot2:::theme_grey() +
            theme(strip.background=element_rect(fill=NA,color='black',size=0.5),
                  strip.text.x=element_blank(),
                  panel.border=element_rect(fill=NA,color='black',size=0.5))
        
        ggsave(file=paste0(OUTPUT_PATH, '/figures/processed/cairns_reef_av.png'),
               p,
               width=12, height=10, dpi = 100)

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_DESIGN = structure(paste0("## Sampling design\n"),
                                                             parent = 'TABSET'),
                               FIG_REF = structure(paste0("\n::: {#fig-sql-cairns}\n"),
                                                   parent = 'SUBSECTION_DESIGN'),
                               FIG = structure(paste0("![](",OUTPUT_PATH,"/figures/processed/cairns_reef_av.png)\n"),
                                               parent = "FIG_REF"),
                               FIG_CAP = structure(paste0("\nTemporal distribution of AIMS Cairns Transect water quality samples. Red and blue symbols signify Dry and Wet season samples respectively. Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                                                   parent = 'FIG_REF'),
                               FIG_REF_END = structure(paste0("\n::: \n"),
                                                       parent = 'SUBSECTION_DESIGN')
                              )
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM)
        ## ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% str()
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% unlist() %>% paste(collapse = '')

        
    }, LOG_FILE, Category = "Data processing:", msg='Preparing report outputs for Water Quality (Niskin) data', return=TRUE)

    ## ----end
} else {
}

MMP_checkData(name = "cairns.reef.av.RData",
              stage = paste0("STAGE", CURRENT_STAGE),
              item = CURRENT_ITEM,
              label = "",
              label.suffix = "processed averages",
              PATH = NISKIN_OUTPUT_PATH)
MMP_openning_banner()

## ----end

## ---- JCU niskin process
CURRENT_ITEM <- "jcuNiskin"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(NISKIN_OUTPUT_PATH, "niskin.jcu.reef.av1.RData"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'jcu.csv'))) {

    ## 1. Read in data
    ## ---- JCU niskin read data
    MMP_tryCatch(
    {
        jcu.reef <- read_csv(paste0(NISKIN_INPUT_PATH, 'jcu.csv')) %>%
                     suppressMessages()
        jcu_lookup = read_csv(paste0(PARAMS_PATH, '/jcu_location_lookup.csv'), trim_ws = TRUE) %>%
            suppressMessages()

        unlink(paste0(DATA_PATH, "/reports/STAGE",CURRENT_STAGE, "_", CURRENT_ITEM, "_.RData")) 
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SECTION = paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                                    item = CURRENT_ITEM),"\n\n"),
                               TABSET = paste0("::: panel-tabset \n\n"),
                               TABSET_END = paste0("::: \n\n"),
                               SUBSECTION_SQL = structure(paste0("## SQL syntax\n"),
                                                          parent = 'TABSET'),
                               SQL = structure(mmp__sql(paste0(NISKIN_INPUT_PATH, 'jcu.sql')),
                                               parent = 'SUBSECTION_SQL')
                               )

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_GLIMPSE = structure(paste0("## Data glimpse\n"),
                                                              parent = 'TABSET'),
                               TAB = structure(mmp__add_table(mmp__glimpse_like(jcu.reef)),
                                               parent = 'SUBSECTION_GLIMPSE'),
                               TAB.CAP = structure(paste0("\n:Extraction of the first five records in each field from the JCU data. {#tbl-sql-jcu}\n\n"),
                                                   parent = 'SUBSECTION_GLIMPSE')
                              )
    },
    LOG_FILE, item = CURRENT_NISKIN, Category = 'Data processing:', msg='Reading in Water Quality (JCU) data', return=TRUE)
    ## ----end
    
    ## 1. First level of data processing
    ## ---- JCU niskin process level 1
    MMP_tryCatch(
    {
        MAXDATE=as.Date(paste0(reportYear,'-08-31'))  #this used to be -06-30
        MINDATE=MAXDATE - lubridate::years(1) + lubridate::days(1)
        niskin.jcu.reef1 = jcu.reef %>%
            mutate(
                COLLECTION_START_DATE = gsub('Sept','Sep', COLLECTION_START_DATE),
                Date = as.Date(COLLECTION_START_DATE, format='%d-%b-%Y'),           # convert to date
                oldSamplingYear=MMP_oldSamplingYear(Date),
                waterYear = MMP_waterYear(Date),
                reneeYear = MMP_reneeYear(Date),
                cwaterYear = MMP_categoricalWaterYear(Date),
                financialYear = MMP_financialYear(Date),                                    # add water year
                cfinancialYear = MMP_categoricalFinancialYear(Date),                        # categorical year
                Dt.num = MMP_decimalDate(Date)                                      # decimal year
            ) %>%
            mutate(SHORT_NAME=as.character(SHORT_NAME)) %>%
            left_join(jcu_lookup %>%
                      dplyr:::select(SHORT_NAME) %>%
                      mutate(SHORT_NAME=as.character(SHORT_NAME)) %>%
                      distinct) %>%
            ##In order to relate water quality measures to environmental covariates (particularly tidal flow)
            ## it is necessary that the time of sample collection be
            ## preserved in addition to the date.
            mutate(Time=as.POSIXct(COLLECTION_START_DATE, format='%d-%b-%Y %H:%M:%S')) %>%
            mutate(
                Collection=interaction(MMP_SITE_NAME, Date)) %>%
            filter(Date<MAXDATE) %>%
            MMP_reorderReefs() %>%
            MMP_selectReefs(source='JCU') %>%
            suppressMessages() %>%
            suppressWarnings()
        save(jcu.reef, file=paste0(NISKIN_OUTPUT_PATH, 'jcu.reef.RData'))
        save(niskin.jcu.reef1, file=paste0(NISKIN_OUTPUT_PATH, 'niskin.jcu.reef1.RData'))
    }, LOG_FILE, Category = 'Data processing:', msg='Initial parsing of Water Quality (Niskin) data', return=TRUE)

    MMP_checkData(name = "niskin.jcu.reef1.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end
    ## 3. Further processing
    ## ---- JCU niskin process level 3
    MMP_tryCatch(
    {
        load(paste0(NISKIN_OUTPUT_PATH, 'niskin.jcu.reef1.RData'))
        niskin.jcu.reef1 <- niskin.jcu.reef1 %>%
            mutate(PN_UM=PN_SHIM_UM) %>%                                      #2019 now all the good JCU PN data is entered as PN_SHIM_UM (actually PN_SHIM_QAQC)
            MMP_limitDetection() %>%                                                           #correct for limit detection for HAND_NH4
            MMP_convertUnits() %>%                                                         #convert units from those used to store data (micro mol) to those  for reporting (micro gram)
             MMP_derivedChem()                                                          #derive a bunch of chemical combinations and ratios
        niskin.jcu.reef.av1 <-
            niskin.jcu.reef1 %>%
            MMP_aggregateWQDuplicates() %>%                   #aggregate the duplicates
            MMP_depthWeightedAverages() %>%                                                #generate a few different depth weighted averages
             MMP_designLatest(WQ=TRUE) %>%
             MMP_GBRMPA_specs(WQ=TRUE) %>%
             MMP_region_subregion(Source='JCU') %>%
             MMP_reorderReefs() %>% arrange(MMP_SITE_NAME) %>% droplevels() %>%
             mutate(Label = MMP_locationLabels(MMP_SITE_NAME),         #create names for consistency with forams data
                    Dtt.num = as.integer(Time),
                    Mnth = as.integer(format(Date, format='%m')),
                    Subregion=factor(Subregion, levels=unique(Subregion))) %>% droplevels %>%
             mutate(HistoricReef=MMP_HistoricReef(MMP_SITE_NAME))
        save(niskin.jcu.reef.av1, file=paste0(NISKIN_OUTPUT_PATH, 'niskin.jcu.reef.av1.RData'))
    }, LOG_FILE, Category = "Data processing:", msg='Process Water Quality (JCU) data', return=TRUE)

    MMP_checkData(name = "niskin.jcu.reef.av1.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "",
                  label.suffix = "processed averages",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()

    ## ----end

    ## ---- JCU niskin outputs
    MMP_tryCatch(
    {
        load(file=paste0(NISKIN_OUTPUT_PATH, 'niskin.jcu.reef.av1.RData'))
        p <- ggplot(niskin.jcu.reef.av1 %>% 
                    dplyr:::select(SHORT_NAME, MMP_SITE_NAME,LATITUDE,Date,Subregion,Season) %>%
                    distinct %>%
                    arrange(desc(LATITUDE)) %>%
                    mutate(MMP_SITE_NAME=factor(MMP_SITE_NAME,levels=rev(unique(MMP_SITE_NAME))),
                           nms = paste0(MMP_SITE_NAME, ' (', SHORT_NAME,')'), 
                           nms = forcats::fct_reorder(nms, LATITUDE)),
                    aes(y=(nms), x=Date))+
            geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=MINDATE,xmax=MAXDATE), fill='grey', color=NA) +
            geom_point(aes(color=Season),show.legend=FALSE)+ggtitle('Water quality niskin data (JCU)')+
                                        #geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=minDate,xmax=maxDate), color='black', fill=NA) +    
            scale_y_discrete('') +
            scale_x_date('',date_breaks='2 years', date_labels='%Y')+
            scale_color_manual('',values=c('red','blue')) +
            facet_grid(Subregion~., scales='free') +
            ggplot2:::theme_grey() +
            theme(strip.background=element_rect(fill=NA,color='black',size=0.5),
                  strip.text.x=element_blank(),
                  panel.border=element_rect(fill=NA,color='black',size=0.5))
        
        ggsave(file=paste0(OUTPUT_PATH, '/figures/processed/niskin_jcu_reef_av1.png'),
               p,
               width=12, height=10, dpi = 100)

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_DESIGN = structure(paste0("## Sampling design\n"),
                                                             parent = 'TABSET'),
                               FIG_REF = structure(paste0("\n::: {#fig-sql-jcu}\n"),
                                                   parent = 'SUBSECTION_DESIGN'),
                               FIG = structure(paste0("![](",OUTPUT_PATH,"/figures/processed/niskin_jcu_reef_av1.png)\n"),
                                               parent = "FIG_REF"),
                               FIG_CAP = structure(paste0("\nTemporal distribution of AIMS JCU water quality samples. Red and blue symbols signify Dry and Wet season samples respectively. Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                                                   parent = 'FIG_REF'),
                               FIG_REF_END = structure(paste0("\n::: \n"),
                                                       parent = 'SUBSECTION_DESIGN')
                              )
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM)
        ## ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% str()
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% unlist() %>% paste(collapse = '')

        
    }, LOG_FILE, Category = "Data processing:", msg='Preparing report outputs for Water Quality (Niskin) data', return=TRUE)

    ## ----end

} else {
}
## ---- outputs
MMP_tryCatch(
{

    MMP_add_to_report(report_list = DOC_REPORT_LIST,
                      content = list(
                          paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                     item = CURRENT_ITEM),"\n\n"),
                          paste0("::: panel-tabset \n\n"),
                          paste0("## SQL syntax\n"),
                          mmp__sql(paste0(NISKIN_INPUT_PATH, 'jcu.sql')),
                          paste0("## Data glimpse\n"),
                          mmp__add_table(mmp__glimpse_like(jcu.reef)),
                          paste0("\n:Extraction of the first five records in each field from the JCU data. {#tbl-sql-cairns}\n\n"),
                          paste0("## Sampling design\n"),
                          paste0("\n::: {#fig-sql-jcu}\n"),
                          paste0("![](",OUTPUT_PATH,"/figures/processed/niskin_jcu_reef_av1.png)\n"),
                          paste0("\nTemporal distribution of JCU water quality samples. Red and blue symbols signify Dry and Wet season samples respectively. Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                          paste0("::: \n"),
                          paste0("::: \n\n")
                      )
                      )
    
    save(DOC_REPORT_LIST, file = paste0(DATA_PATH, "/processed/DOC_REPORT_LIST.RData"))
}, LOG_FILE, Category = "Data processing:", msg='Preparing report outputs for Water Quality (JCU) data', return=TRUE)

## ----end

MMP_checkData(name = "niskin.jcu.reef.av1.RData",
              stage = paste0("STAGE", CURRENT_STAGE),
              item = CURRENT_ITEM,
              label = "",
              label.suffix = "processed averages",
              PATH = NISKIN_OUTPUT_PATH)
MMP_openning_banner()

## ----end

## ---- JCU CY niskin process
CURRENT_ITEM <- "jcuCYNiskin"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(NISKIN_OUTPUT_PATH, "niskin.cy.reef.av.RData"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'cy.csv'))) {

    ## 1. Read in data
    ## ---- JCU CY niskin read data
    MMP_tryCatch(
    {
        cy.reef <- read_csv(paste0(NISKIN_INPUT_PATH, 'cy.csv')) %>%
            suppressMessages()
        lookup = read_csv(paste0(PARAMS_PATH, '/lookup.csv'), trim_ws = TRUE) %>%
            suppressMessages()
        wq.sites = read_csv(paste0(PARAMS_PATH, '/wq.sites.csv'), trim_ws = TRUE) %>%
            suppressMessages()

        save(cy.reef, file=paste0(NISKIN_OUTPUT_PATH, 'cy.reef.RData'))
        unlink(paste0(DATA_PATH, "/reports/STAGE",CURRENT_STAGE, "_", CURRENT_ITEM, "_.RData")) 
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SECTION = paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                                    item = CURRENT_ITEM),"\n\n"),
                               TABSET = paste0("::: panel-tabset \n\n"),
                               TABSET_END = paste0("::: \n\n"),
                               SUBSECTION_SQL = structure(paste0("## SQL syntax\n"),
                                                          parent = 'TABSET'),
                               SQL = structure(mmp__sql(paste0(NISKIN_INPUT_PATH, 'cy.sql')),
                                               parent = 'SUBSECTION_SQL')
                               )

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_GLIMPSE = structure(paste0("## Data glimpse\n"),
                                                              parent = 'TABSET'),
                               TAB = structure(mmp__add_table(mmp__glimpse_like(cy.reef)),
                                               parent = 'SUBSECTION_GLIMPSE'),
                               TAB.CAP = structure(paste0("\n:Extraction of the first five records in each field from the Cape York data. {#tbl-sql-cy}\n\n"),
                                                   parent = 'SUBSECTION_GLIMPSE')
                              )
    },
    LOG_FILE, item = CURRENT_NISKIN, Category = 'Data processing:', msg='Reading in Water Quality (CY) data', return=TRUE)
    ## ----end

    ## 1. First level of data processing
    ## ---- CY niskin process level 1
    MMP_tryCatch(
    {
        ## Check for missing samples
        cy.reef %>% dplyr::select(LOCATION_NAME, MMP_SITE_NAME,SHORT_NAME) %>% distinct %>%
            anti_join(wq.sites %>% dplyr::select(SHORT_NAME) %>% distinct) %>%
            left_join(wq.sites %>% dplyr::select(SHORT_NAME) %>% distinct) %>%
            suppressWarnings() %>%
            suppressMessages()
        MAXDATE=as.Date(paste0(reportYear,'-08-31'))  #this used to be -06-30
        MINDATE=MAXDATE - lubridate::years(1) + lubridate::days(1)
        niskin.cy.reef = cy.reef %>%
            filter(MMP_SITE_NAME!='') %>%  #2021 - exclude any records that do not have a MMP_SITE_NAME - these are identified in the anti_join above 
            mutate(
                COLLECTION_START_DATE = gsub('Sept','Sep', COLLECTION_START_DATE),
                Date = as.Date(COLLECTION_START_DATE, format='%d-%b-%Y'),           # convert to date
                oldSamplingYear=MMP_oldSamplingYear(Date),
                waterYear = MMP_waterYear(Date),
                reneeYear = MMP_reneeYear(Date),
                cwaterYear = MMP_categoricalWaterYear(Date),
                financialYear = MMP_financialYear(Date),                                    # add water year
                cfinancialYear = MMP_categoricalFinancialYear(Date),                        # categorical year
                Dt.num = MMP_decimalDate(Date)                                      # decimal year
            ) %>%
            mutate(Time=as.POSIXct(COLLECTION_START_DATE, format='%d-%b-%Y %H:%M:%S')) %>%
            mutate(SHORT_NAME=as.character(SHORT_NAME)) %>%
            mutate(
                Collection=interaction(MMP_SITE_NAME, Date)) %>%
            filter(Date<MAXDATE) %>%
            MMP_reorderReefs() %>%
            MMP_selectReefs(source='CY') 
        save(niskin.cy.reef, file=paste0(NISKIN_OUTPUT_PATH, 'niskin.cy.reef.RData'))
        save(cy.reef, file=paste0(NISKIN_OUTPUT_PATH, 'cy.reef.RData'))
    }, LOG_FILE, Category = 'Data processing:', msg='Initial parsing of Water Quality (Niskin) data', return=TRUE)

    MMP_checkData(name = "niskin.cy.reef.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end
    ## 3. Further processing
    ## ---- CY niskin process level 3
    MMP_tryCatch(
    {
        load(paste0(NISKIN_OUTPUT_PATH, 'niskin.cy.reef.RData'))
        niskin.cy.reef <- niskin.cy.reef %>%
            mutate(PN_UM=PN_SHIM_UM) %>%                                      #2019 now all the good JCU PN data is entered as PN_SHIM_UM (actually PN_SHIM_QAQC)
            MMP_limitDetection() %>%                                                           #correct for limit detection for HAND_NH4
            MMP_convertUnits() %>%                                                         #convert units from those used to store data (micro mol) to those  for reporting (micro gram)
             MMP_derivedChem()                                                          #derive a bunch of chemical combinations and ratios
        niskin.cy.reef.av <-
            niskin.cy.reef %>%
            MMP_aggregateWQDuplicates() %>%                   #aggregate the duplicates
            MMP_depthWeightedAverages() %>%                                                #generate a few different depth weighted averages
             MMP_designLatest(WQ=TRUE) %>%
             MMP_GBRMPA_specs(WQ=TRUE) %>%
             MMP_region_subregion(Source='JCU') %>%
            filter(!is.na(Subregion)) %>% droplevels %>% 
             MMP_reorderReefs() %>% arrange(MMP_SITE_NAME) %>% droplevels() %>%
             mutate(Label = MMP_locationLabels(MMP_SITE_NAME),         #create names for consistency with forams data
                    Dtt.num = as.integer(Time),
                    Mnth = as.integer(format(Date, format='%m')),
                    Subregion=factor(Subregion, levels=unique(Subregion))) %>% droplevels
        save(niskin.cy.reef.av, file=paste0(NISKIN_OUTPUT_PATH, 'niskin.cy.reef.av.RData'))
    }, LOG_FILE, Category = "Data processing:", msg='Process Water Quality (CY) data', return=TRUE)

    MMP_checkData(name = "niskin.cy.reef.av.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "",
                  label.suffix = "processed averages",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()

    ## ----end

    ## ---- CY niskin outputs
    MMP_tryCatch(
    {
        load(file=paste0(NISKIN_OUTPUT_PATH, 'niskin.cy.reef.av.RData'))
        p <- ggplot(niskin.cy.reef.av %>% 
                    dplyr:::select(SHORT_NAME, MMP_SITE_NAME,LATITUDE,Date,Subregion,Season) %>%
                    distinct %>%
                    arrange(desc(LATITUDE)) %>%
                    mutate(MMP_SITE_NAME=factor(MMP_SITE_NAME,levels=rev(unique(MMP_SITE_NAME))),
                           nms = paste0(MMP_SITE_NAME, ' (', SHORT_NAME,')'), 
                           nms = forcats::fct_reorder(nms, LATITUDE)),
                    aes(y=(nms), x=Date))+
            geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=MINDATE,xmax=MAXDATE), fill='grey', color=NA) +
            geom_point(aes(color=Season),show.legend=FALSE)+ggtitle('Water quality niskin data (JCU CY)')+
                                        #geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=minDate,xmax=maxDate), color='black', fill=NA) +    
            scale_y_discrete('') +
            scale_x_date('',date_breaks='2 years', date_labels='%Y')+
            scale_color_manual('',values=c('red','blue')) +
            facet_grid(Subregion~., scales='free') +
            ggplot2:::theme_grey() +
            theme(strip.background=element_rect(fill=NA,color='black',size=0.5),
                  strip.text.x=element_blank(),
                  panel.border=element_rect(fill=NA,color='black',size=0.5))
    
        ggsave(file=paste0(OUTPUT_PATH, '/figures/processed/niskin_cy_reef_av.png'),
               p,
               width=12, height=10, dpi = 100)

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_DESIGN = structure(paste0("## Sampling design\n"),
                                                             parent = 'TABSET'),
                               FIG_REF = structure(paste0("\n::: {#fig-sql-cy}\n"),
                                                   parent = 'SUBSECTION_DESIGN'),
                               FIG = structure(paste0("![](",OUTPUT_PATH,"/figures/processed/niskin_cy_reef_av.png)\n"),
                                               parent = "FIG_REF"),
                               FIG_CAP = structure(paste0("\nTemporal distribution of Cape York Niskin water quality samples. Red and blue symbols signify Dry and Wet season samples respectively. Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                                                   parent = 'FIG_REF'),
                               FIG_REF_END = structure(paste0("\n::: \n"),
                                                       parent = 'SUBSECTION_DESIGN')
                              )
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM)
        ## ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% str()
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% unlist() %>% paste(collapse = '')

        
    }, LOG_FILE, Category = "Data processing:", msg='Preparing report outputs for Water Quality (Niskin) data', return=TRUE)

    ## ----end
    
} else {
}

MMP_checkData(name = "niskin.cy.reef.av.RData",
              stage = paste0("STAGE", CURRENT_STAGE),
              item = CURRENT_ITEM,
              label.prefix = "Processed",
              label = "",
              label.suffix = "averages",
              PATH = NISKIN_OUTPUT_PATH)
MMP_openning_banner()

## ----end

## ---- JCU Event niskin process
CURRENT_ITEM <- "jcuEventNiskin"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(NISKIN_OUTPUT_PATH, "niskin.jcu.event.reef.av1.RData"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'jcuEvent.csv'))) {

    ## 1. Read in data
    ## ---- JCU Event niskin read data
    MMP_tryCatch({
        jcu.event.reef <- read_csv(paste0(NISKIN_INPUT_PATH, 'jcuEvent.csv')) %>%
                     suppressMessages()
        jcu_lookup = read_csv(paste0(PARAMS_PATH, '/jcu_location_lookup.csv'), trim_ws = TRUE) %>%
            suppressMessages()
        wq.sites = read_csv(paste0(PARAMS_PATH, '/wq.sites.csv'), trim_ws = TRUE) %>%
            suppressMessages()

        save(niskin.reef, file=paste0(NISKIN_OUTPUT_PATH, 'jcu.event.reef.RData'))
        unlink(paste0(DATA_PATH, "/reports/STAGE",CURRENT_STAGE, "_", CURRENT_ITEM, "_.RData")) 
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SECTION = paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                                    item = CURRENT_ITEM),"\n\n"),
                               TABSET = paste0("::: panel-tabset \n\n"),
                               TABSET_END = paste0("::: \n\n"),
                               SUBSECTION_SQL = structure(paste0("## SQL syntax\n"),
                                                          parent = 'TABSET'),
                               SQL = structure(mmp__sql(paste0(NISKIN_INPUT_PATH, 'jcuEvent.sql')),
                                               parent = 'SUBSECTION_SQL')
                               )

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_GLIMPSE = structure(paste0("## Data glimpse\n"),
                                                              parent = 'TABSET'),
                               TAB = structure(mmp__add_table(mmp__glimpse_like(jcu.event.reef)),
                                               parent = 'SUBSECTION_GLIMPSE'),
                               TAB.CAP = structure(paste0("\n:Extraction of the first five records in each field from the JCU event niskin data. {#tbl-sql-jcuEvent}\n\n"),
                                                   parent = 'SUBSECTION_GLIMPSE')
                              )
    },
    LOG_FILE, item = CURRENT_NISKIN, Category = 'Data processing:', msg='Reading in Water Quality (JCU Event) data', return=TRUE)
    ## ----end
    
    ## 1. First level of data processing
    ## ---- JCU Event niskin process level 1
    MMP_tryCatch(
    {
        ## Check for missing reefs etc
        jcu.event.reef %>% dplyr::select(LOCATION_NAME, MMP_SITE_NAME,SHORT_NAME) %>% distinct %>%
            anti_join(wq.sites %>% dplyr::select(SHORT_NAME) %>% distinct) %>%
            left_join(wq.sites %>% dplyr::select(SHORT_NAME) %>% distinct) %>%
            suppressMessages() %>%
            suppressWarnings()
        MAXDATE=as.Date(paste0(reportYear,'-08-31'))  #this used to be -06-30
        MINDATE=MAXDATE - lubridate::years(1) + lubridate::days(1)
        niskin.jcu.event.reef1 = jcu.event.reef %>%
            mutate(
                COLLECTION_START_DATE = gsub('Sept','Sep', COLLECTION_START_DATE),
                Date = as.Date(COLLECTION_START_DATE, format='%d-%b-%Y'),           # convert to date
                oldSamplingYear=MMP_oldSamplingYear(Date),
                waterYear = MMP_waterYear(Date),
                reneeYear = MMP_reneeYear(Date),
                cwaterYear = MMP_categoricalWaterYear(Date),
                financialYear = MMP_financialYear(Date),                                    # add water year
                cfinancialYear = MMP_categoricalFinancialYear(Date),                        # categorical year
                Dt.num = MMP_decimalDate(Date)                                      # decimal year
            ) %>%
            mutate(SHORT_NAME=as.character(SHORT_NAME)) %>%
            mutate(Time=as.POSIXct(COLLECTION_START_DATE, format='%d-%b-%Y %H:%M:%S')) %>%
            mutate(
                Collection=interaction(MMP_SITE_NAME, Date)) %>%
            filter(Date<MAXDATE) %>%
            MMP_reorderReefs() 
        save(niskin.jcu.event.reef1, file=paste0(NISKIN_OUTPUT_PATH, 'niskin.jcu.event.reef1.RData'))
        save(jcu.event.reef, file=paste0(NISKIN_OUTPUT_PATH, 'jcu.event.reef.RData'))
    }, LOG_FILE, Category = 'Data processing:', msg='Initial parsing of Water Quality (JCU Event) data', return=TRUE)

    MMP_checkData(name = "niskin.jcu.event.reef1.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end
    ## 3. Further processing
    ## ---- JCU Event niskin process level 3
    MMP_tryCatch(
    {
        load(paste0(NISKIN_OUTPUT_PATH, 'niskin.jcu.event.reef1.RData'))
        niskin.jcu.event.reef1 <- niskin.jcu.event.reef1 %>%
            mutate(PN_UM=PN_SHIM_UM) %>%                                      #2019 now all the good JCU PN data is entered as PN_SHIM_UM (actually PN_SHIM_QAQC)
            MMP_limitDetection() %>%                                                           #correct for limit detection for HAND_NH4
            MMP_convertUnits() %>%                                                         #convert units from those used to store data (micro mol) to those  for reporting (micro gram)
             MMP_derivedChem()                                                          #derive a bunch of chemical combinations and ratios
        niskin.jcu.event.reef.av1 <-
            niskin.jcu.event.reef1 %>%
            MMP_aggregateWQDuplicates() %>%                   #aggregate the duplicates
            MMP_depthWeightedAverages() %>%                                                #generate a few different depth weighted averages
             MMP_designLatest(WQ=TRUE) %>%
             MMP_GBRMPA_specs(WQ=TRUE) %>%
             MMP_region_subregion(Source='JCU') %>%
             MMP_reorderReefs() %>% arrange(MMP_SITE_NAME) %>% droplevels() %>%
             mutate(Label = MMP_locationLabels(MMP_SITE_NAME),         #create names for consistency with forams data
                    Dtt.num = as.integer(Time),
                    Mnth = as.integer(format(Date, format='%m')),
                    Subregion=factor(Subregion, levels=unique(Subregion))) %>% droplevels
        save(niskin.jcu.event.reef.av1, file=paste0(NISKIN_OUTPUT_PATH, 'niskin.jcu.event.reef.av1.RData'))
    }, LOG_FILE, Category = "Data processing:", msg='Process Water Quality (JCU Event) data', return=TRUE)

    MMP_checkData(name = "niskin.jcu.event.reef.av1.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.suffix = "processed averages",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()

    ## ----end

    ## ---- JCU Event niskin outputs
    MMP_tryCatch(
    {
        load(file=paste0(NISKIN_OUTPUT_PATH, 'niskin.jcu.event.reef.av1.RData'))
        p <- ggplot(niskin.jcu.event.reef.av1 %>% 
                    dplyr:::select(SHORT_NAME, MMP_SITE_NAME,LATITUDE,Date,Subregion,Season) %>%
                    distinct %>%
                    arrange(desc(LATITUDE)) %>%
                    mutate(MMP_SITE_NAME=factor(MMP_SITE_NAME,levels=rev(unique(MMP_SITE_NAME))),
                           nms = paste0(MMP_SITE_NAME, ' (', SHORT_NAME,')'), 
                           nms = forcats::fct_reorder(nms, LATITUDE)),
                    aes(y=(nms), x=Date))+
            geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=MINDATE,xmax=MAXDATE), fill='grey', color=NA) +
            geom_point(aes(color=Season),show.legend=FALSE)+ggtitle('Water quality niskin data (JCU Event)')+
                                        #geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=minDate,xmax=maxDate), color='black', fill=NA) +    
            scale_y_discrete('') +
            scale_x_date('',date_breaks='2 years', date_labels='%Y')+
            scale_color_manual('',values=c('red','blue')) +
            facet_grid(Subregion~., scales='free') +
            ggplot2:::theme_grey() +
            theme(strip.background=element_rect(fill=NA,color='black',size=0.5),
                  strip.text.x=element_blank(),
                  panel.border=element_rect(fill=NA,color='black',size=0.5))
        
        ggsave(file=paste0(OUTPUT_PATH, '/figures/processed/niskin_jcu_event_reef_av1.png'),
               p,
               width=12, height=10, dpi = 100)

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_DESIGN = structure(paste0("## Sampling design\n"),
                                                             parent = 'TABSET'),
                               FIG_REF = structure(paste0("\n::: {#fig-sql-jcuEvent}\n"),
                                                   parent = 'SUBSECTION_DESIGN'),
                               FIG = structure(paste0("![](",OUTPUT_PATH,"/figures/processed/niskin_jcu_event_reef_av1.png)\n"),
                                               parent = "FIG_REF"),
                               FIG_CAP = structure(paste0("\nTemporal distribution of JCU event Niskin water quality samples. Red and blue symbols signify Dry and Wet season samples respectively. Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                                                   parent = 'FIG_REF'),
                               FIG_REF_END = structure(paste0("\n::: \n"),
                                                       parent = 'SUBSECTION_DESIGN')
                              )
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM)
        ## ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% str()
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% unlist() %>% paste(collapse = '')

        
    }, LOG_FILE, Category = "Data processing:", msg='Preparing report outputs for Water Quality (Niskin) data', return=TRUE)
    ## ----end
    
} else {
}

MMP_checkData(name = "niskin.jcu.event.reef.av1.RData",
              stage = paste0("STAGE", CURRENT_STAGE),
              item = CURRENT_ITEM,
              label.prefix = "Processed JCU Event niskin",
              label.suffix = "averaged data",
              PATH = NISKIN_OUTPUT_PATH)
MMP_openning_banner()

## ----end

## ---- JCU Event CY Event niskin
CURRENT_ITEM <- "jcuCYEventNiskin"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(NISKIN_OUTPUT_PATH, "niskin.cy.event.reef.av1.RData"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'cyEvent.csv'))) {
    
    ## 1. Read in data
    ## ---- CY Event niskin read data
    MMP_tryCatch(
    {
        cy.event.reef <- read_csv(paste0(NISKIN_INPUT_PATH, 'cyEvent.csv')) %>%
            suppressMessages()
        jcu_lookup = read_csv(paste0(PARAMS_PATH, '/jcu_location_lookup.csv'), trim_ws = TRUE) %>%
            suppressMessages()
        wq.sites = read_csv(paste0(PARAMS_PATH, '/wq.sites.csv'), trim_ws = TRUE) %>%
            suppressMessages()

        save(cy.event.reef, file=paste0(NISKIN_OUTPUT_PATH, 'cy.event.reef.RData'))
        unlink(paste0(DATA_PATH, "/reports/STAGE",CURRENT_STAGE, "_", CURRENT_ITEM, "_.RData")) 
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SECTION = paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                                    item = CURRENT_ITEM),"\n\n"),
                               TABSET = paste0("::: panel-tabset \n\n"),
                               TABSET_END = paste0("::: \n\n"),
                               SUBSECTION_SQL = structure(paste0("## SQL syntax\n"),
                                                          parent = 'TABSET'),
                               SQL = structure(mmp__sql(paste0(NISKIN_INPUT_PATH, 'cyEvent.sql')),
                                               parent = 'SUBSECTION_SQL')
                               )

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_GLIMPSE = structure(paste0("## Data glimpse\n"),
                                                              parent = 'TABSET'),
                               TAB = structure(mmp__add_table(mmp__glimpse_like(niskin.reef)),
                                               parent = 'SUBSECTION_GLIMPSE'),
                               TAB.CAP = structure(paste0("\n:Extraction of the first five records in each field from the Cape York event niskin data. {#tbl-sql-cyEvent}\n\n"),
                                                   parent = 'SUBSECTION_GLIMPSE')
                              )
    },
    LOG_FILE, item = CURRENT_NISKIN, Category = 'Data processing:', msg='Reading in Water Quality (CY) data', return=TRUE)
    ## ----end
    
    ## 1. First level of data processing
    ## ---- CY Event niskin process level 1
    MMP_tryCatch(
    {
        ## Check for missing samples
        cy.event.reef %>% dplyr::select(LOCATION_NAME, MMP_SITE_NAME,SHORT_NAME) %>% distinct %>%
            anti_join(wq.sites %>% dplyr::select(SHORT_NAME) %>% distinct) %>%
            left_join(wq.sites %>% dplyr::select(SHORT_NAME) %>% distinct) %>%
            suppressMessages() %>%
            suppressWarnings()
        MAXDATE=as.Date(paste0(reportYear,'-08-31'))  #this used to be -06-30
        MINDATE=MAXDATE - lubridate::years(1) + lubridate::days(1)
        niskin.cy.event.reef1 = cy.event.reef %>%
            mutate(
                COLLECTION_START_DATE = gsub('Sept','Sep', COLLECTION_START_DATE),
                Date = as.Date(COLLECTION_START_DATE, format='%d-%b-%Y'),           # convert to date
                oldSamplingYear=MMP_oldSamplingYear(Date),
                waterYear = MMP_waterYear(Date),
                reneeYear = MMP_reneeYear(Date),
                cwaterYear = MMP_categoricalWaterYear(Date),
                financialYear = MMP_financialYear(Date),                                    # add water year
                cfinancialYear = MMP_categoricalFinancialYear(Date),                        # categorical year
                Dt.num = MMP_decimalDate(Date)                                      # decimal year
            ) %>%
            mutate(Time=as.POSIXct(COLLECTION_START_DATE, format='%d-%b-%Y %H:%M:%S')) %>%
            mutate(SHORT_NAME=as.character(SHORT_NAME)) %>%
            mutate(
                Collection=interaction(MMP_SITE_NAME, Date)) %>%
            filter(Date<MAXDATE) %>%
            MMP_reorderReefs() 
        save(niskin.cy.event.reef1, file=paste0(NISKIN_OUTPUT_PATH, 'niskin.cy.event.reef1.RData'))
        save(cy.event.reef, file=paste0(NISKIN_OUTPUT_PATH, 'cy.event.reef.RData'))
    }, LOG_FILE, Category = 'Data processing:', msg='Initial parsing of Water Quality (CY Event) data', return=TRUE)

    MMP_checkData(name = "niskin.cy.event.reef1.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end
    ## 3. Further processing
    ## ---- CY Event niskin process level 3
    MMP_tryCatch(
    {
        load(paste0(NISKIN_OUTPUT_PATH, 'niskin.cy.event.reef1.RData'))
        niskin.cy.event.reef1 <- niskin.cy.event.reef1 %>%
            mutate(PN_UM=PN_SHIM_UM) %>%                                      #2019 now all the good JCU PN data is entered as PN_SHIM_UM (actually PN_SHIM_QAQC)
            MMP_limitDetection() %>%                                                           #correct for limit detection for HAND_NH4
            MMP_convertUnits() %>%                                                         #convert units from those used to store data (micro mol) to those  for reporting (micro gram)
             MMP_derivedChem()                                                          #derive a bunch of chemical combinations and ratios
        niskin.cy.event.reef.av1 <-
            niskin.cy.event.reef1 %>%
            MMP_aggregateWQDuplicates() %>%                   #aggregate the duplicates
            MMP_depthWeightedAverages() %>%                                                #generate a few different depth weighted averages
             MMP_designLatest(WQ=TRUE) %>%
             MMP_GBRMPA_specs(WQ=TRUE) %>%
             MMP_region_subregion(Source='JCU') %>%
             MMP_reorderReefs() %>% arrange(MMP_SITE_NAME) %>% droplevels() %>%
             mutate(Label = MMP_locationLabels(MMP_SITE_NAME),         #create names for consistency with forams data
                    Dtt.num = as.integer(Time),
                    Mnth = as.integer(format(Date, format='%m')),
                    Subregion=factor(Subregion, levels=unique(Subregion))) %>% droplevels
        save(niskin.cy.event.reef.av1, file=paste0(NISKIN_OUTPUT_PATH, 'niskin.cy.event.reef.av1.RData'))
    }, LOG_FILE, Category = "Data processing:", msg='Process Water Quality (CY Event) data', return=TRUE)

    MMP_checkData(name = "niskin.cy.event.reef.av1.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "CY Event niskin processed averages",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()

    ## ----end

    ## ---- CY Event niskin outputs
    MMP_tryCatch(
    {
        load(file=paste0(NISKIN_OUTPUT_PATH, 'niskin.cy.event.reef.av1.RData'))
        p <- ggplot(niskin.cy.event.reef.av1 %>% 
                    dplyr:::select(SHORT_NAME, MMP_SITE_NAME,LATITUDE,Date,Subregion,Season) %>%
                    distinct %>%
                    arrange(desc(LATITUDE)) %>%
                    mutate(MMP_SITE_NAME=factor(MMP_SITE_NAME,levels=rev(unique(MMP_SITE_NAME))),
                           nms = paste0(MMP_SITE_NAME, ' (', SHORT_NAME,')'), 
                           nms = forcats::fct_reorder(nms, LATITUDE)),
                    aes(y=(nms), x=Date))+
            geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=MINDATE,xmax=MAXDATE), fill='grey', color=NA) +
            geom_point(aes(color=Season),show.legend=FALSE)+ggtitle('Water quality niskin data (CY Event)')+
                                        #geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=minDate,xmax=maxDate), color='black', fill=NA) +    
            scale_y_discrete('') +
            scale_x_date('',date_breaks='2 years', date_labels='%Y')+
            scale_color_manual('',values=c('red','blue'), limits = c('Dry','Wet')) +
            facet_grid(Subregion~., scales='free') +
            ggplot2:::theme_grey() +
            theme(strip.background=element_rect(fill=NA,color='black',size=0.5),
                  strip.text.x=element_blank(),
                  panel.border=element_rect(fill=NA,color='black',size=0.5))
        
        ggsave(file=paste0(OUTPUT_PATH, '/figures/processed/niskin_cy_event_reef_av1.png'),
               p,
               width=12, height=10, dpi = 100)
        
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_DESIGN = structure(paste0("## Sampling design\n"),
                                                             parent = 'TABSET'),
                               FIG_REF = structure(paste0("\n::: {#fig-sql-cyEvent}\n"),
                                                   parent = 'SUBSECTION_DESIGN'),
                               FIG = structure(paste0("![](",OUTPUT_PATH,"/figures/processed/niskin_cy_event_reef_av1.png)\n"),
                                               parent = "FIG_REF"),
                               FIG_CAP = structure(paste0("\nTemporal distribution of Cape York event Niskin water quality samples. Red and blue symbols signify Dry and Wet season samples respectively. Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                                                   parent = 'FIG_REF'),
                               FIG_REF_END = structure(paste0("\n::: \n"),
                                                       parent = 'SUBSECTION_DESIGN')
                              )
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM)
        ## ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% str()
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM) %>% unlist() %>% paste(collapse = '')

        
    }, LOG_FILE, Category = "Data processing:", msg='Preparing report outputs for Water Quality (Niskin) data', return=TRUE)

    ## ----end
    
} else {
}

MMP_checkData(name = "niskin.cy.event.reef.av1.RData",
              stage = paste0("STAGE", CURRENT_STAGE),
              item = CURRENT_ITEM,
              label.prefix = "Processed",
              label = "",
              label.suffix = "averaged data",
              PATH = NISKIN_OUTPUT_PATH)
MMP_openning_banner()

## ----end

