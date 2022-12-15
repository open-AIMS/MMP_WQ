source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

OTHER_INPUT_PATH <- paste0(DATA_PATH, "/primary/other/")
OTHER_OUTPUT_PATH <- paste0(DATA_PATH, "/processed/other/")
MAXDATE=as.Date(paste0(reportYear,'-09-30'))
MINDATE=MAXDATE-years(1)+days(1)


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
    
    ## 1. Read in data
    ## ---- Tidal read data
    MMP_tryCatch({
        load(file=paste0(OTHER_INPUT_PATH,"tides.RData"))
        
    }, LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing', msg='Reading in tidal data', return=TRUE)
    ## ----end

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
        
    ## ---- tides outputs
    MMP_tryCatch(
    {
        load(file=paste0(OTHER_OUTPUT_PATH, 'tides.daily.RData'))

        tides.sum <-
            tides.daily %>%
            imap(~ .x %>% mutate(MMP_SITE_NAME = .y)) %>%
            bind_rows() %>%
            left_join(lookup, by = c("MMP_SITE_NAME" = "reef.alias")) %>%
            left_join(wq.sites %>%
                      dplyr::select(reef.alias, Latitude),
                      by = c("MMP_SITE_NAME" = "reef.alias")) %>%
            mutate(MMP_SITE_NAME = forcats::fct_reorder(MMP_SITE_NAME, Latitude, min, .desc = TRUE)) %>%
            group_by(Subregion) %>%
            nest()
        ## generate the figures
        tides.sum <- tides.sum %>%
            mutate(g = map(.x = data,
                          ~ .x %>% 
                              ggplot(aes(y = Range, x = as.Date(Date))) +
                              geom_rect(data = NULL, aes(ymin=-Inf,ymax=Inf,xmin=as.Date(paste0(reportYear,'-10-01'))-years(1)+days(1), xmax=as.Date(paste0(reportYear,'-10-01'))), fill='grey', color=NA) +
                              geom_line() +
                              facet_grid(MMP_SITE_NAME  ~ ., space = 'free', switch = 'y') +
                              scale_y_continuous('') +
                              scale_x_date('',expand = c(0,0),date_breaks='2 years', date_labels='%Y') +
                              theme_classic() +
                              theme(strip.text.y.left = element_text(angle = 0, vjust = 0.5, hjust = 1),
                                    strip.placement = 'outside',
                                    strip.background = element_blank())
                          ## filter(Subregion == 'Burdekin') %>%
                          ))
        ## output the figures 
        purrr::pwalk(
                   .l = list(path = paste0(OUTPUT_PATH,
                                           '/figures/processed/tides.daily_',
                                           tides.sum$Subregion, ".png"),
                             plot = tides.sum$g),
                   .f = function(path, plot){
                       n <- plot$data %>% pull(MMP_SITE_NAME) %>% unique() %>% length()
                       dims <- wrap_dims(n, ncol = 1, nrow = NULL)
                       ggsave(filename = path,
                              plot = plot,
                              width = 10,
                              height = 1*dims[1]+0.5,
                              dpi = 100)
                   }
               )

        ## output the doc list initial structure        
        unlink(paste0(DATA_PATH, "/reports/STAGE",CURRENT_STAGE, "_", CURRENT_ITEM, "_.RData")) 
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SECTION = paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                                    item = CURRENT_ITEM),"\n\n"),
                               TABSET = paste0("::: panel-tabset \n\n"),
                               TABSET_END = paste0("::: \n\n"),
                              )

        ## output the doc list 
        purrr::pwalk(
                   .l = list(tides.sum$Subregion),
                   .f = function(S) {
                       SS <- str_replace_all(S, ' ','_')
                       MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                                              !!!setNames(list(
                                                     structure(paste0("## ", S, "\n"),
                                                               parent = 'TABSET')),
                                                     paste0('SUBSECTION_DESIGN_',S)
                                                     ), 
                                              !!!setNames(list(
                                                     structure(paste0("\n::: {#fig-sql-tides-",SS,"}\n"),
                                                               parent = paste0('SUBSECTION_DESIGN_', S))),
                                                     paste0('FIG_REF_',S)
                                                      ),
                                              !!!setNames(list(
                                                     structure(paste0("![](",OUTPUT_PATH,"/figures/processed/tides.daily_", S, ".png)\n"),
                                                               parent = paste0("FIG_REF_", S))),
                                                     paste0('FIG_', S)
                                                     ),
                                              !!!setNames(list(
                                                          structure(paste0("\nTemporal tidal trends for the ", S, " subregion. Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                                                                    parent = paste0('FIG_REF_',S))),
                                                          paste0('FIG_CAP_',SS)),
                                              !!!setNames(list(
                                                     structure(paste0("\n::: \n"),
                                                               parent = paste0('SUBSECTION_DESIGN_',S))),
                                                     paste0('FIG_END_', S)
                                                     ) 
                                              )
                   }
               )


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
    ## 1. Read in data
    ## ---- BOM weather data data
    MMP_tryCatch(
    {
        bom <- read_csv(paste0(OTHER_INPUT_PATH, 'bom.csv')) %>%
            suppressMessages()
        unlink(paste0(DATA_PATH, "/reports/STAGE",CURRENT_STAGE, "_", CURRENT_ITEM, "_.RData")) 
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SECTION = paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                                    item = CURRENT_ITEM),"\n\n"),
                               TABSET = paste0("::: panel-tabset \n\n"),
                               TABSET_END = paste0("::: \n\n"),
                               SUBSECTION_SQL = structure(paste0("## SQL syntax\n"),
                                                          parent = 'TABSET'),
                               SQL = structure(mmp__sql(paste0(OTHER_INPUT_PATH, 'bom.sql')),
                                               parent = 'SUBSECTION_SQL')
                               )

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_GLIMPSE = structure(paste0("## Data glimpse\n"),
                                                              parent = 'TABSET'),
                               TAB = structure(mmp__add_table(mmp__glimpse_like(bom)),
                                               parent = 'SUBSECTION_GLIMPSE'),
                               TAB.CAP = structure(paste0("\n:Extraction of the first five records in each field from the BO weather data. {#tbl-sql-bom}\n\n"),
                                                   parent = 'SUBSECTION_GLIMPSE')
                              )

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing', msg='Reading in BOM weather data', return=TRUE)
    ## ----end
    
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
                   Dt.num = MMP_decimalDate(Date))
    
        save(bom.weather, file=paste0(OTHER_OUTPUT_PATH, 'bom.weather.RData'))
    }, LOG_FILE, Category = 'Data processing', msg='Initial parsing of BOM (historical weather) data', return=TRUE)
    ## ----end

    ## ---- BOM weather outputs
    MMP_tryCatch(
    {
        load(file=paste0(OTHER_OUTPUT_PATH, 'bom.weather.RData'))

        ## design plot
        p <- ggplot(bom.weather, aes(y=LOCATION, x=Date)) +
            geom_rect(aes(ymin=-Inf,
                          ymax=Inf,
                          xmin=MAXDATE-years(1)+days(1),
                          xmax=MAXDATE), fill='grey') +
            geom_point()+
            ggtitle('BOM weather data')+
            scale_y_discrete('Location') +
            theme_classic() +
            theme(axis.line.x = element_line(),
                  axis.line.y = element_line())
        
        ggsave(file=paste0(OUTPUT_PATH, '/figures/processed/bom.weather.png'),
               p,
               width=12, height=10, dpi = 100)

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_DESIGN = structure(paste0("## Sampling design\n"),
                                                             parent = 'TABSET'),
                               FIG_REF = structure(paste0("\n::: {#fig-sql-bom.weather}\n"),
                                                   parent = 'SUBSECTION_DESIGN'),
                               FIG = structure(paste0("![](",OUTPUT_PATH,"/figures/processed/bom.weather.png)\n"),
                                               parent = "FIG_REF"),
                               FIG_CAP = structure(paste0("\nTemporal distribution of BOM weather data. Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                                                   parent = 'FIG_REF'),
                               FIG_REF_END = structure(paste0("\n::: \n"),
                                                       parent = 'SUBSECTION_DESIGN')
                              )
        ## MMP_get_report_list(CURRENT_STAGE, CURRENT_ITEM)
 
        ## Wind data values (not just dates)
        filltime <- expand.grid(LOCATION=unique(bom.weather$LOCATION),
                                Date=seq(MINDATE,MAXDATE,by='1 day'))
        
        p <- bom.weather %>%
            filter(Date>as.Date('2005-01-01')) %>%
            full_join(filltime) %>%
            arrange(Date) %>%
            ggplot(aes(y=WIND_SPEED, x=Date)) +
            geom_line()+
            ggtitle('Recent BOM weather data') +
            facet_wrap(~LOCATION)+
            scale_y_continuous('Wind speed (m)') +
            theme_classic() +
            theme(axis.line.x = element_line(),
                  axis.line.y = element_line())

        ggsave(file=paste0(OUTPUT_PATH, '/figures/processed/bom.png'),
               p,
               width=12, height=10, dpi = 100)

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_SAMPLES = structure(paste0("## BOM Wind data\n"),
                                                             parent = 'TABSET'),
                               FIG_REF_SAMPLES = structure(paste0("\n::: {#fig-sql-bom}\n"),
                                                   parent = 'SUBSECTION_SAMPLES'),
                               FIG_SAMPLES = structure(paste0("![](",OUTPUT_PATH,"/figures/processed/bom.png)\n"),
                                               parent = "FIG_REF_SAMPLES"),
                               FIG_CAP_SAMPLES = structure(paste0("\nBOM weather data. Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                                                   parent = 'FIG_REF_SAMPLES'),
                               FIG_REF_END_SAMPLES = structure(paste0("\n::: \n"),
                                                       parent = 'SUBSECTION_SAMPLES')
                              )
        ## Wind data for original sites
        lookup <- read.csv('../parameters/lookup.csv', strip.white = TRUE)
        p <- bom.weather %>%
            filter(Date>as.Date('2005-01-01')) %>%
            full_join(filltime) %>% arrange(Date)  %>%
            right_join(lookup %>% dplyr::select(LOCATION=BOM, Region, Subregion)) %>%
            filter(!is.na(STATION_NUMBER)) %>% 
            ggplot(aes(y=WIND_SPEED, x=Date))+
            geom_line()+
            ggtitle('Recent BOM weather data') +
            facet_wrap(~LOCATION)+
            scale_y_continuous('Wind speed (m)') +
            theme(panel.background=element_rect(color='black'),
                  text=element_text(size=10),
                  axis.text.x=element_text(angle=20),
                  panel.grid.major.x=element_line(color='gray', size=0.5),
                  panel.grid.major.y=element_line(color='gray', size=0.5)
                  )
        ggsave(file=paste0(OUTPUT_PATH, '/figures/processed/bom1.png'),
               p,
               width=12, height=10, dpi = 100)

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_SAMPLES1 = structure(paste0("## BOM Wind data\n"),
                                                             parent = 'TABSET'),
                               FIG_REF_SAMPLES1 = structure(paste0("\n::: {#fig-sql-bom1}\n"),
                                                   parent = 'SUBSECTION_SAMPLES1'),
                               FIG_SAMPLES1 = structure(paste0("![](",OUTPUT_PATH,"/figures/processed/bom1.png)\n"),
                                               parent = "FIG_REF_SAMPLES1"),
                               FIG_CAP_SAMPLES1 = structure(paste0("\nBOM weather data for original MMP WQ sites. Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                                                   parent = 'FIG_REF_SAMPLES1'),
                               FIG_REF_END_SAMPLES1 = structure(paste0("\n::: \n"),
                                                       parent = 'SUBSECTION_SAMPLES1')
                              )
    }, LOG_FILE, Category = "Data processing:", msg='Preparing report outputs for BOM weather data', return=TRUE)

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
    
    ## 1. Read in data
    ## ---- discharge read data
    MMP_tryCatch(
    {
        discharge <- read_csv(paste0(OTHER_INPUT_PATH, 'discharge.csv')) %>%
            suppressMessages()
        unlink(paste0(DATA_PATH, "/reports/STAGE",CURRENT_STAGE, "_", CURRENT_ITEM, "_.RData")) 
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SECTION = paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                                    item = CURRENT_ITEM),"\n\n"),
                               TABSET = paste0("::: panel-tabset \n\n"),
                               TABSET_END = paste0("::: \n\n"),
                               SUBSECTION_SQL = structure(paste0("## SQL syntax\n"),
                                                          parent = 'TABSET'),
                               SQL = structure(mmp__sql(paste0(OTHER_INPUT_PATH, 'discharge.sql')),
                                               parent = 'SUBSECTION_SQL')
                               )

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_GLIMPSE = structure(paste0("## Data glimpse\n"),
                                                              parent = 'TABSET'),
                               TAB = structure(mmp__add_table(mmp__glimpse_like(discharge)),
                                               parent = 'SUBSECTION_GLIMPSE'),
                               TAB.CAP = structure(paste0("\n:Extraction of the first five records in each field from the river discharge data. {#tbl-sql-discharge}\n\n"),
                                                   parent = 'SUBSECTION_GLIMPSE')
                              )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing', msg='Reading in discharge data', return=TRUE)
    ## ----end

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

    }, LOG_FILE, Category = 'Data processing', msg='Initial parsing of discharge data', return=TRUE)
    ## ----end

    ## 2. Extract the historic river discharge data
    ## ---- discharge process level 2
    MMP_tryCatch(
    {
        load(file=paste0(DATA_PATH, '/primary/other/river.lookup.RData'))
        discharge<-discharge %>%
            left_join(river.lookup) %>%
            mutate(Year=ifelse(month(Date) > 9, year(Date)+1, year(Date)))
    }, LOG_FILE, Category = 'Data processing', msg='Discharge gauge correction factors', return=TRUE)

    ## ----end


    ## 3. Further processing 
    ## ---- discharge process level 3
    MMP_tryCatch(
    {
        ## discharge <- discharge %>%
        ##     mutate(Subregion=
        ##                ifelse(subregion=='Cape York', 'Cape York',
        ##                ifelse(subregion=='Daintree', 'Barron Daintree',
        ##                ifelse(subregion=='Johnstone', 'Johnstone Russell Mulgrave',
        ##                ifelse(subregion=='Tully', 'Tully Herbert',
        ##                ifelse(subregion=='Burdekin','Burdekin',
        ##                ifelse(subregion=='Proserpine','Mackay Whitsunday','Fitzroy')))))))
        ## discharge <- discharge %>%
        ##     mutate(Region = ifelse(Subregion %in% c('Barron Daintree','Johnstone Russell Mulgrave', 'Tully Herbert'), 'Wet Tropics', as.character(Subregion))) %>%
        ##     mutate(Subregion = factor(Subregion, 
        
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
        ## discharge.baseLine <- read.csv(paste0(PARAMS_PATH, "/LTmedian.discharge.river.csv"),
        ##                                strip.white = TRUE)
        ## discharge.baseLine <- discharge.baseLine %>%
        ##     mutate(River = ifelse(River=="O'Connell River", 'OConnell River',as.character(River)))
        ## save(discharge.baseline, file=paste0(OTHER_OUTPUT_PATH, 'discharge.baseline.RData'))
        load(file=paste0(DATA_PATH, '/primary/other/discharge.baseline.RData'))
        
        discharge <- discharge %>%
            left_join(discharge.baseline)
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
    
    ## ---- discharge outputs
    MMP_tryCatch(
    {
        ## ---- sampling design
        {
            load(file=paste0(OTHER_OUTPUT_PATH, 'discharge.RData'))
            p <- discharge %>%
                filter(Date>as.Date('2005-01-01')) %>%
                ggplot(aes(y=RIVER_NAME, x=Date))+
                geom_blank() +
                geom_rect(aes(ymin=-Inf,
                              ymax=Inf,
                              xmin=MAXDATE-years(1)+days(1),
                              xmax=MAXDATE), fill='grey')  +
                geom_point()+
                scale_y_discrete('') +
                ggtitle('River discharge data') +
                facet_grid(Subregion ~ ., scales = 'free_y', space = 'free') +
                theme_classic()
            
            ggsave(file=paste0(OUTPUT_PATH, '/figures/processed/discharge.png'),
                   p,
                   width=12, height=10, dpi = 100)

            MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                                   SUBSECTION_DESIGN = structure(paste0("## Sampling design\n"),
                                                                 parent = 'TABSET'),
                                   FIG_REF = structure(paste0("\n::: {#fig-sql-discharge}\n"),
                                                       parent = 'SUBSECTION_DESIGN'),
                                   FIG = structure(paste0("![](",OUTPUT_PATH,"/figures/processed/discharge.png)\n"),
                                                   parent = "FIG_REF"),
                                   FIG_CAP = structure(paste0("\nTemporal distribution of river discharge. Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                                                       parent = 'FIG_REF'),
                                   FIG_REF_END = structure(paste0("\n::: \n"),
                                                           parent = 'SUBSECTION_DESIGN')
                                   )
        }
        ## ----end
        
        ## ---- individual discharge plots
        {
            load(file=paste0(OTHER_OUTPUT_PATH, 'discharge.RData'))
            load(file=paste0(OTHER_OUTPUT_PATH, 'discharge.annual.RData'))
            load(file=paste0(DATA_PATH, '/primary/other/discharge.baseline.RData'))
            load(file=paste0(DATA_PATH, '/primary/other/river.lookup.RData'))

            discharge.subregion <-
                discharge %>%
                filter(Date >= as.Date('2006-01-01')) %>%
                group_by(Subregion) %>%
                nest()
            discharge.subregion <- discharge.subregion %>%
                mutate(Discharge = map(.x = data,
                                       .f = ~ .x %>%
                                           group_by(Date) %>%
                                           summarise(DISCHARGE_RATE_DAILY = sum(PARAM_VALUE))))
            discharge.annual <- discharge.annual %>%
                mutate(Date = as.Date(paste0(Year, '-09-01'))) %>%
                filter(Date >= as.Date('2006-01-01')) %>%
                group_by(Subregion) %>%
                nest() %>%
                rename(Annual = data)
            
            discharge.baseline <-
                discharge.baseline %>%
                left_join(river.lookup %>% distinct(River, correction.factor, Region, Subregion)) %>%
                mutate(Region = factor(River, levels = unique(River)),
                       Subregion = factor(Subregion, levels = unique(Subregion))) %>%
                group_by(Subregion) %>%
                ## summarise(LTmedian = sum(LT.median * correction.factor)) %>% 
                summarise(LTmedian = sum(LT.median)) %>% 
                group_by(Subregion) %>%
                nest() %>% 
                rename(Baseline = data)
                
            discharge.subregion <-
                discharge.subregion %>%
                full_join(discharge.annual) %>% 
                full_join(discharge.baseline)
                
            ## discharge.subregion[1,'data'][[1]][[1]] %>% as.data.frame %>% head 
            ## discharge.subregion[4,'Baseline'][[1]][[1]] %>% as.data.frame %>% head 

            discharge.subregion <-
                discharge.subregion %>%
                mutate(plot = pmap(.l = list(Discharge, Annual, Baseline),
                                   .f = ~ mmp__discharge_plot(Discharge = ..1,
                                                            Annual = ..2,
                                                            Baseline = ..3)
                                   ))
            
            ## discharge.subregion[1,'plot'][[1]][[1]] 
            ## dev.off()

            ## save the figures
            walk2(.x = discharge.subregion$plot,
                  .y = discharge.subregion$Subregion,
                  .f = ~ ggsave(paste0(OUTPUT_PATH, '/figures/processed/discharge_', .y, '.png'),
                                .x,
                                width = 10, height = 6,
                                dpi = 100)
                  )
            
            ## report string
            MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                                   SUBSECTION_TEMPORAL_TRENDS = structure("## Temporal trends\n",
                                                                          parent = "TABSET"),
                                   TABSET_trends = structure(paste0("::: panel-tabset \n\n"),
                                                             parent = "SUBSECTION_TEMPORAL_TRENDS"),
                                   TABSET_trends_END = structure(paste0("::: \n\n"),
                                                                parent = "SUBSECTION_TEMPORAL_TRENDS")
                                   )

            walk(.x = discharge.subregion$Subregion,
                 .f = function(S) {
                       SS <- str_replace_all(S, ' ','_')
                       MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                                              !!!setNames(list(
                                                      structure(paste0("### ", S, "\n"),
                                                                parent = 'TABSET_trends')),
                                                      paste0('SUBSECTION_DESIGN_',S)
                                                      ), 
                                              !!!setNames(list(
                                                     structure(paste0("\n::: {#fig-sql-discharge-",SS,"}\n"),
                                                               parent = paste0('SUBSECTION_DESIGN_', S))),
                                                     paste0('FIG_REF_',S)
                                                     ),
                                              !!!setNames(list(
                                                     structure(paste0("![](",OUTPUT_PATH,"/figures/processed/discharge_", S, ".png)\n"),
                                                               parent = paste0("FIG_REF_", S))),
                                                     paste0('FIG_', S)
                                                     ),
                                              !!!setNames(list(
                                                          structure(paste0("\nDaily river discharge (blue) and annual river discharge (red) for the ", S, " subregion. The dashed horizontal line represents the long term median discharge.  Dark vertical band represents the ",as.numeric(reportYear),"/",as.numeric(reportYear)," reporting domain.\n"),
                                                                    parent = paste0('FIG_REF_',S))),
                                                          paste0('FIG_CAP_',SS)),
                                              !!!setNames(list(
                                                     structure(paste0("\n::: \n"),
                                                               parent = paste0('SUBSECTION_DESIGN_',S))),
                                                     paste0('FIG_END_', S)
                                                     ) 
                                              )
                 }
                 )
        }
        ## ----end

        ## ---- table
        {
            load(file=paste0(OTHER_OUTPUT_PATH, 'discharge.RData'))
            load(file=paste0(DATA_PATH, '/primary/other/river.lookup.RData'))
            REGIONS <- river.lookup %>% pull(Region) %>% unique
            ## generate a lookup of shortened river names (this is for
            ## arranging the rivers in order from North to South as
            ## per the arrangement in the river.lookup file)
            RIVERS <- river.lookup %>% pull(River) %>% unique %>% str_replace('\\ River', '') %>%
                str_replace('-','|') %>%
                str_replace('Water Park Creek','Waterpark') %>%
                str_subset("^$", negate = TRUE)

            ## - for each RIVER_NAME
            ##   - make a river label that includes the station id
            ## - some rivers have changed their id's so average or sum per river
            ## - calculate discharge as discharge times the correction factor
            ## - in a given year, some rivers have multiple gauges, sum these together
            ## - remove RIVER_NAME
            ## - sort by year
            ## - declare factor levels
            ## - widen the data so that their is a column per year
            discharge.table <- discharge %>%
                group_by(RIVER_NAME) %>%
                mutate(STATION_ID=paste0(unique(RIVER_NAME), " (",joinRiverIDs(unique(STATION_ID)),")"))  %>%
                ungroup %>%
                group_by(RIVER_NAME, correction.factor,waterYear) %>%
                summarise(Region=(unique(Region)),
                          River=unique(STATION_ID),
                          Median=median(as.numeric(LT.median), na.rm=TRUE),
                          Dis=sum(PARAM_VALUE, na.rm=TRUE)) %>%
                mutate(Dis=Dis*correction.factor) %>%
                ungroup %>%
                group_by(RIVER_NAME, waterYear) %>%
                summarise(Region = unique(na.omit(Region)),
                          Dis = sum(Dis, na.rm = TRUE),
                          River = unique(River),
                          Median = median(Median, na.rm = TRUE)) %>%
                ungroup() %>%
                dplyr::select(-RIVER_NAME) %>% 
                arrange(waterYear) %>% 
                mutate(Region=factor(Region, levels=unique(Region)),
                       River=factor(River, levels=unique(River))) %>%    
                pivot_wider(id_cols = c(Region, River, Median),
                            values_from = Dis, names_from = waterYear)
            ## Restrict table to the last 15 years
            nc <- ncol(discharge.table)
            discharge.table <- data.frame(discharge.table[,1:2],
                                          "Long term" = discharge.table$Median,
                                          discharge.table[,(nc-min(14,nc-3)+1):(nc)])
            colnames(discharge.table)[-1:-3] <- as.character(
                as.numeric(gsub("X","",colnames(discharge.table)[-1:-3])))
            ## remove the word 'river' from the labels
            discharge.table <- discharge.table %>%
                mutate(
                    River = factor(River,
                                   levels = River[unlist(sapply(1:length(RIVERS), function(x) str_which(River, RIVERS[x])))]),
                    River = str_replace(River, "\\ River", ""),
                    Region = factor(Region, levels = REGIONS)) %>%
                arrange(Region, River)

            ## express discharge values relative to long term averages
            discharge.table <- discharge.table %>%
                mutate(across(matches("[0-9]{4}"),
                              function(x) round(x/Long.term,2))) 

            ## get a vector of column numbers of the focal years                
            wch <- discharge.table %>% colnames() %>% str_which("^[0-9]{4}$") 
            ## create a reference table of colours based on discharge ratio
            table.colours <- discharge.table %>%
                mutate(across(matches("[0-9]{4}"),
                              function(x) case_when(x<1.5 ~ "black",
                                                    x>=1.5 & x < 2 ~ heat.colors(5)[3],
                                                    x>=2 & x < 3 ~ heat.colors(5)[2],
                                                    x>=3 ~ heat.colors(5)[1]
                                                   ))) %>%
                mutate(across(matches("[0-9]{4}"), function(x) replace_na(x, "white")))

            ## convert table to kable table
            discharge.table.kbl <- discharge.table %>%
                ## knitr::kable(format = 'markdown') 
                knitr::kable(format = 'html', caption = '**{@tbl-discharge}**: A new caption') %>%
                kableExtra::kable_styling(fixed_thead = TRUE) %>%
                purrr::reduce(wch, function(x, y) {
                    kableExtra::column_spec(x,y, color = table.colours[,y])
                    }, .init = .
                    )
            ##     kableExtra::scroll_box(height = '500px', width = '100%')
            ## discharge.table.kbl

            ## discharge.table.kbl <- discharge.table %>%
            ##     DT::datatable(
            ##             caption = 'This is the caption',
            ##             extensions = c('Buttons','FixedColumns',
            ##                            'FixedHeader', 'Scroller'),
            ##             options = list(dom = 'BFrtip',
            ##                            buttons = c('copy', 'csv',
            ##                                        'excel', 'pdf', 'print'),
            ##                            fixedColumns = list(leftColumns = 3),
            ##                            scrollX = TRUE,
            ##                            pageLength = 10,
            ##                            fixedHeader = TRUE,
            ##                            scrollY = 400,
            ##                            scroller = TRUE),
            ##             rownames = FALSE) 
            ## save(discharge.table.kbl, file = paste0(OUTPUT_PATH, "/tables/discharge_table.RData"))
            ##     ## htmltools::save_html(file = paste0(OUTPUT_PATH, "/tables/discharge_table.html")) 
                ## htmltools::tagList() %>% paste()
            discharge.table.kbl <- mmp__make_table_chunk(tab = discharge.table.kbl,
                                 caption = "Relative annual freshwater discharge (fraction of long-term median) for the major GBR Catchment rivers influencing the sampling sites of the MMP Inshore Water Quality Monitoring Program. Shaded cells highlight years for which river flow exceeded the median annual flow as estimated from available long-term time series for each river (LT median; from October 1970 to August 2017): yellow= 1.5 to 2-times LT median, orange= 2 to 3 times LT median, red= >3-times LT median. Records for the 2016 water year are incomplete (to August 2017). Discharge data were supplied by the Queensland Department of Natural Resources and Mines (gauging station codes given after river names). *** Indicates years for which >15% of daily flow estimates were not available, ** similarly indicate years for which >15% of daily flow was not available but these missing records are likely have been zero flow and so annual flow estimates are valid, whereas an * indicates that between 5% and 15% of daily observations were missing. Discharge data were supplied by the Queensland Department of Natural Resources and Mines (gauging station codes given after river names).")


            
            ## ## - dump and capture the raw kable object
            ## ## - create an R chunk as a string to put into the qmd
            ## a <- paste(capture.output(dump("discharge.table.kbl","")), collapse='') %>%
            ##     str_replace_all("NA","")
            ## discharge.table.kbl <-
            ##     paste0('\n```{r dischargeTable, results = "asis", echo=FALSE}\n',
            ##            '#| label: tbl-dischargeTable\n',
            ##            '#| tbl-cap: "Relative annual freshwater discharge (fraction of long-term median) for the major GBR Catchment rivers influencing the sampling sites of the MMP Inshore Water Quality Monitoring Program. Shaded cells highlight years for which river flow exceeded the median annual flow as estimated from available long-term time series for each river (LT median; from October 1970 to August 2017): yellow= 1.5 to 2-times LT median, orange= 2 to 3 times LT median, red= >3-times LT median. Records for the 2016 water year are incomplete (to August 2017). Discharge data were supplied by the Queensland Department of Natural Resources and Mines (gauging station codes given after river names). *** Indicates years for which >15% of daily flow estimates were not available, ** similarly indicate years for which >15% of daily flow was not available but these missing records are likely have been zero flow and so annual flow estimates are valid, whereas an * indicates that between 5% and 15% of daily observations were missing. Discharge data were supplied by the Queensland Department of Natural Resources and Mines (gauging station codes given after river names)."\n\n',
            ##            "options(knitr.kable.NA = '')\n",
            ##           ## paste0('load("',OUTPUT_PATH,'/tables/discharge_table.RData")\n'),
            ##            a,
            ##           '\n',
            ##           '\ndischarge.table.kbl\n',
            ##           ## paste0('cat("<table>",paste0("<caption>", "(#tab:dischargeTable)", "caption", "Relative annual freshwater discharge (fraction of long-term median) for the major GBR Catchment rivers influencing the sampling sites of the MMP Inshore Water Quality Monitoring Program. Shaded cells highlight years for which river flow exceeded the median annual flow as estimated from available long-term time series for each river (LT median; from October 1970 to August 2017): yellow= 1.5 to 2-times LT median, orange= 2 to 3 times LT median, red= >3-times LT median. Records for the 2016 water year are incomplete (to August 2017). Discharge data were supplied by the Queensland Department of Natural Resources and Mines (gauging station codes given after river names). *** Indicates years for which >15% of daily flow estimates were not available, ** similarly indicate years for which >15% of daily flow was not available but these missing records are likely have been zero flow and so annual flow estimates are valid, whereas an * indicates that between 5% and 15% of daily observations were missing. Discharge data were supplied by the Queensland Department of Natural Resources and Mines (gauging station codes given after river names).", "</caption>"),"</table>", sep ="\n")\n'),
            ##           '```\n\n'
            ##           ) %>%
            ##     str_replace_all("\"", "'")
            

            
                ## knitr::kable(format = "html",
                ##              table.attr = "class=\"table2\"") 
                ## kableExtra::scroll_box(fixed_thead = TRUE, height = "100%", width = "100%")
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_annual = structure(paste0("## Annual discharge\n"),
                                                              parent = 'TABSET'),
                               TAB = structure(discharge.table.kbl,
                                               parent = 'SUBSECTION_annual')## ,
                              )

        }
        ## ----end
    }, LOG_FILE, Category = 'Data processing:', msg='Producing discharge outputs', return=TRUE)
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
