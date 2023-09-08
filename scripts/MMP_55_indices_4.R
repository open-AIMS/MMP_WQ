source("MMP_functions.R")
source("MMP_functions_indices.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}


NISKIN_INPUT_PATH <- paste0(DATA_PATH, "/processed/niskin/")
PARAMS_INPUT_PATH <- paste0(DATA_PATH, "/primary/other/")
FLNTU_INPUT_PATH <- paste0(DATA_PATH, "/processed/loggers/")
INDICES_OUTPUT_PATH <- paste0(DATA_PATH, "/indices/")
FIGURE_OUTPUT_PATH <- paste0(OUTPUT_PATH, "/figures/indices/")

## Type 4 index  
## ---- Type 4
CURRENT_ITEM <- "Type4"
## mmp__add_status(stage = paste0("STAGE", CURRENT_STAGE),
##                 item = CURRENT_ITEM,
##                 name = "Type 4",
##                 status = "progress")
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(INDICES_OUTPUT_PATH,"wq.alt4.idx.RData"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'wq.all.reef.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/wq.guidelines.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/wq.units.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/names_lookup.RData')) 
    ) {

    MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
                           SUBSECTION_4 = structure(paste0("## Type 4\n"),
                                                    parent = 'TABSET'),
                           TABSET_4 = structure(paste0("\n:::: panel-tabset\n"),
                                                   parent = 'SUBSECTION_4'),
                           TABSET_4_END = structure(paste0("\n:::: \n"),
                                                       parent = 'SUBSECTION_4')
                           )

    ## 1. Read in data
    ## ---- Read in data
    MMP_tryCatch(
    {
        load(file=paste0(NISKIN_INPUT_PATH, 'wq.all.reef.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.units.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData')) 
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))

        lookup <- lookup %>%
            left_join(names_lookup) %>%
            suppressWarnings() %>%
            suppressMessages()
        wq.sites <- wq.sites %>%
            left_join(names_lookup) %>%
            left_join(lookup %>%
                      dplyr:::select(MMP_SITE_NAME,Region,Subregion)) %>%
            suppressWarnings() %>%
            suppressMessages()
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Reading in data for alt4 indices.', return=TRUE)
    ## ----end

    
    ## 2. Calculate indices
    ## ---- Calculate indices 
    MMP_tryCatch(
    {
        wq.alt4.idx <-
            (wq.alt4.qaqc2 <-
                 (wq.alt4.qaqc1 <-
                      (wq.alt4.qaqc <- wq.all.reef %>%
                           ungroup %>% filter(waterYear>2015) %>%
                           filter(!(Source=='JCU Niskin' & Measure %in% c('PP.wm','PN.wm')),
                                  !(Source=='CY Niskin' & Measure %in% c('PP.wm','PN.wm'))) %>%
                           droplevels() %>%
                           group_by(MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                                    GBRMPA_water_area,Region,Reg,Subregion,Subreg,Source,Measure) %>%
                           arrange(MMP_SITE_NAME,Source,Measure,waterYear) %>%
                           filter(!is.na(MMP_SITE_NAME)) %>%
                           left_join(wq.guidelines %>%
                                     dplyr::select(GBRMPA_group,Measure,GL.Season,Location,
                                                   GL,DirectionOfFailure,SHORT_NAME,Latitude)) %>%
                           filter(GL.Season=='Annual')) %>%
                      ungroup() %>%
                      group_by(Source,MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                               GBRMPA_water_area,Region,Reg,Subregion,Subreg,waterYear,
                               cwaterYear,Measure,Location) %>%
                      summarise(
                          GL=unique(GL),
                          DirectionOfFailure=unique(DirectionOfFailure),
                          Latitude=mean(Latitude,na.rm=TRUE),
                          Value.Mean=mean(Value, na.rm=TRUE),
                          Value.Median=median(Value, na.rm=TRUE),
                          Value=ifelse(unique(Location)=='Median',
                                       median(Value, na.rm=TRUE),
                                       mean(Value, na.rm=TRUE))
                      )
                 ) %>%
                 ungroup() %>%
                 mutate(Index=MMP_WQI_lastYear(.)) %>%
                 dplyr::select(-Source,-GL,-DirectionOfFailure,-Value,-Value.Mean,
                               -Value.Median,-Location,-Latitude) %>%
                 group_by(MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                          GBRMPA_water_area,Region,Reg,Subregion,Subreg,waterYear,
                          cwaterYear,Measure) %>%
                 summarize(Index=mean(Index,na.rm=TRUE))
            ) %>%
            spread(Measure,Index) %>%
            mutate(CombinedTurb=rowMeans(cbind(NTU,SECCHI_DEPTH.wm,TSS_MGPERL.wm), na.rm=TRUE),
                   Index=rowMeans(cbind(DRIFTCHL_UGPERL.wm,CombinedTurb,
                                        PN.wm,PP.wm,NOx.wm), na.rm=TRUE)) %>%
            mutate(reportCardYear=as.Date(paste0(waterYear,'-01-01'))) %>%
            suppressMessages() %>%
            suppressWarnings()

        wq.alt4.idx.subregion <- wq.alt4.idx %>%
            group_by(Subregion,Subreg,reportCardYear) %>%
            summarize(Index=median(Index,na.rm=TRUE))

        wq.alt4.idx.region <- wq.alt4.idx %>%
            group_by(Region,Reg,reportCardYear) %>%
            summarize(Index=median(Index,na.rm=TRUE))

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Calculate alt4 indices.', return=TRUE)
    ## ----end

    ## 4. Generate QAQC figure 
    ## ---- Generate QAQC figure 
    MMP_tryCatch(
    {
        ##QAQC figure
        p <- mmp__qaqc(wq.alt4.qaqc, level = 1, type = 4,
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt4_qaqc.pdf'),
            width = 159.2/25.4, height = 159.2/25.4, pointsize = 10)
        print(p)
        dev.off()

        png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt4_qaqc.png'),
            width = 180, height = 180, units = 'mm',res = 100, pointsize = 10)
        print(p)
        dev.off()

        png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt4_qaqc_large.png'),
            width = 180, height = 180, units = 'mm',res = 600, pointsize = 10)
        print(p)
        dev.off()

        MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
                               SUBSECTION_4_qaqc = structure(paste0("### QAQC\n"),
                                                             parent = 'TABSET_4'),
                               FIG_REF_4_qaqc = structure(paste0("\n::::: {#fig-4-qaqc}\n"),
                                                   parent = 'SUBSECTION_4_qaqc'),
                               FIG_4_qaqc = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt4_qaqc.png)\n"),
                                               parent = "FIG_REF_4_qaqc"),
                               FIG_CAP_4_qaqc = structure(paste0("\nObserved ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality data associated with the alt4 (formulation 1) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n"),
                                                   parent = 'FIG_REF_4_qaqc'),
                               FIG_REF_4_END = structure(paste0("\n::::: \n"),
                                                       parent = 'SUBSECTION_4_qaqc')
                               )
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt4 QAQC plot 1.', return=TRUE)
    ## ----end
    
    MMP_checkData(name = "wq.alt4.idx.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.prefix = "Processed",
                  PATH = INDICES_OUTPUT_PATH,
                  progressive = FALSE)
    MMP_openning_banner()
}
