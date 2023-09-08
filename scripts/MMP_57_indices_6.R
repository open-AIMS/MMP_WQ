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

## Type 6 index  
## ---- Type 6
CURRENT_ITEM <- "Type6"
## mmp__add_status(stage = paste0("STAGE", CURRENT_STAGE),
##                 item = CURRENT_ITEM,
##                 name = "Type 6",
##                 status = "progress")
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(INDICES_OUTPUT_PATH,"wq.alt6.idx.RData"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'wq.all.reef.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/wq.guidelines.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/wq.units.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/hierarchy.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/names_lookup.RData')) 
    ) {

    MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
                           SUBSECTION_6 = structure(paste0("## Type 6\n"),
                                                    parent = 'TABSET'),
                           TABSET_6 = structure(paste0("\n:::: panel-tabset\n"),
                                                   parent = 'SUBSECTION_6'),
                           TABSET_6_END = structure(paste0("\n:::: \n"),
                                                       parent = 'SUBSECTION_6')
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
        hier <- get(load(file=paste0(PARAMS_INPUT_PATH, 'hierarchy.RData')))
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
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Reading in data for alt6 indices.', return=TRUE)
    ## ----end

    
    ## 2. Calculate indices
    ## ---- Calculate indices 
    MMP_tryCatch(
    {
        wq.alt6.idx <-
            (wq.alt6.qaqc1 <-
                 (wq.alt6.qaqc <-                               # Retain compiled data for QAQC 1
                      wq.all.reef %>%                           # take the data
                      ungroup() %>%                             # ungroup it 
                      left_join(hier) %>%                       # add the hierarchical info
                      filter(!is.na(Indicator)) %>%             # exclude Measures that are not part of the hierarchy
                      filter(!is.na(MMP_SITE_NAME),             # exclude those with missing reef names
                             reneeYear>2015) %>%                # and records early than 2015
                      filter(!(Source=='JCU Niskin' &
                               Measure %in% c('PP.wm','PN.wm') &
                               reneeYear<2021),                 # exclude JCU PP and PN data for years before 2021
                             !(Source=='CY Niskin' &
                               Measure %in% c('PP.wm','PN.wm') &
                               reneeYear<2021)) %>%             # exclude CY PP and PN data for years before 2021
                  droplevels() %>%     
                  left_join(wq.guidelines %>%                   # add the guideline data
                            dplyr::select(GBRMPA_group,Measure,GL.Season,Location,GL,
                                          DirectionOfFailure,SHORT_NAME,Latitude)) %>%
                  group_by(MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                           GBRMPA_water_area,
                           Region,Reg,Subregion,Subreg,waterYear,reneeYear,
                           cwaterYear,Indicator,Subindicator,Measure,Season) %>%
                  do({                                          # filter out either the Annual or Wet/Dry
                      x=.
                      ## if((is.na(x$GL[x$GL.Season=='Dry'])) | (is.na(x$GL[x$GL.Season=='Wet']))) x %>% filter(GL.Season=='Annual')
                      if(any(is.na(x$GL[x$GL.Season=='Dry'])) | any(is.na(x$GL[x$GL.Season=='Wet']))) x %>% filter(GL.Season=='Annual')
                      else x %>% filter(as.character(x$GL.Season) == unique(as.character(x$Season)))
                  })
                 ) %>%
                 group_by(MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,GBRMPA_water_area,
                          Region,Reg,Subregion,Subreg,waterYear,reneeYear,cwaterYear,
                          Indicator,Subindicator,Measure,Location,GL.Season) %>%
                 do({                                           # Aggregate over samples in a season.  GL must be applied to medians or means etc 
                     dat=.
                     data.frame(GL=unique(dat$GL),
                                DirectionOfFailure=unique(dat$DirectionOfFailure),
                                Latitude=mean(dat$Latitude,na.rm=TRUE),
                                Value=ifelse(unique(dat$Location)=='Median',
                                             median(dat$Value, na.rm=TRUE),
                                             mean(dat$Value, na.rm=TRUE)),
                                Value.Mean=mean(dat$Value, na.rm=TRUE),
                                Value.Median=median(dat$Value, na.rm=TRUE))
                 })
            ) %>%
            ungroup() %>%
            mutate(Index=MMP_WQI_lastYear(.),                   #  calculate WQIs
                   Score=RC_index(x=Value,GL=GL,fold=2,DOF=DirectionOfFailure,type='MAMP',
                                  capped=TRUE,scaled=TRUE))

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Calculate alt6 indices.', return=TRUE)
    ## ----end

    ## 6. Generate QAQC figure 
    ## ---- Generate QAQC figure 
    MMP_tryCatch(
    {
        ##QAQC figure
        p <- mmp__qaqc(wq.alt6.qaqc, level = 1, type = 6,
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_qaqc.pdf'),
            width = 159.2/25.4, height = 159.2/25.4, pointsize = 10)
        print(p)
        dev.off()

        png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_qaqc.png'),
            width = 180, height = 180, units = 'mm',res = 100, pointsize = 10)
        print(p)
        dev.off()

        png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_qaqc_large.png'),
            width = 180, height = 180, units = 'mm',res = 600, pointsize = 10)
        print(p)
        dev.off()

        MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
                               SUBSECTION_6_qaqc = structure(paste0("### QAQC\n"),
                                                             parent = 'TABSET_6'),
                               FIG_REF_6_qaqc = structure(paste0("\n::::: {#fig-6-qaqc}\n"),
                                                   parent = 'SUBSECTION_6_qaqc'),
                               FIG_6_qaqc = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_qaqc.png)\n"),
                                               parent = "FIG_REF_6_qaqc"),
                               FIG_CAP_6_qaqc = structure(paste0("\nObserved ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality data associated with the alt6 (formulation 1) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n"),
                                                   parent = 'FIG_REF_6_qaqc'),
                               FIG_REF_6_END = structure(paste0("\n::::: \n"),
                                                       parent = 'SUBSECTION_6_qaqc')
                               )
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt6 QAQC plot 1.', return=TRUE)
    ## ----end
    
    MMP_checkData(name = "wq.alt6.idx.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.prefix = "Processed",
                  PATH = INDICES_OUTPUT_PATH,
                  progressive = FALSE)
    MMP_openning_banner()
}
