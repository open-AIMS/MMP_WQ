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

## Type 2 index - old formulation 
## ---- Type 2
CURRENT_ITEM <- "Type2"
## mmp__add_status(stage = paste0("STAGE", CURRENT_STAGE),
##                 item = CURRENT_ITEM,
##                 name = "Type 2",
##                 status = "progress")
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(INDICES_OUTPUT_PATH,"wq.alt2.idx.RData"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'wq.all.reef.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/wq.guidelines.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/wq.units.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/names_lookup.RData')) 
    ) {

    MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
                           SUBSECTION_2 = structure(paste0("## Type 2\n"),
                                                    parent = 'TABSET'),
                           TABSET_2 = structure(paste0("\n:::: panel-tabset\n"),
                                                   parent = 'SUBSECTION_2'),
                           TABSET_2_END = structure(paste0("\n:::: \n"),
                                                       parent = 'SUBSECTION_2')
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
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Reading in data for old indices.', return=TRUE)
    ## ----end

    
    ## 2. Calculate indices
    ## ---- Calculate indices 
    MMP_tryCatch(
    {

        wq.alt2.idx <-
            (wq.alt2.qaqc2 <-
                 (wq.alt2.qaqc1 <-
                      (wq.alt2.qaqc <- wq.all.reef %>%
                           ## indicator which are the historic reefs
                           mutate(HistoricReef=MMP_HistoricReef(MMP_SITE_NAME)) %>% 
                           filter(Source %in% c('AIMS Niskin', 'AIMS FLNTU'), HistoricReef==TRUE) %>%
                           ungroup() %>% 
                           left_join(wq.guidelines %>%
                                     filter(GL.Season=='Annual') %>%
                                     dplyr:::select(GBRMPA_group,Measure,GL,DirectionOfFailure,
                                                    SHORT_NAME,Latitude,Location))
                      ) %>% 
                      group_by(MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                               GBRMPA_water_area,Region,Reg,Subregion,Subreg,Source,
                               Measure,GL,DirectionOfFailure,Location) %>%
                      arrange(MMP_SITE_NAME,Source,Measure,oldSamplingYear) %>%
                      filter(!is.na(MMP_SITE_NAME)) %>%
                      summarise(Year = unique(oldSamplingYear),
                                Value.Mean = rollAv(Value, oldSamplingYear, location = 'Mean'),
                                Value.Median = rollAv(Value, oldSamplingYear, location = 'Median')
                                ) %>%
                      ungroup %>%
                      mutate(Value=ifelse(Location=='Mean', Value.Mean,Value.Median))
                 ) %>%
                 mutate(Index=MMP_WQI_lastYear(.))
            ) %>%
            dplyr:::select(-Source,-GL,-DirectionOfFailure,-Value,-Value.Mean,
                           -Value.Median,-Location) %>%
            spread(Measure,Index) %>%
            mutate(CombinedTurb=rowMeans(cbind(NTU,TSS_MGPERL.wm), na.rm=TRUE),
                   Index=rowMeans(cbind(DRIFTCHL_UGPERL.wm,CombinedTurb,PN.wm,PP.wm,NOx.wm))) %>%
            mutate(reportCardYear=as.Date(paste0(Year,'-01-01'))) %>%
            suppressMessages() %>%
            suppressWarnings()

        wq.alt2.idx.subregion <- wq.alt2.idx %>%
            group_by(Subregion,Subreg,Year,reportCardYear) %>%
            summarize(Index=median(Index,na.rm=TRUE))

        wq.alt2.idx.region <- wq.alt2.idx %>%
            group_by(Region,Reg,Year,reportCardYear) %>%
            summarize(Index=median(Index,na.rm=TRUE))

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Calculate alt2 indices.', return=TRUE)
    ## ----end

    ## 4. Generate QAQC figure 
    ## ---- Generate QAQC figure 
    MMP_tryCatch(
    {
        ##QAQC figure
        wq.alt2.qaqc <-
            wq.alt2.qaqc %>%
            filter(Measure %in% c('DRIFTCHL_UGPERL.wm','TSS_MGPERL.wm',
                                  'SECCHI_DEPTH.wm','PP.wm','PN.wm','NOx.wm', 'NTU')) %>%
            mutate(Subregion=gsub(' ','~',Subregion),
                   Subregion=factor(Subregion, levels=unique(Subregion))) %>%
            filter(waterYear == reportYear) %>%
            left_join(wq.units %>%
                      dplyr:::select(Measure,Name.graphs.abbr)) %>%
            left_join(wq.sites %>%
                      dplyr:::select(MMP_SITE_NAME, Latitude)) %>% 
            arrange(Latitude) %>%
            mutate(Subregion=factor(Subregion, levels=unique(Subregion)),
                   MMP_SITE_NAME=factor(MMP_SITE_NAME, levels=unique(MMP_SITE_NAME))) %>%
            suppressMessages() %>%
            suppressWarnings()

        GL <- wq.alt2.qaqc %>%
            dplyr::select(Name.graphs.abbr,Subregion,MMP_SITE_NAME,GL) %>%
            distinct() %>%
            mutate(lower=GL/2, upper=GL*2,lower1=GL/4, upper1=GL*4)

        p <- ggplot(wq.alt2.qaqc, aes(x=Value,y=MMP_SITE_NAME)) +
            geom_blank(aes(color=Season)) +
            geom_segment(dat=GL, aes(x=lower1,xend=upper1,
                                     y=MMP_SITE_NAME, yend=MMP_SITE_NAME),
                         size=5, color='purple', alpha=0.1) +
            geom_segment(dat=GL, aes(x=lower,xend=upper,
                                     y=MMP_SITE_NAME, yend=MMP_SITE_NAME),
                         size=5, color='purple', alpha=0.3) +
            geom_text(aes(x=GL), label='I', size=5,color='purple') +
            geom_point(data=wq.historic.qaqc %>%
                           filter(Source=='AIMS Niskin') %>%
                           droplevels(),
                       aes(color=Season, shape=Source, size=Source),
                       position=position_jitter(height=0.0, width=0),
                       show.legend=FALSE) +
            geom_point(data = wq.historic.qaqc %>%
                           filter(Source=='AIMS FLNTU') %>%
                           droplevels(),
                       aes(color=Season,shape=Source,size=Source),
                       position=position_jitter(height=0.4, width=0),
                       show.legend=FALSE) +
            facet_grid(Subregion~Name.graphs.abbr, scales='free', space='free_y',
                       labeller = QAQC_labeller,as.table=FALSE) +
            scale_x_log10('Depth weighted averages',breaks=c(0.1,0.5,1,5,10,50,100)) +
            scale_y_discrete('') +
            scale_fill_manual('Season', values=c('red','blue')) +
            scale_color_manual('Season', values=c('red','blue')) +
            scale_shape_manual('Source', values=c(16,16)) +
            scale_size_manual('Source', values=c(0.1,1)) +
            theme_mmp_qaqc 

        pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt2_qaqc.pdf'),
            width = 159.2/25.4, height = 159.2/25.4, pointsize = 10)
        print(p)
        dev.off()

        png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt2_qaqc.png'),
            width = 159.2, height = 159.2, units = 'mm',res = 300, pointsize = 10)
        print(p)
        dev.off()

        MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
                               SUBSECTION_2_qaqc = structure(paste0("### QAQC\n"),
                                                             parent = 'TABSET_2'),
                               FIG_REF_2_qaqc = structure(paste0("\n::::: {#fig-2-qaqc}\n"),
                                                   parent = 'SUBSECTION_2_qaqc'),
                               FIG_2_qaqc = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt2_qaqc.png)\n"),
                                               parent = "FIG_REF_2_qaqc"),
                               FIG_CAP_2_qaqc = structure(paste0("\nObserved ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality data associated with the alt2 (formulation 1) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n"),
                                                   parent = 'FIG_REF_2_qaqc'),
                               FIG_REF_2_END = structure(paste0("\n::::: \n"),
                                                       parent = 'SUBSECTION_2_qaqc')
                               )
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt2 QAQC plot 1.', return=TRUE)
    ## ----end
    
    MMP_checkData(name = "wq.alt2.idx.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.prefix = "Processed",
                  PATH = INDICES_OUTPUT_PATH,
                  progressive = FALSE)
    MMP_openning_banner()
}
