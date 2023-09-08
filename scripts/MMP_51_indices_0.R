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

## Type 0 index (Full old, historic spatial and temporal formulation) - wq.alt1
## ---- Type 0
CURRENT_ITEM <- "Type0"
## mmp__add_status(stage = paste0("STAGE", CURRENT_STAGE),
##                 item = CURRENT_ITEM,
##                 name = "Type 0",
##                 status = "progress")
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(INDICES_OUTPUT_PATH,"wq.historic.idx.RData"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'wq.historic.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/old.wq.guidelines.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/wq.units.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/names_lookup.RData')) 
    ) {

    MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
                           SUBSECTION_0 = structure(paste0("## Type 0\n"),
                                                    parent = 'TABSET'),
                           TABSET_0 = structure(paste0("\n:::: panel-tabset\n"),
                                                   parent = 'SUBSECTION_0'),
                           TABSET_0_END = structure(paste0("\n:::: \n"),
                                                       parent = 'SUBSECTION_0')
                           )

    ## 1. Read in data
    ## ---- Read in data
    MMP_tryCatch(
    {
        load(file=paste0(NISKIN_INPUT_PATH, 'wq.historic.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'old.wq.guidelines.RData'))
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
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Reading in data for historic indices.', return=TRUE)
    ## ----end

    ## 2. Calculate indices
    ## ---- Calculate indices 
    MMP_tryCatch(
    {
        wq.historic.idx <-
            (wq.historic.qaqc2 <-
                 (wq.historic.qaqc1 <-
                      (wq.historic.qaqc <- 
                           wq.historic %>%
                           ## indicate which are the historic reefs
                           mutate(HistoricReef = MMP_HistoricReef(MMP_SITE_NAME)) %>% 
                           filter(Source %in% c('AIMS Niskin', 'AIMS FLNTU'), HistoricReef == TRUE) %>%
                           ungroup %>% #arrange(MMP_SITE_NAME,Measure,financialYear) %>%
                           left_join(old.wq.guidelines) %>%
                           ungroup() 
                      ) %>%
                      group_by(MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                               GBRMPA_water_area,Region,Reg,Subregion,Subreg,Source,
                               Measure,GL,DirectionOfFailure) %>%
                      arrange(MMP_SITE_NAME,Source,Measure,oldSamplingYear) %>%
                      filter(!is.na(MMP_SITE_NAME)) %>% 
                      summarise(Year = unique(oldSamplingYear),
                                Value = rollAv(Value, oldSamplingYear, location = 'Mean')
                                )
                 ) %>%
                 ungroup() %>%
                 mutate(Index = MMP_WQI_lastYear(.))
            ) %>%
            dplyr:::select(-Source,-GL,-DirectionOfFailure,-Value) %>%
            spread(Measure,Index) %>%
            mutate(CombinedTurb = rowMeans(cbind(NTU,TSS_MGPERL.wm), na.rm=TRUE),
                   Index = rowMeans(cbind(DRIFTCHL_UGPERL.wm,CombinedTurb,PN.wm,PP.wm,NOx.wm))) %>%
            mutate(reportCardYear = as.Date(paste0(Year,'-01-01'))) %>%
            suppressMessages() %>%
            suppressWarnings()

        wq.historic.idx.subregion <- wq.historic.idx %>%
            group_by(Subregion, Subreg, Year,reportCardYear) %>%
            summarize(Index = median(Index,na.rm = TRUE)) %>%
            suppressMessages() %>%
            suppressWarnings()

        wq.historic.idx.region <- wq.historic.idx %>%
            group_by(Region, Reg, Year, reportCardYear) %>%
            summarize(Index = median(Index, na.rm = TRUE)) %>%
            suppressMessages() %>%
            suppressWarnings()

        wq.alt1.idx.subregion.subindicator <-
            wq.historic.idx %>% 
            dplyr::select(MMP_SITE_NAME, GBRMPA_group, SHORT_NAME, Water_Samples,
                          GBRMPA_water_area, Region, Reg, Subregion, Subreg,
                          Year, reportCardYear, 
                          ##NTU, TSS_MGPERL.wm, SECCHI_DEPTH.wm,
                          CombinedTurb, DRIFTCHL_UGPERL.wm, PN.wm, PP.wm, NOx.wm) %>%
            pivot_longer(cols = c(CombinedTurb, DRIFTCHL_UGPERL.wm, PN.wm, PP.wm, NOx.wm),
                         names_to = 'Measure', values_to = 'Index') %>%
            suppressMessages() %>%
            suppressWarnings()

        save(wq.alt1.idx.subregion.subindicator,
             file = paste0(INDICES_OUTPUT_PATH, 'wq.alt1.idx.subregion.subindicator.RData'))
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Calculate historic indices.', return=TRUE)
    ## ----end

    ## 3. Save data for excel spreadsheet 
    ## ---- save data for excel spreadsheet
    MMP_tryCatch(
    {
        measure.site.year <- wq.historic.idx
        save(measure.site.year,
             file = paste0(DATA_PATH, '/final/measure.site.year.RData'))
        subindicator.subregion.year <- wq.historic.idx.subregion
        save(subindicator.subregion.year,
             file = paste0(DATA_PATH, '/final/subindicator.subregion.year.RData'))
        indicator.subregion.year <- wq.historic.idx.subregion
        save(indicator.subregion.year,
             file = paste0(DATA_PATH, '/final/indicator.subregion.year.RData'))
        indicator.region.year <- wq.historic.idx.region
        save(indicator.region.year,
             file = paste0(DATA_PATH, '/final/indicator.region.year.RData'))
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Prepare historic indices for excel spreadsheet.', return=TRUE)
    ## ----end

    ## 4. Generate calculation tables 
    ## ---- Generate calculation tables 
    MMP_tryCatch(
    {
        ## Table of current report Year values and associated indices
        ## for the appendix
        wq.historic.qaqc2 <- wq.historic.qaqc2 %>% ungroup()
        save(wq.historic.qaqc2,
             file = paste0(DATA_PATH, '/final/wq.historic.qaqc2.RData'))

        ## Table of progressive calculations for the appendix - not
        ## sure that this is used
        wq.historic.idx_calc <- wq.historic.idx %>%
            mutate(Total = rowSums(cbind(DRIFTCHL_UGPERL.wm, CombinedTurb,
                                         PN.wm, PP.wm, NOx.wm))) %>%
            dplyr:::rename(DRIFTCHL_UGPERL.idx = DRIFTCHL_UGPERL.wm, PN.idx = PN.wm,
                           PP.idx = PP.wm, NOx.idx = NOx.wm, NTU.idx = NTU,
                           SECCHI_DEPTH.idx = SECCHI_DEPTH.wm,
                           TSS_MGPERL.idx = TSS_MGPERL.wm) %>%
            left_join(wq.historic.qaqc1 %>%
                      ungroup() %>%
                      dplyr:::select(MMP_SITE_NAME,Year,Measure,Value) %>%
                      spread(key = Measure, value = Value)) %>%
            mutate(DateRange = paste0(Year-3, '-', Year)) %>%
            dplyr:::select(Region, Reef = MMP_SITE_NAME, DateRange,
                           NOx.wm, PN.wm, PP.wm, DRIFTCHL_UGPERL.wm, TSS_MGPERL.wm,
                           SECCHI_DEPTH.wm, NTU,NOx.idx, PN.idx, PP.idx,
                           DRIFTCHL_UGPERL.idx, TSS_MGPERL.idx, SECCHI_DEPTH.idx,
                           NTU.idx, CombinedTurb, Total,Scaled = Index) %>%
            suppressMessages() %>%
            suppressWarnings()

        save(wq.historic.idx_calc,
             file = paste0(DATA_PATH, '/final/wq.historic.idx_calc.RData'))
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate historic indices calculation tables.', return=TRUE)
    ## ----end

    ## 4. Generate QAQC figure 
    ## ---- Generate QAQC figure 
    MMP_tryCatch(
    {
        ##QAQC figure
        p <- mmp__qaqc(wq.historic.qaqc, level = 1, type = 1,
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_historic_qaqc.pdf'),
            width = 159.2/25.4, height = 159.2/25.4, pointsize = 10)
        print(p)
        dev.off()

        png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_historic_qaqc.png'),
            width = 180, height = 180, units = 'mm',res = 100, pointsize = 10)
        print(p)
        dev.off()

        png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_historic_qaqc_large.png'),
            width = 180, height = 180, units = 'mm',res = 600, pointsize = 10)
        print(p)
        dev.off()

        MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
                               SUBSECTION_0_qaqc = structure(paste0("### QAQC\n"),
                                                             parent = 'TABSET_0'),
                               FIG_REF_0_qaqc = structure(paste0("\n::::: {#fig-0-qaqc}\n"),
                                                   parent = 'SUBSECTION_0_qaqc'),
                               FIG_0_qaqc = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_historic_qaqc.png)\n"),
                                               parent = "FIG_REF_0_qaqc"),
                               FIG_CAP_0_qaqc = structure(paste0("\nObserved ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality data associated with the historic (formulation 0) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n"),
                                                   parent = 'FIG_REF_0_qaqc'),
                               FIG_REF_END = structure(paste0("\n::::: \n"),
                                                       parent = 'SUBSECTION_0_qaqc')
                               )
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate QAQC plot 1.', return=TRUE)
    ## ----end

    ## 5. Generate QAQC figure 2 
    ## ---- Generate QAQC figure 2 
    MMP_tryCatch(
    {
        ##QAQC figure 1
        wq.historic.qaqc1 <- wq.historic.qaqc1 %>%
            filter(Measure %in% c('DRIFTCHL_UGPERL.wm','TSS_MGPERL.wm',
                                  'SECCHI_DEPTH.wm','PP.wm','PN.wm','NOx.wm', 'NTU')) %>% 
            ungroup() %>%
            mutate(Subregion = gsub(' ','~',Subregion),
                   Subregion = factor(Subregion, levels = unique(Subregion))) %>%
            filter(Year == reportYear) %>%
            left_join(wq.units %>%
                      dplyr:::select(Measure,Name.graphs.abbr)) %>%
            left_join(wq.sites %>%
                      dplyr:::select(MMP_SITE_NAME, Latitude)) %>% 
            mutate(Season='Annual') %>%
            arrange(Latitude) %>%
            mutate(Subregion=factor(Subregion, levels=unique(Subregion)),
                   MMP_SITE_NAME=factor(MMP_SITE_NAME, levels=unique(MMP_SITE_NAME))) %>%
            suppressMessages() %>%
            suppressWarnings()

        GL <- wq.historic.qaqc1 %>%
            dplyr::select(Name.graphs.abbr,Subregion,MMP_SITE_NAME,GL) %>%
            distinct()%>%
            mutate(lower=GL/2, upper=GL*2,lower1=GL/4, upper1=GL*4)

        p <- ggplot(wq.historic.qaqc1,aes(x=Value,y=MMP_SITE_NAME)) +
            geom_blank(aes(color=Season)) +
            geom_segment(dat=GL,aes(x=lower1,xend=upper1,
                                    y=MMP_SITE_NAME, yend=MMP_SITE_NAME),
                         size=5, color='purple', alpha=0.1) +
            geom_segment(dat=GL,aes(x=lower,xend=upper,y=MMP_SITE_NAME,
                                    yend=MMP_SITE_NAME),
                         size=5, color='purple', alpha=0.3) +
            geom_text(aes(x=GL), label='I', size=5,color='purple') +
            geom_point(aes(fill=Season,color=Season),shape=21,show.legend = FALSE,size=2) +
            facet_grid(Subregion~Name.graphs.abbr, scales='free', space='free_y',
                       labeller = QAQC_labeller,as.table=FALSE) +
            scale_x_log10('Four year running means', breaks=c(0.1,0.5,1,5,10,50,100)) +
            scale_y_discrete('') +
            scale_fill_manual('Season', values=c('white','blue'),guide=FALSE) +
            scale_color_manual('Season', values=c('black','blue'),guide=FALSE) +
            theme_mmp_qaqc

        pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_historic_qaqc1.pdf'),
            width=159.2/25.4, height=159.2/25.4,pointsize=12)
        print(p)
        dev.off()

        png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_historic_qaqc1.png'),
            width=159.2, height=159.2,units='mm',res=300,pointsize=12)
        print(p)
        dev.off()

        MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
                               SUBSECTION_0_qaqc1 = structure(paste0("### QAQC 1\n"),
                                                             parent = 'TABSET_0'),
                               FIG_REF_0_qaqc1 = structure(paste0("\n::::: {#fig-0-qaqc1}\n"),
                                                   parent = 'SUBSECTION_0_qaqc1'),
                               FIG_0_qaqc1 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_historic_qaqc1.png)\n"),
                                               parent = "FIG_REF_0_qaqc1"),
                               FIG_CAP_0_qaqc1 = structure(paste0("\nSeasonal or annual ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality averages associated with the historic (formulation 0) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n"),
                                                   parent = 'FIG_REF_0_qaqc1'),
                               FIG_REF_0_qaqc1_END = structure(paste0("\n::::: \n"),
                                                       parent = 'SUBSECTION_0_qaqc1')
                               )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate QAQC plot 2.', return=TRUE)
    ## ----end

    ## 6. Generate QAQC figure 3 
    ## ---- Generate QAQC figure 3 
    MMP_tryCatch(
    {
        ##QAQC figure idx
        wq.historic.qaqc2 <- wq.historic.qaqc2 %>%
            filter(Measure %in% c('DRIFTCHL_UGPERL.wm','TSS_MGPERL.wm',
                                  'SECCHI_DEPTH.wm','PP.wm','PN.wm','NOx.wm','NTU')) %>%
            ungroup() %>%
            mutate(Subregion=gsub(' ','~',Subregion),
                   Subregion=factor(Subregion, levels=unique(Subregion))) %>%
            filter(Year == reportYear) %>%
            left_join(wq.units %>%
                      dplyr:::select(Measure,Name.graphs.abbr)) %>%
            left_join(wq.sites %>%
                      dplyr:::select(MMP_SITE_NAME, Latitude)) %>% 
            mutate(Season='Annual') %>%
            arrange(Latitude) %>%
            mutate(Subregion=factor(Subregion, levels=unique(Subregion)),
                   MMP_SITE_NAME=factor(MMP_SITE_NAME, levels=unique(MMP_SITE_NAME)))

        GL <- wq.historic.qaqc2 %>%
            dplyr:::select(Name.graphs.abbr,Subregion,MMP_SITE_NAME,GL) %>%
            distinct()%>%
            mutate(lower=GL/2, upper=GL*2,lower1=GL/4, upper1=GL*4)

        p <- ggplot(wq.historic.qaqc2,aes(x=Index,y=MMP_SITE_NAME)) +
            geom_blank(aes(color=Season)) +
            annotate(geom='rect',xmin=-1,xmax=-2/3,ymin=-Inf, ymax=Inf,
                     fill=trafficLightPalette[1],color=NA)+
            annotate(geom='rect',xmin=-2/3,xmax=-1/3,ymin=-Inf, ymax=Inf,
                     fill=trafficLightPalette[2],color=NA)+
            annotate(geom='rect',xmin=-1/3,xmax=0,ymin=-Inf, ymax=Inf,
                     fill=trafficLightPalette[3],color=NA)+
            annotate(geom='rect',xmin=0,xmax=0.5,ymin=-Inf, ymax=Inf,
                     fill=trafficLightPalette[4],color=NA)+
            annotate(geom='rect',xmin=0.5,xmax=1,ymin=-Inf, ymax=Inf,
                     fill=trafficLightPalette[5],color=NA)+
            geom_point(aes(fill=Season,color=Season),shape=21,show.legend=FALSE, size=2) +
            facet_grid(Subregion~Name.graphs.abbr, scales='free', space='free_y',
                       labeller = QAQC_labeller,as.table = FALSE) +
            scale_x_continuous('Index (Modified Amplitude)', expand=c(0.07,0)) +
            scale_y_discrete('') +
            scale_fill_manual('Season', values=c('white'), guide=FALSE) +
            scale_color_manual('Season', values=c('black'),guide=FALSE) +
            theme_mmp_qaqc

        pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_historic_qaqc2.pdf'),
            width=159.2/25.4, height=159.2/25.4, pointsize=10)
        print(p)
        dev.off()

        png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_historic_qaqc2.png'),
            width=159.2, height=159.2,units='mm', res=300, pointsize=10)
        print(p)
        dev.off()
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate QAQC plot 3.', return=TRUE)
    ## ----end

    ## 7. Generate QAQC figure 4 
    ## ---- Generate QAQC figure 4 
    MMP_tryCatch(
    {
        ## Regional Worms
        wq.historic.idx.region <- wq.historic.idx.region %>%
            ungroup() %>%
            left_join(wq.sites %>%
                      dplyr::select(Region, Latitude, Region) %>%
                      group_by(Region) %>%
                      summarize_all(funs(mean(., na.rm=TRUE)))) %>% 
            arrange(desc(Latitude)) %>%
            mutate(Region=factor(Region, levels=unique(Region))) %>%
            mutate(Grade=MMP_generateOldGrades(Index))

        wq.historic.idx.region.g1 <- ggplot(wq.historic.idx.region %>%
                                            filter(Year>2007),
                                            aes(y=Index, x=reportCardYear)) +
            geom_hline(yintercept=0, linetype='dashed') +
            geom_line() +
            geom_point(aes(fill=Grade),shape=21, size=3, show.legend = TRUE) +
            scale_y_continuous('Water Quality Index',limits=c(-1,1)) +
            scale_x_date('', limits=c(as.Date('2006-01-01'),
                                      as.Date(paste0(reportYear,'-01-01')))) +
            scale_fill_manual('',breaks=c('A','B','C','D','E'),
                              values=rev(trafficLightPalette),
                              limits=c('A','B','C','D','E'),
                              labels=c('Very Good','Good','Moderate','Poor','Very Poor')) +
            theme_mmp +
            theme(strip.background=element_blank(),
                  panel.margin.x=unit(1,'line'))

        pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_historic_idx_region.pdf'),
            width=(159.2)/25.4, height=(159.2*(2/7))/25.4)
        print(wq.historic.idx.region.g1 + facet_grid(~Region,as.table=FALSE))
        dev.off()

        pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_historic_idx_region1.pdf'),
            width=7, height=2)
        print(wq.historic.idx.region.g1 + facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'))
        dev.off()

        png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_historic_idx_region.png'),
            width=(159.2), height=(159.2*(2/7)),units='mm',res=300, pointsize=10)
        print(wq.historic.idx.region.g1 + facet_grid(~Region,as.table=FALSE))
        dev.off()

        png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_historic_idx_region1.png'),
            width=7, height=2,units='in',res=300)
        print(wq.historic.idx.region.g1 + facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'))
        dev.off()

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate QAQC plot 4.', return=TRUE)
    ## ----end

    ## 8. Generate QAQC figure 5 
    ## ---- Generate QAQC figure 5 
    MMP_tryCatch(
    {
        ## Subregion Worms
        wq.historic.idx.subregion <- wq.historic.idx.subregion %>%
            ungroup() %>%
            left_join(wq.sites %>%
                      dplyr::select(Subregion, Latitude, Subregion) %>%
                      group_by(Subregion) %>%
                      summarize_all(funs(mean(., na.rm=TRUE)))) %>% 
            arrange(desc(Latitude)) %>%
            mutate(Subregion=factor(Subregion, levels=unique(Subregion))) %>%
            mutate(Grade=MMP_generateOldGrades(Index))

        wq.historic.idx.subregion.g1 <- ggplot(wq.historic.idx.subregion %>%
                                               filter(Year>2007),
                                               aes(y=Index, x=reportCardYear)) +
            geom_hline(yintercept=0, linetype='dashed') +
            geom_line() +
            geom_point(aes(fill=Grade),shape=21, size=3, show.legend = TRUE) +
            scale_y_continuous('Water Quality Index',limits=c(-1,1)) +
            scale_x_date('', limits=c(as.Date('2006-01-01'),
                                      as.Date(paste0(reportYear,'-01-01')))) +
            scale_fill_manual('',breaks=c('A','B','C','D','E'),
                              values=rev(trafficLightPalette),
                              limits=c('A','B','C','D','E'),
                              labels=c('Very Good','Good','Moderate','Poor','Very Poor')) +
            theme_mmp +
            theme(strip.background=element_blank(),
                  panel.margin.x=unit(1,'line'))

        pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_historic_idx_subregion.pdf'),
            width=2*(159.2)/25.4, height=2*(159.2*(2/10))/25.4, pointsize=10)
        print(wq.historic.idx.subregion.g1 + facet_grid(~Subregion,as.table=FALSE))
        dev.off()

        pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_historic_idx_subregion1.pdf'),
            width=2*(159.2)/25.4, height=2*(159.2*(4/7))/25.4, pointsize=10)
        print(wq.historic.idx.subregion.g1 +
              facet_wrap(~Subregion,as.table=TRUE,nrow=2,scales='free'))                       
        dev.off()

        png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_historic_idx_subregion.png'),
            width=2*(159.2), height=2*(159.2*(2/10)), pointsize=10, units='mm', res=300)
        print(wq.historic.idx.subregion.g1 + facet_grid(~Subregion,as.table=FALSE))
        dev.off()

        png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_historic_idx_subregion1.png'),
            width=2*(159.2), height=2*(159.2*(4/7)), pointsize=10,units='mm',res=300)
        print(wq.historic.idx.subregion.g1 +
              facet_wrap(~Subregion,as.table=TRUE,nrow=2,scales='free'))                       
        dev.off()

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate QAQC plot 5.', return=TRUE)
    ## ----end

    ## 9. Save indices 
    ## ---- Save indices 
    MMP_tryCatch(
    {
        save(wq.historic.idx,
             file = paste0(INDICES_OUTPUT_PATH, 'wq.historic.idx.RData'))
        save(wq.historic.idx.region,
             file = paste0(INDICES_OUTPUT_PATH, 'wq.historic.idx.region.RData'))
        save(wq.historic.idx.subregion,
             file = paste0(INDICES_OUTPUT_PATH, 'wq.historic.idx.subregion.RData'))
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Save indices', return=TRUE)
    ## ----end

    
    MMP_checkData(name = "wq.historic.idx.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.prefix = "Processed",
                  PATH = INDICES_OUTPUT_PATH,
                  progressive = FALSE)
    MMP_openning_banner()
}

