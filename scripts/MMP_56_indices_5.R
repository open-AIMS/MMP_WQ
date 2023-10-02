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

## Type 5 index  
## ---- Type 5
CURRENT_ITEM <- "Type5"
## mmp__add_status(stage = paste0("STAGE", CURRENT_STAGE),
##                 item = CURRENT_ITEM,
##                 name = "Type 5",
##                 status = "progress")
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(INDICES_OUTPUT_PATH,"wq.alt5.idx.RData"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'wq.all.reef.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/wq.guidelines.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/wq.units.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/names_lookup.RData')) 
    ) {

    MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
                           SUBSECTION_5 = structure(paste0("## Type 5\n"),
                                                    parent = 'TABSET'),
                           TABSET_5 = structure(paste0("\n:::: panel-tabset\n"),
                                                   parent = 'SUBSECTION_5'),
                           TABSET_5_END = structure(paste0("\n:::: \n"),
                                                       parent = 'SUBSECTION_5')
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
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Reading in data for alt5 indices.', return=TRUE)
    ## ----end

    
    ## 2. Calculate indices
    ## ---- Calculate indices 
    MMP_tryCatch(
    {
        wq.alt5.idx <-
            (wq.alt5.qaqc2 <-
                 (wq.alt5.qaqc1 <-
                      (wq.alt5.qaqc <-
                           wq.all.reef %>%
                           filter(!is.na(MMP_SITE_NAME), reneeYear>2015) %>% ungroup %>%
                           ## Barbara wants the following added back in
                           ## Previously Renee wanted them excluded
                           ## filter(!(Source=='JCU Niskin' & Measure %in% c('PP.wm','PN.wm')),
                           ##        !(Source=='CY Niskin' & Measure %in% c('PP.wm','PN.wm'))) %>%
                           ## droplevels %>%
                           left_join(wq.guidelines %>%
                                     dplyr::select(GBRMPA_group,Measure,GL.Season,Location,
                                                   GL,DirectionOfFailure,SHORT_NAME,Latitude)) %>%
                           ungroup() %>%
                           group_by(MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                                    GBRMPA_water_area,Region,Reg,Subregion,Subreg,
                                    ## waterYear,
                                    reneeYear,
                                    ## cwaterYear,
                                    Measure,Season) %>%
                           do({  # filter out either the Annual or Wet/Dry
                               x=.
                               if((all(is.na(x$GL[x$GL.Season=='Dry']))) |
                                  (all(is.na(x$GL[x$GL.Season=='Wet']))))
                                   x %>% filter(GL.Season=='Annual')
                               else x %>% filter(as.character(x$GL.Season) ==
                                                 unique(as.character(x$Season)))
                           })
                      ) %>%
                      group_by(MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                               GBRMPA_water_area,Region,Reg,Subregion,Subreg,
                               ## waterYear,
                               reneeYear,
                               ## cwaterYear,
                               Measure,Location,GL.Season) %>%
                      do({  #Aggregate over 
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
                 ungroup %>% mutate(Index=MMP_WQI_lastYear(.)) %>%
                 group_by(MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                          GBRMPA_water_area,Region,Reg,Subregion,Subreg,
                          ## waterYear,
                          reneeYear,
                          ## cwaterYear,
                          Measure) %>%
                 summarize(Index=mean(Index,na.rm=TRUE))
            ) %>%
            ungroup() %>% 
            spread(Measure,Index) %>%
            mutate(CombinedTurb=rowMeans(cbind(NTU,SECCHI_DEPTH.wm,TSS_MGPERL.wm), na.rm=TRUE),
                   Index=rowMeans(cbind(DRIFTCHL_UGPERL.wm,CombinedTurb,
                                        PN.wm,PP.wm,NOx.wm), na.rm=TRUE)) %>%
            mutate(reportCardYear=as.Date(paste0(reneeYear,'-01-01'))) %>%
            suppressMessages() %>%
            suppressWarnings()

        save(wq.alt5.idx,
             file = paste0(DATA_PATH, "/indices/wq.alt5.idx.RData"))
        wq.alt5.idx.subregion <- wq.alt5.idx %>%
            group_by(Subregion,Subreg,reportCardYear) %>%
            summarize(Index=median(Index,na.rm=TRUE))

        wq.alt5.idx.region <- wq.alt5.idx %>%
            group_by(Region,Reg,reportCardYear) %>%
            summarize(Index=median(Index,na.rm=TRUE))

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Calculate alt5 indices.', return=TRUE)
    ## ----end

    ## 5. Generate QAQC figure 
    ## 2021 Note, the following does not use reneeYear as it and cwaterYear are not consistent...
    ## ---- Generate QAQC figure 
    MMP_tryCatch(
    {
        ##QAQC figure
        p <- mmp__qaqc(wq.alt5.qaqc, level = 1, type = '5',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt5_qaqc",
                                    Plot = p, units = "in",
                                    fig.width = 180/25.4, fig.height = 180/25.4, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "QAQC", fig_name_suffix = "wq_alt5_qaqc",
                           label_suffix = "_5_qaqc", tabset_parent = "TABSET_5",
                           fig.caption = paste0("\nObserved ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality data associated with the alt5 (formulation 5) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n")) 

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_qaqc.pdf'),
        ##     width = 159.2/25.4, height = 159.2/25.4, pointsize = 10)
        ## print(p)
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_qaqc.png'),
        ##     width = 180, height = 180, units = 'mm',res = 100, pointsize = 10)
        ## print(p)
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_qaqc_large.png'),
        ##     width = 180, height = 180, units = 'mm',res = 600, pointsize = 10)
        ## print(p)
        ## dev.off()

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_5_qaqc = structure(paste0("### QAQC\n"),
        ##                                                      parent = 'TABSET_5'),
        ##                        FIG_REF_5_qaqc = structure(paste0("\n::::: {#fig-5-qaqc}\n"),
        ##                                            parent = 'SUBSECTION_5_qaqc'),
        ##                        FIG_5_qaqc = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt5_qaqc.png)\n"),
        ##                                        parent = "FIG_REF_5_qaqc"),
        ##                        FIG_CAP_5_qaqc = structure(paste0("\nObserved ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality data associated with the alt5 (formulation 5) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n"),
        ##                                            parent = 'FIG_REF_5_qaqc'),
        ##                        FIG_REF_5_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_5_qaqc')
        ##                        )
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt5 QAQC plot 1.', return=TRUE)
    ## ----end
    
    ## 5. Generate QAQC figure 2 
    ## ---- Generate QAQC figure 2 
    MMP_tryCatch(
    {
        ##QAQC figure 1
        p <- mmp__qaqc(wq.alt5.qaqc1, level = 2, type = '5',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt5_qaqc1",
                                    Plot = p, units = "in",
                                    fig.width = 180/25.4, fig.height = 180/25.4, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "QAQC 1", fig_name_suffix = "wq_alt5_qaqc1",
                           label_suffix = "_5_qaqc1", tabset_parent = "TABSET_5",
                           fig.caption = paste0("\nSeasonal or annual ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality averages associated with the alt5 (formulation 5) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n")) 

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_qaqc1.pdf'),
        ##     width=159.2/25.4, height=159.2/25.4,pointsize=10)
        ## print(p)
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_qaqc1.png'),
        ##     width=180, height=180,units='mm',res=100,pointsize=12)
        ## print(p)
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_qaqc1_large.png'),
        ##     width=180, height=180,units='mm',res=600,pointsize=12)
        ## print(p)
        ## dev.off()

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_5_qaqc1 = structure(paste0("### QAQC 1\n"),
        ##                                                      parent = 'TABSET_5'),
        ##                        FIG_REF_5_qaqc1 = structure(paste0("\n::::: {#fig-5-qaqc1}\n"),
        ##                                            parent = 'SUBSECTION_5_qaqc1'),
        ##                        FIG_5_qaqc1 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt5_qaqc1.png)\n"),
        ##                                        parent = "FIG_REF_5_qaqc1"),
        ##                        FIG_CAP_5_qaqc1 = structure(paste0("\nSeasonal or annual ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality averages associated with the alt5 (formulation 5) indices. Red and blue symbols represent dry and wet season annual averages. Red and purple band represent half and twice dry and wet annual guideline values respectively.\n"),
        ##                                            parent = 'FIG_REF_5_qaqc1'),
        ##                        FIG_REF_5_qaqc1_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_5_qaqc1')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt5 QAQC plot 2.', return=TRUE)
    ## ----end

    ## 6. Generate QAQC figure 3 
    ## ---- Generate QAQC figure 3 
    MMP_tryCatch(
    {
        ##QAQC figure idx
        p <- mmp__qaqc(wq.alt5.qaqc2, level = 3, type = '5',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt5_qaqc2",
                                    Plot = p, units = "in",
                                    fig.width = 180/25.4, fig.height = 180/25.4, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "QAQC 2", fig_name_suffix = "wq_alt5_qaqc2",
                           label_suffix = "_5_qaqc2", tabset_parent = "TABSET_5",
                           fig.caption = paste0("\nSeasonal or annual ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality averages associated with the alt5 (formulation 5) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n")) 

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_qaqc2.pdf'),
        ##     width=159.2/25.4, height=159.2/25.4, pointsize=10)
        ## print(p)
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_qaqc2.png'),
        ##     width=180, height=180,units='mm', res=100, pointsize=10)
        ## print(p)
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_qaqc2_large.png'),
        ##     width=180, height=180,units='mm', res=600, pointsize=10)
        ## print(p)
        ## dev.off()
        
        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_5_qaqc2 = structure(paste0("### QAQC 2\n"),
        ##                                                      parent = 'TABSET_5'),
        ##                        FIG_REF_5_qaqc2 = structure(paste0("\n::::: {#fig-5-qaqc2}\n"),
        ##                                            parent = 'SUBSECTION_5_qaqc2'),
        ##                        FIG_5_qaqc2 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt5_qaqc2.png)\n"),
        ##                                        parent = "FIG_REF_5_qaqc2"),
        ##                        FIG_CAP_5_qaqc2 = structure(paste0("\nSeasonal or annual ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality averages associated with the alt5 (formulation 5) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n"),
        ##                                            parent = 'FIG_REF_5_qaqc2'),
        ##                        FIG_REF_5_qaqc2_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_5_qaqc2')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt5 QAQC plot 3.', return=TRUE)
    ## ----end

    ## 7. Generate QAQC figure 4 
    ## ---- Generate QAQC figure 4 
    MMP_tryCatch(
    {
        ## Regional Worms
        p <- mmp__indicator_trends(wq.alt5.idx.region, level = 1, type = '5',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt5_idx_region",
                               Plot = p + facet_grid(~Region,as.table=FALSE),
                               units = "in",
                               fig.width = 180/25.4, fig.height = 180*(2/7)/25.4, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional Index", fig_name_suffix = "wq_alt5_idx_region",
                           label_suffix = "_5_idx1", tabset_parent = "TABSET_5",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt5 (formulation 5) indices.\n")) 

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt5_idx_region1",
                               Plot = p + facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'),
                               units = "in",
                               fig.width = 8, fig.height = 2, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional Index alt.", fig_name_suffix = "wq_alt5_idx_region1",
                           label_suffix = "_5_idx1a", tabset_parent = "TABSET_5",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt5 (formulation 5) indices.\n")) 

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_idx_region.pdf'),
        ##     width=(159.2)/25.4, height=(159.2*(2/7))/25.4)
        ## print(p + facet_grid(~Region,as.table=FALSE))
        ## dev.off()

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_idx_region1.pdf'),
        ##     width=7, height=2)
        ## print(p + facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'))
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_idx_region.png'),
        ##     width=(159.2), height=(159.2*(2/7)),units='mm',res=300, pointsize=10)
        ## print(p + facet_grid(~Region,as.table=FALSE))
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_idx_region_large.png'),
        ##     width=(159.2), height=(159.2*(2/7)),units='mm',res=600, pointsize=10)
        ## print(p + facet_grid(~Region,as.table=FALSE))
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_idx_region1.png'),
        ##     width=7, height=2,units='in',res=300)
        ## print(p + facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'))
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_idx_region1_large.png'),
        ##     width=7, height=2,units='in',res=600)
        ## print(p + facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'))
        ## dev.off()

        wq.alt5.idx.region <- p$data
        save(wq.alt5.idx.region,
             file = paste0(DATA_PATH, '/final/wq.alt5.idx.region.RData'))

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_5_idx1 = structure(paste0("### Regional Index\n"),
        ##                                                      parent = 'TABSET_5'),
        ##                        FIG_REF_5_idx1 = structure(paste0("\n::::: {#fig-5-idx1}\n"),
        ##                                            parent = 'SUBSECTION_5_idx1'),
        ##                        FIG_5_qaqc1 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt5_idx_region.png)\n"),
        ##                                        parent = "FIG_REF_5_idx1"),
        ##                        FIG_CAP_5_idx1 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt5 (formulation 5) indices.\n"),
        ##                                            parent = 'FIG_REF_5_idx1'),
        ##                        FIG_REF_5_idx1_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_5_idx1'),

        ##                        FIG_REF_5_idx1b = structure(paste0("\n::::: {#fig-5-idx1b}\n"),
        ##                                            parent = 'SUBSECTION_5_idx1'),
        ##                        FIG_5_qaqc1b = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt5_idx_region1.png)\n"),
        ##                                        parent = "FIG_REF_5_idx1b"),
        ##                        FIG_CAP_5_idx1b = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt5 (formulation 0) indices.\n"),
        ##                                            parent = 'FIG_REF_5_idx1b'),
        ##                        FIG_REF_5_idx1b_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_5_idx1')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt4 QAQC plot 4.', return=TRUE)
    ## ----end

    ## 8. Generate QAQC figure 5 
    ## ---- Generate QAQC figure 5 
    MMP_tryCatch(
    {
        ## Regional Worms
        p <- mmp__indicator_trends(wq.alt5.idx.subregion, level = 2, type = '5',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt5_idx_subregion",
                               Plot = p + facet_grid(~Subregion,as.table=FALSE),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 2*180*(2/10)/25.4, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregion Index", fig_name_suffix = "wq_alt5_idx_subregion",
                           label_suffix = "_5_idx2", tabset_parent = "TABSET_5",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Subregion for the alt5 (formulation 5) indices.\n")) 

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt5_idx_subregion1",
                               Plot = p + facet_wrap(~Subregion,as.table=TRUE,nrow=2,scales='free'),
                               units = "in",
                               fig.width = 2*(180)/25.4, fig.height = 2*(159.2*(4/7))/25.4,
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional Index alt.", fig_name_suffix = "wq_alt5_idx_subregion1",
                           label_suffix = "_5_idx2a", tabset_parent = "TABSET_5",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Subregion for the alt5 (formulation 5) indices.\n")) 

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_idx_subregion.pdf'),
        ##     width=(159.2)/25.4, height=(159.2*(2/7))/25.4)
        ## print(p + facet_grid(~Subregion,as.table=FALSE))
        ## dev.off()

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_idx_subregion1.pdf'),
        ##     width=7, height=2)
        ## print(p + facet_wrap(~Subregion,as.table=FALSE,nrow=1,scales='free_y'))
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_idx_subregion.png'),
        ##     width=(159.2), height=(159.2*(2/7)),units='mm',res=300, pointsize=10)
        ## print(p + facet_grid(~Subregion,as.table=FALSE))
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_idx_subregion_large.png'),
        ##     width=(159.2), height=(159.2*(2/7)),units='mm',res=600, pointsize=10)
        ## print(p + facet_grid(~Subregion,as.table=FALSE))
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_idx_subregion1.png'),
        ##     width=7, height=2,units='in',res=300)
        ## print(p + facet_wrap(~Subregion,as.table=FALSE,nrow=1,scales='free_y'))
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt5_idx_subregion1_large.png'),
        ##     width=7, height=2,units='in',res=600)
        ## print(p + facet_wrap(~Subregion,as.table=FALSE,nrow=1,scales='free_y'))
        ## dev.off()

        wq.alt5.idx.subregion <- p$data
        save(wq.alt5.idx.subregion,
             file = paste0(DATA_PATH, '/final/wq.alt5.idx.subregion.RData'))

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_5_idx2 = structure(paste0("### Subregional Index\n"),
        ##                                                      parent = 'TABSET_5'),
        ##                        FIG_REF_5_idx2 = structure(paste0("\n::::: {#fig-5-idx2}\n"),
        ##                                            parent = 'SUBSECTION_5_idx2'),
        ##                        FIG_5_qaqc2 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt5_idx_subregion.png)\n"),
        ##                                        parent = "FIG_REF_5_idx2"),
        ##                        FIG_CAP_5_idx1 = structure(paste0("\nTemporal trends in the water quality index conditional on Subregion for the alt5 (formulation 5) indices.\n"),
        ##                                            parent = 'FIG_REF_5_idx2'),
        ##                        FIG_REF_5_idx2_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_5_idx2'),

        ##                        FIG_REF_5_idx2b = structure(paste0("\n::::: {#fig-5-idx2b}\n"),
        ##                                            parent = 'SUBSECTION_5_idx2'),
        ##                        FIG_5_qaqc2b = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt5_idx_subregion1.png)\n"),
        ##                                        parent = "FIG_REF_5_idx2b"),
        ##                        FIG_CAP_5_idx1b = structure(paste0("\nTemporal trends in the water quality index conditional on Subregion for the alt5 (formulation 5) indices.\n"),
        ##                                            parent = 'FIG_REF_5_idx2b'),
        ##                        FIG_REF_5_idx2b_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_5_idx2')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt4 QAQC plot 4.', return=TRUE)
    ## ----end
    
    MMP_checkData(name = "wq.alt5.idx.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.prefix = "Processed",
                  PATH = INDICES_OUTPUT_PATH,
                  progressive = FALSE)
    MMP_openning_banner()
}
