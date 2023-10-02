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

        ## 2019 - Renee wants me to remove Enclosed coastal sites
        wq.alt6.idx <- wq.alt6.idx %>%
            filter(GBRMPA_water_area %in% c('Open Coastal waters',
                                            'Midshelf waters','Offshore waters')) %>%
            droplevels()


    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Calculate alt6 indices.', return=TRUE)
    ## ----end

    ## 6. Generate QAQC figure 
    ## ---- Generate QAQC figure 
    MMP_tryCatch(
    {
        ##QAQC figure
        p <- mmp__qaqc(wq.alt6.qaqc, level = 1, type = '6',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_qaqc",
                                    Plot = p, units = "in",
                                    fig.width = 180/25.4, fig.height = 180/25.4, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "QAQC", fig_name_suffix = "wq_alt6_qaqc",
                           label_suffix = "_6_qaqc", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nObserved ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality data associated with the alt6 (formulation 6) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n")) 

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_qaqc.pdf'),
        ##     width = 159.2/25.4, height = 159.2/25.4, pointsize = 10)
        ## print(p)
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_qaqc.png'),
        ##     width = 180, height = 180, units = 'mm',res = 100, pointsize = 10)
        ## print(p)
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_qaqc_large.png'),
        ##     width = 180, height = 180, units = 'mm',res = 600, pointsize = 10)
        ## print(p)
        ## dev.off()

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_6_qaqc = structure(paste0("### QAQC\n"),
        ##                                                      parent = 'TABSET_6'),
        ##                        FIG_REF_6_qaqc = structure(paste0("\n::::: {#fig-6-qaqc}\n"),
        ##                                            parent = 'SUBSECTION_6_qaqc'),
        ##                        FIG_6_qaqc = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_qaqc.png)\n"),
        ##                                        parent = "FIG_REF_6_qaqc"),
        ##                        FIG_CAP_6_qaqc = structure(paste0("\nObserved ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality data associated with the alt6 (formulation 6) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n"),
        ##                                            parent = 'FIG_REF_6_qaqc'),
        ##                        FIG_REF_6_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_qaqc')
        ##                        )
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt6 QAQC plot 1.', return=TRUE)
    ## ----end

    ## 5. Generate QAQC figure 2 
    ## ---- Generate QAQC figure 2 
    MMP_tryCatch(
    {
        ##QAQC figure 1
        p <- mmp__qaqc(wq.alt6.qaqc1, level = 2, type = '6',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_qaqc1",
                                    Plot = p, units = "in",
                                    fig.width = 180/25.4, fig.height = 180/25.4, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "QAQC 1", fig_name_suffix = "wq_alt6_qaqc1",
                           label_suffix = "_6_qaqc1", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nSeasonal or annual ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality averages associated with the alt6 (formulation 6) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n")) 

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_qaqc1.pdf'),
        ##     width=159.2/25.4, height=159.2/25.4,pointsize=10)
        ## print(p)
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_qaqc1.png'),
        ##     width=180, height=180,units='mm',res=100,pointsize=12)
        ## print(p)
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_qaqc1_large.png'),
        ##     width=180, height=180,units='mm',res=600,pointsize=12)
        ## print(p)
        ## dev.off()

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_6_qaqc1 = structure(paste0("### QAQC 6\n"),
        ##                                                      parent = 'TABSET_6'),
        ##                        FIG_REF_6_qaqc1 = structure(paste0("\n::::: {#fig-6-qaqc1}\n"),
        ##                                            parent = 'SUBSECTION_6_qaqc1'),
        ##                        FIG_6_qaqc1 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_qaqc1.png)\n"),
        ##                                        parent = "FIG_REF_6_qaqc1"),
        ##                        FIG_CAP_6_qaqc1 = structure(paste0("\nSeasonal or annual ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality averages associated with the alt6 (formulation 6) indices. Red and blue symbols represent dry and wet season annual averages. Red and purple band represent half and twice dry and wet annual guideline values respectively.\n"),
        ##                                            parent = 'FIG_REF_5_qaqc1'),
        ##                        FIG_REF_6_qaqc1_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_qaqc1')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt5 QAQC plot 2.', return=TRUE)
    ## ----end

    ## 6. Generate QAQC figure 3 
    ## ---- Generate QAQC figure 3 
    MMP_tryCatch(
    {

        ## Bootstrapping works best when the Scores are calculated on the individual observations.
        ## If bootstrapping begins with already aggregated data, then some of the uncertainty has
        ## already been lost.  In the case of MMP water quality metrics, we have been clearly directed
        ## that the Guideline values MUST be applied on specific aggregations (either annual or seasonally).
        ## Consequently, it is only at the level of aggregated over sites or measures that we can begin to
        ## propagate any uncertainty. This means that we necessarily cannot capture the uncertainty at duplicate,
        ## depth or even samples through the year level.  The only sources of uncertainty that we can capture
        ## are from site->region and measure->indicator levels.
        
        ## waterYear.site.measure.season level - this step can be bootstrapped
        reneeYear.site.measure.season <- wq.alt6.idx %>%
            mutate(Weight = 1) %>%
            dplyr::select(reneeYear, Region, Subregion, MMP_SITE_NAME,
                          GL.Season, Indicator, Subindicator, Measure, Weight, Score) %>%
            RC_aggregate(grouping_cols = c('reneeYear', 'Region', 'Subregion',
                                         'MMP_SITE_NAME', 'GL.Season', 'Indicator',
                                         'Subindicator', 'Measure'),
                         gradetype = 'MMP') %>%
            mutate(Grade = MMP_generateOldGrades(Score))
        
        ## reneeYear.site.measure level (aggregate over season)
        ## for the bootstrapping, this must start with the raw indexed data
        reneeYear.site.measure <- reneeYear.site.measure.season %>%
            mutate(Weight = 1) %>%
            RC_aggregate(grouping_cols = c('reneeYear','Region','Subregion',
                                           'MMP_SITE_NAME','Indicator','Subindicator',
                                           'Measure'),
                         gradetype = 'MMP') %>%
            mutate(Grade = MMP_generateOldGrades(Score))
        
        
        ##QAQC figure idx
        p <- mmp__qaqc(reneeYear.site.measure, level = 3, type = '6',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_qaqc2",
                                    Plot = p, units = "in",
                                    fig.width = 180/25.4, fig.height = 180/25.4, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "QAQC 2", fig_name_suffix = "wq_alt6_qaqc2",
                           label_suffix = "_6_qaqc2", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nSeasonal or annual ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality averages associated with the alt6 (formulation 6) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n")) 

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_qaqc2.pdf'),
        ##     width=159.2/25.4, height=159.2/25.4, pointsize=10)
        ## print(p)
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_qaqc2.png'),
        ##     width=180, height=180,units='mm', res=100, pointsize=10)
        ## print(p)
        ## dev.off()

        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_qaqc2_large.png'),
        ##     width=180, height=180,units='mm', res=600, pointsize=10)
        ## print(p)
        ## dev.off()
        
        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_6_qaqc2 = structure(paste0("### QAQC 2\n"),
        ##                                                      parent = 'TABSET_6'),
        ##                        FIG_REF_6_qaqc2 = structure(paste0("\n::::: {#fig-6-qaqc2}\n"),
        ##                                            parent = 'SUBSECTION_6_qaqc2'),
        ##                        FIG_6_qaqc2 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_qaqc2.png)\n"),
        ##                                        parent = "FIG_REF_6_qaqc2"),
        ##                        FIG_CAP_6_qaqc2 = structure(paste0("\nSeasonal or annual ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality averages associated with the alt6 (formulation 6) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n"),
        ##                                            parent = 'FIG_REF_6_qaqc2'),
        ##                        FIG_REF_6_qaqc2_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_qaqc2')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt6 QAQC plot 3.', return=TRUE)
    ## ----end

    ## 7. Generate QAQC figure 4 
    ## ---- Generate QAQC figure 4 
    MMP_tryCatch(
    {
        ## Regional Worms
        p <- mmp__indicator_trends(reneeYear.site.measure, level = 1, type = '6',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_idx_site_measure",
                               Plot = p  + facet_grid(Subindicator + Measure ~ Subregion,
                                                      as.table = TRUE,
                                                      labeller = labeller(Subregion =
                                                                              label_wrap_gen(10))),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 4*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Site/Measure Index", fig_name_suffix = "wq_alt6_idx_site_measure",
                           label_suffix = "_6_idx1", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n")) 

        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_site_measure.pdf'),
        ##        p + facet_grid(Subindicator + Measure ~ Subregion,
        ##                       as.table = TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))),
        ##        width = 2*(159.2)/25.4,
        ##        height = 4*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_site_measure.png'),
        ##        p + facet_grid(Subindicator + Measure ~ Subregion,
        ##                       as.table = TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))),
        ##        width = 2*(159.2)/25.4,
        ##        height = 4*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 100
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_site_measure_large.png'),
        ##        p + facet_grid(Subindicator + Measure ~ Subregion,
        ##                       as.table = TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))),
        ##        width = 2*(159.2)/25.4,
        ##        height = 4*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 600
        ##    )

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_6_idx1 = structure(paste0("### Site/Measure Index\n"),
        ##                                                      parent = 'TABSET_6'),
        ##                        FIG_REF_6_idx1 = structure(paste0("\n::::: {#fig-6-idx1}\n"),
        ##                                            parent = 'SUBSECTION_6_idx1'),
        ##                        FIG_6_qaqc1 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_idx_site_measure.png)\n"),
        ##                                        parent = "FIG_REF_6_idx1"),
        ##                        FIG_CAP_6_idx1 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n"),
        ##                                            parent = 'FIG_REF_6_idx1'),
        ##                        FIG_REF_6_idx1_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_idx1')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt6 Index plot 1.', return=TRUE)
    ## ----end
    
    ## 8. Generate QAQC figure 5 
    ## ---- Generate QAQC figure 5 
    MMP_tryCatch(
    {
        reneeYear.subregion.measure <- reneeYear.site.measure %>%
            mutate(Weight = 1) %>%
            RC_aggregate(grouping_cols = c('reneeYear','Region','Subregion','Indicator',
                                           'Subindicator','Measure'),
                         gradetype = 'MMP') %>%
            mutate(Grade = MMP_generateOldGrades(Score))

        reneeYear.subregion.measure.boot = reneeYear.site.measure %>%
            mutate(Weight = 1) %>%
            dplyr::select(reneeYear, Region, Subregion, Indicator,
                          Subindicator, Measure, Weight, Score) %>%
            filter(!is.na(Score)) %>% 
            RC_boot_accumulate(seed = 123, size = 100,
                               grouping_cols = c('reneeYear','Region','Subregion',
                                                 'Indicator','Subindicator','Measure'))

        reneeYear.subregion.measure <- reneeYear.subregion.measure %>%
            full_join(reneeYear.subregion.measure.boot$sum)

        ## Worms
        p <- mmp__indicator_trends(reneeYear.subregion.measure, level = 2, type = '6',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_idx_subregion_measure",
                               Plot = p  + facet_grid(Subindicator + Measure ~ Subregion,
                                                      as.table = TRUE,
                                                      labeller = labeller(Subregion =
                                                                              label_wrap_gen(10))),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 4*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional/Measure Index", fig_name_suffix = "wq_alt6_idx_subregion_measure",
                           label_suffix = "_6_idx2", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n")) 


        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_idx_subregion_measure_CI",
                               Plot = p  + facet_grid(Subindicator + Measure ~ Subregion,
                                                      as.table = TRUE,
                                                      labeller = labeller(Subregion =
                                                                              label_wrap_gen(10))) +
                                   geom_linerange(aes(ymin = Lower, ymax = Upper)),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 4*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional/Measure Index Alt.", fig_name_suffix = "wq_alt6_idx_subregion_measure_CI",
                           label_suffix = "_6_idx2a", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n")) 

        
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_measure.pdf'),
        ##        p + facet_grid(Subindicator + Measure ~ Subregion,
        ##                       as.table = TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))),
        ##        width = 2*(159.2)/25.4,
        ##        height = 4*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_measure_CI.pdf'),
        ##        p + facet_grid(Subindicator + Measure ~ Subregion,
        ##                       as.table = TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))) +
        ##        geom_linerange(aes(ymin = Lower, ymax = Upper)),
        ##        width = 2*(159.2)/25.4,
        ##        height = 4*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_measure.png'),
        ##        p + facet_grid(Subindicator + Measure ~ Subregion,
        ##                       as.table = TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))),
        ##        width = 2*(159.2)/25.4,
        ##        height = 4*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 100
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_measure_CI.png'),
        ##        p + facet_grid(Subindicator + Measure ~ Subregion,
        ##                       as.table = TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))) +
        ##        geom_linerange(aes(ymin = Lower, ymax = Upper)),
        ##        width = 2*(159.2)/25.4,
        ##        height = 4*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 100
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_measure_large.png'),
        ##        p + facet_grid(Subindicator + Measure ~ Subregion,
        ##                       as.table = TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))),
        ##        width = 2*(159.2)/25.4,
        ##        height = 4*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 600
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_measure_CI_large.png'),
        ##        p + facet_grid(Subindicator + Measure ~ Subregion,
        ##                       as.table = TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))) +
        ##        geom_linerange(aes(ymin = Lower, ymax = Upper)),
        ##        width = 2*(159.2)/25.4,
        ##        height = 4*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 600
        ##    )

        reneeYear.subregion.measure.worm <- p$data

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_6_idx2 = structure(paste0("### Subregion/Measure Index\n"),
        ##                                                      parent = 'TABSET_6'),
        ##                        FIG_REF_6_idx2 = structure(paste0("\n::::: {#fig-6-idx2}\n"),
        ##                                            parent = 'SUBSECTION_6_idx2'),
        ##                        FIG_6_qaqc2 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_idx_subregion_measure.png)\n"),
        ##                                        parent = "FIG_REF_6_idx2"),
        ##                        FIG_CAP_6_idx2 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n"),
        ##                                            parent = 'FIG_REF_6_idx2'),
        ##                        FIG_REF_6_idx2_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_idx2'),

        ##                        FIG_REF_6_idx2b = structure(paste0("\n::::: {#fig-6-idx2b}\n"),
        ##                                            parent = 'SUBSECTION_6_idx2'),
        ##                        FIG_6_qaqc2b = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_idx_subregion_measure_CI.png)\n"),
        ##                                        parent = "FIG_REF_6_idx2b"),
        ##                        FIG_CAP_6_idx2b = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n"),
        ##                                            parent = 'FIG_REF_6_idx2b'),
        ##                        FIG_REF_6_idx2b_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_idx2')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt6 Index plot 2.', return=TRUE)
    ## ----end

    ## 9. Generate QAQC figure 6 
    ## ---- Generate QAQC figure 6 
    MMP_tryCatch(
    {
        reneeYear.region.measure <- reneeYear.subregion.measure %>%
            mutate(Weight = 1) %>%
            RC_aggregate(grouping_cols = c('reneeYear','Region','Indicator','Subindicator','Measure'),
                         gradetype = 'MMP') %>%
            mutate(Grade = MMP_generateOldGrades(Score))
        
        reneeYear.region.measure.boot = reneeYear.subregion.measure.boot$dist %>%
            mutate(Weight = 1) %>%
            RC_boot_aggregate(seed = 123, size = 100,
                              grouping_cols = c('reneeYear','Region','Indicator',
                                                'Subindicator','Measure'),
                              over = 'Subregion')
        reneeYear.region.measure <- reneeYear.region.measure %>%
            full_join(reneeYear.region.measure.boot$sum)

        ## Worms
        p <- mmp__indicator_trends(reneeYear.region.measure, level = 3, type = '6',
                                   wq.units = wq.units,
                                   wq.sites = wq.sites
                                   )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_idx_region_measure",
                               Plot = p  + facet_grid(Subindicator + Measure ~ Region,
                                                      as.table = TRUE),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 4*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional/Measure Index", fig_name_suffix = "wq_alt6_idx_region_measure",
                           label_suffix = "_6_idx3", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n")) 


        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_idx_region_measure_CI",
                               Plot = p  + facet_grid(Subindicator + Measure ~ Region,
                                                      as.table = TRUE) +
                                   geom_linerange(aes(ymin = Lower, ymax = Upper)),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 4*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional/Measure Index Alt.", fig_name_suffix = "wq_alt6_idx_region_measure_CI",
                           label_suffix = "_6_idx3a", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n")) 

        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_measure.pdf'),
        ##        p + facet_grid(Subindicator + Measure ~ Region,
        ##                       as.table = TRUE),
        ##        width = 2*(159.2)/25.4,
        ##        height = 4*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10
        ##        )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_measure_CI.pdf'),
        ##        p + facet_grid(Subindicator + Measure ~ Region,
        ##                       as.table = TRUE) +
        ##        geom_linerange(aes(ymin = Lower, ymax = Upper)),
        ##        width = 2*(159.2)/25.4,
        ##        height = 4*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10
        ##        )

        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_measure.png'),
        ##        p + facet_grid(Subindicator + Measure ~ Region,
        ##                       as.table = TRUE),
        ##        width = 2*(159.2)/25.4,
        ##        height = 4*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 100
        ##        )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_measure_CI.png'),
        ##        p + facet_grid(Subindicator + Measure ~ Region,
        ##                       as.table = TRUE) +
        ##        geom_linerange(aes(ymin = Lower, ymax = Upper)),
        ##        width = 2*(159.2)/25.4,
        ##        height = 4*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 100
        ##        )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_measure_large.png'),
        ##        p + facet_grid(Subindicator + Measure ~ Region,
        ##                       as.table = TRUE),
        ##        width = 2*(159.2)/25.4,
        ##        height = 4*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 600
        ##        )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_measure_CI_large.png'),
        ##        p + facet_grid(Subindicator + Measure ~ Region,
        ##                       as.table = TRUE) +
        ##        geom_linerange(aes(ymin = Lower, ymax = Upper)),
        ##        width = 2*(159.2)/25.4,
        ##        height = 4*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 600
        ##        )

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_6_idx3 = structure(paste0("### Region/Measure Index\n"),
        ##                                                      parent = 'TABSET_6'),
        ##                        FIG_REF_6_idx3 = structure(paste0("\n::::: {#fig-6-idx3}\n"),
        ##                                            parent = 'SUBSECTION_6_idx3'),
        ##                        FIG_6_qaqc3 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_idx_region_measure.png)\n"),
        ##                                        parent = "FIG_REF_6_idx3"),
        ##                        FIG_CAP_6_idx3 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n"),
        ##                                            parent = 'FIG_REF_6_idx3'),
        ##                        FIG_REF_6_idx3_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_idx3'),

        ##                        FIG_REF_6_idx3b = structure(paste0("\n::::: {#fig-6-idx3b}\n"),
        ##                                            parent = 'SUBSECTION_6_idx3'),
        ##                        FIG_6_qaqc3b = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_idx_region_measure_CI.png)\n"),
        ##                                        parent = "FIG_REF_6_idx3b"),
        ##                        FIG_CAP_6_idx3b = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n"),
        ##                                            parent = 'FIG_REF_6_idx3b'),
        ##                        FIG_REF_6_idx3b_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_idx3')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt6 Index plot 3.', return=TRUE)
    ## ----end

    ## 10. Generate QAQC figure 7 
    ## ---- Generate QAQC figure 7 
    MMP_tryCatch(
    {

        reneeYear.subregion.subindicator <- reneeYear.subregion.measure %>%
        mutate(Weight = 1) %>%
            RC_aggregate(grouping_cols = c('reneeYear','Region','Subregion',
                                           'Indicator','Subindicator'),
                         gradetype = 'MMP') %>%
            mutate(Grade = MMP_generateOldGrades(Score))

        reneeYear.subregion.subindicator.boot <- reneeYear.subregion.measure.boot$dist %>%
            mutate(Weight = 1) %>%
        RC_boot_aggregate(seed = 123, size = 100,
                          grouping_cols = c('reneeYear','Region','Subregion',
                                            'Indicator','Subindicator'),
                          over = 'Measure')

        reneeYear.subregion.subindicator <- reneeYear.subregion.subindicator %>%
            full_join(reneeYear.subregion.subindicator.boot$sum)

        ## Worms
        p <- mmp__indicator_trends(reneeYear.subregion.subindicator, level = 4, type = '6',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_idx_subregion_subindicator",
                               Plot = p  + facet_grid(Subindicator ~ Subregion,
                                                      as.table = TRUE,
                                                      labeller = labeller(Subregion =
                                                                              label_wrap_gen(10))),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 2*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional/Subindicator Index", fig_name_suffix = "wq_alt6_idx_subregion_subindicator",
                           label_suffix = "_6_idx4", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n")) 


        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_idx_subregion_subindicator_CI",
                               Plot = p  + facet_grid(Subindicator ~ Subregion,
                                                      as.table = TRUE,
                                                      labeller = labeller(Subregion =
                                                                              label_wrap_gen(10))) +
                                   geom_linerange(aes(ymin = Lower, ymax = Upper)),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 2*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional/Subindicator Index Alt.", fig_name_suffix = "wq_alt6_idx_subregion_subindicator_CI",
                           label_suffix = "_6_idx4a", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n")) 

        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_subindicator.pdf'),
        ##        p + facet_grid(Subindicator~Subregion, as.table=TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))),
        ##        width=2*(159.2)/25.4,
        ##        height=2*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_subindicator.png'),
        ##        p + facet_grid(Subindicator~Subregion, as.table=TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))),
        ##        width=2*(159.2)/25.4,
        ##        height=2*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 100
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_subindicator_large.png'),
        ##        p + facet_grid(Subindicator~Subregion, as.table=TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))),
        ##        width=2*(159.2)/25.4,
        ##        height=2*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 600
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_subindicator_CI.pdf'),
        ##        p + facet_grid(Subindicator~Subregion,as.table=TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))) +
        ##        geom_linerange(aes(ymin = Lower, ymax = Upper)),
        ##        width = 2*(159.2)/25.4,
        ##        height = 2*(2*(159.2*(2/10))/25.4),
        ##        pointsize = 10
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_subindicator_CI.png'),
        ##        p + facet_grid(Subindicator~Subregion,as.table=TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))) +
        ##        geom_linerange(aes(ymin = Lower, ymax = Upper)),
        ##        width = 2*(159.2)/25.4,
        ##        height = 2*(2*(159.2*(2/10))/25.4),
        ##        pointsize = 10,
        ##        dpi = 100
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_subindicator_CI_large.png'),
        ##        p + facet_grid(Subindicator~Subregion,as.table=TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))) +
        ##        geom_linerange(aes(ymin = Lower, ymax = Upper)),
        ##        width = 2*(159.2)/25.4,
        ##        height = 2*(2*(159.2*(2/10))/25.4),
        ##        pointsize = 10,
        ##        dpi = 300
        ##    )
        wq.alt6.idx.subregion.subindicator <- reneeYear.subregion.subindicator.worm <- p$data
        save(wq.alt6.idx.subregion.subindicator,
             file = paste0(DATA_PATH, '/indices/wq.alt6.idx.subregion.subindicator.RData'))

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_6_idx4 = structure(paste0("### Subregion/Subindicator Index\n"),
        ##                                                      parent = 'TABSET_6'),
        ##                        FIG_REF_6_idx4 = structure(paste0("\n::::: {#fig-6-idx4}\n"),
        ##                                            parent = 'SUBSECTION_6_idx4'),
        ##                        FIG_6_qaqc4 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_idx_subregion_subindicator.png)\n"),
        ##                                        parent = "FIG_REF_6_idx4"),
        ##                        FIG_CAP_6_idx4 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n"),
        ##                                            parent = 'FIG_REF_6_idx4'),
        ##                        FIG_REF_6_idx4_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_idx4'),

        ##                        FIG_REF_6_idx4b = structure(paste0("\n::::: {#fig-6-idx4b}\n"),
        ##                                            parent = 'SUBSECTION_6_idx4'),
        ##                        FIG_6_qaqc4b = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_idx_subregion_subindicator_CI.png)\n"),
        ##                                        parent = "FIG_REF_6_idx4b"),
        ##                        FIG_CAP_6_idx4b = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n"),
        ##                                            parent = 'FIG_REF_6_idx4b'),
        ##                        FIG_REF_6_idx4b_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_idx4')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt6 Index plot 4.', return=TRUE)
    ## ----end

    ## 11. Generate QAQC figure 8 
    ## ---- Generate QAQC figure 8 
    MMP_tryCatch(
    {

        ## reneeYear.region.subindicator (aggregate over subregion)
        reneeYear.region.subindicator <- reneeYear.subregion.subindicator %>%
            mutate(Weight = 1) %>%
            RC_aggregate(grouping_cols = c('reneeYear','Region','Indicator','Subindicator'),
                         gradetype = 'MMP') %>%
            mutate(Grade = MMP_generateOldGrades(Score))

        reneeYear.region.subindicator.boot <- reneeYear.subregion.subindicator.boot$dist %>%
            mutate(Weight = 1) %>%
            RC_boot_aggregate(seed = 123, size = 100,
                              grouping_cols = c('reneeYear','Region','Indicator','Subindicator'),
                              over = 'Subindicator')

        reneeYear.region.subindicator <- reneeYear.region.subindicator %>%
            full_join(reneeYear.region.subindicator.boot$sum)


        ## Worms
        p <- mmp__indicator_trends(reneeYear.region.subindicator, level = 5, type = '6',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_idx_region_subindicator",
                               Plot = p  + facet_grid(Subindicator ~ Region,
                                                      as.table = TRUE),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 2*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional/Subindicator Index", fig_name_suffix = "wq_alt6_idx_region_subindicator",
                           label_suffix = "_6_idx5", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n")) 


        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_idx_region_subindicator_CI",
                               Plot = p  + facet_grid(Subindicator ~ Region,
                                                      as.table = TRUE) +
                                   geom_linerange(aes(ymin = Lower, ymax = Upper)),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 2*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional/Subindicator Index Alt.", fig_name_suffix = "wq_alt6_idx_region_subindicator_CI",
                           label_suffix = "_6_idx5a", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n")) 

        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_subindicator.pdf'),
        ##        p + facet_grid(Subindicator~Region, as.table=TRUE),
        ##        width=2*(159.2)/25.4,
        ##        height=2*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_subindicator.png'),
        ##        p + facet_grid(Subindicator~Region, as.table=TRUE),
        ##        width=2*(159.2)/25.4,
        ##        height=2*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 100
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_subindicator_large.png'),
        ##        p + facet_grid(Subindicator~Region, as.table=TRUE),
        ##        width=2*(159.2)/25.4,
        ##        height=2*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 600
        ##    )

        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_subindicator_CI.pdf'),
        ##        p + facet_grid(Subindicator~Region, as.table=TRUE) +
        ##        geom_linerange(aes(ymin=Lower, ymax=Upper)),
        ##        width=2*(159.2)/25.4,
        ##        height=2*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_subindicator_CI.png'),
        ##        p + facet_grid(Subindicator~Region, as.table=TRUE) +
        ##        geom_linerange(aes(ymin=Lower, ymax=Upper)),
        ##        width=2*(159.2)/25.4,
        ##        height=2*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 100
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_subindicator_CI_large.png'),
        ##        p + facet_grid(Subindicator~Region, as.table=TRUE) +
        ##        geom_linerange(aes(ymin=Lower, ymax=Upper)),
        ##        width=2*(159.2)/25.4,
        ##        height=2*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 600
        ##    )
        wq.alt6.idx.region.subindicator <- p$data
        save(wq.alt6.idx.region.subindicator,
             file = paste0(DATA_PATH, '/indices/wq.alt6.idx.region.subindicator.RData'))

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_6_idx5 = structure(paste0("### Region/Subindicator Index\n"),
        ##                                                      parent = 'TABSET_6'),
        ##                        FIG_REF_6_idx5 = structure(paste0("\n::::: {#fig-6-idx5}\n"),
        ##                                            parent = 'SUBSECTION_6_idx5'),
        ##                        FIG_6_qaqc5 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_idx_region_subindicator.png)\n"),
        ##                                        parent = "FIG_REF_6_idx5"),
        ##                        FIG_CAP_6_idx5 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n"),
        ##                                            parent = 'FIG_REF_6_idx5'),
        ##                        FIG_REF_6_idx5_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_idx5'),

        ##                        FIG_REF_6_idx5b = structure(paste0("\n::::: {#fig-6-idx5b}\n"),
        ##                                            parent = 'SUBSECTION_6_idx5'),
        ##                        FIG_6_qaqc5b = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_idx_region_subindicator_CI.png)\n"),
        ##                                        parent = "FIG_REF_6_idx5b"),
        ##                        FIG_CAP_6_idx5b = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n"),
        ##                                            parent = 'FIG_REF_6_idx5b'),
        ##                        FIG_REF_6_idx5b_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_idx5')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt6 Index plot 5.', return=TRUE)
    ## ----end
    
    ## 12. Generate QAQC figure 9 
    ## ---- Generate QAQC figure 9 
    MMP_tryCatch(
    {
        reneeYear.subregion.indicator <- reneeYear.subregion.subindicator %>%
            mutate(Weight = 1) %>%
            RC_aggregate(grouping_cols = c('reneeYear','Region','Subregion','Indicator'),
                         gradetype = 'MMP') %>%
            mutate(Grade = MMP_generateOldGrades(Score))

        reneeYear.subregion.indicator.boot = reneeYear.subregion.subindicator.boot$dist %>%
            mutate(Weight = 1) %>%
            RC_boot_aggregate(seed = 123, size = 100,
                              grouping_cols=c('reneeYear','Region','Subregion','Indicator'),
                              over='Subindicator')
        reneeYear.subregion.indicator = reneeYear.subregion.indicator %>%
            full_join(reneeYear.subregion.indicator.boot$sum)

        ## Worms
        p <- mmp__indicator_trends(reneeYear.subregion.indicator, level = 6, type = '6',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_idx_subregion_indicator",
                               Plot = p  + facet_grid(Indicator ~ Subregion,
                                                      as.table = TRUE,
                                                      labeller = labeller(Subregion =
                                                                              label_wrap_gen(10))),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 1*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional/Indicator Index", fig_name_suffix = "wq_alt6_idx_subregion_indicator",
                           label_suffix = "_6_idx6", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n")) 


        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_idx_subregion_indicator_CI",
                               Plot = p  + facet_grid(Indicator ~ Subregion,
                                                      as.table = TRUE,
                                                      labeller = labeller(Subregion =
                                                                              label_wrap_gen(10))) +
                                   geom_linerange(aes(ymin = Lower, ymax = Upper)),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 1*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional/Indicator Index Alt.", fig_name_suffix = "wq_alt6_idx_subregion_indicator_CI",
                           label_suffix = "_6_idx6a", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n")) 

        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_indicator.pdf'),
        ##        p + facet_grid(Indicator~Subregion, as.table=TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))),
        ##        width = 2*(159.2)/25.4,
        ##        height = 1*(2*(159.2*(2/10))/25.4),
        ##        pointsize = 10
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_indicator.png'),
        ##        p + facet_grid(Indicator~Subregion, as.table=TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))),
        ##        width = 2*(159.2)/25.4,
        ##        height = 1*(2*(159.2*(2/10))/25.4),
        ##        pointsize = 10,
        ##        dpi = 100
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_indicator_large.png'),
        ##        p + facet_grid(Indicator~Subregion, as.table=TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))),
        ##        width = 2*(159.2)/25.4,
        ##        height = 1*(2*(159.2*(2/10))/25.4),
        ##        pointsize = 10,
        ##        dpi = 600
        ##    )

        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_indicator_CI.pdf'),
        ##        p + facet_grid(Indicator~Subregion, as.table=TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))) +
        ##        geom_linerange(aes(ymin=Lower, ymax=Upper)),
        ##        width = 2*(159.2)/25.4,
        ##        height = 1*(2*(159.2*(2/10))/25.4),
        ##        pointsize = 10
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_indicator_CI.png'),
        ##        p + facet_grid(Indicator~Subregion, as.table=TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))) +
        ##        geom_linerange(aes(ymin=Lower, ymax=Upper)),
        ##        width = 2*(159.2)/25.4,
        ##        height = 1*(2*(159.2*(2/10))/25.4),
        ##        pointsize = 10,
        ##        dpi = 100
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_subregion_indicator_CI_large.png'),
        ##        p + facet_grid(Indicator~Subregion, as.table=TRUE,
        ##                       labeller = labeller(Subregion = label_wrap_gen(10))) +
        ##        geom_linerange(aes(ymin=Lower, ymax=Upper)),
        ##        width = 2*(159.2)/25.4,
        ##        height = 1*(2*(159.2*(2/10))/25.4),
        ##        pointsize = 10,
        ##        dpi = 600
        ##    )

        wq.alt6.idx.subregion.indicator <- p$data
        save(wq.alt6.idx.subregion.indicator,
             file = paste0(DATA_PATH, '/indices/wq.alt6.idx.subregion.indicator.RData'))
        reneeYear.subregion.indicator.worm <- p$data
        wq.alt6.idx.subregion <- reneeYear.subregion.indicator.worm
        save(wq.alt6.idx.subregion,
             file = paste0(DATA_PATH, '/final/wq.alt6.idx.subregion.RData'))

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_6_idx6 = structure(paste0("### Subregion/indicator Index\n"),
        ##                                                      parent = 'TABSET_6'),
        ##                        FIG_REF_6_idx6 = structure(paste0("\n::::: {#fig-6-idx6}\n"),
        ##                                            parent = 'SUBSECTION_6_idx6'),
        ##                        FIG_6_qaqc6 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_idx_subregion_indicator.png)\n"),
        ##                                        parent = "FIG_REF_6_idx6"),
        ##                        FIG_CAP_6_idx6 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n"),
        ##                                            parent = 'FIG_REF_6_idx6'),
        ##                        FIG_REF_6_idx6_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_idx6'),

        ##                        FIG_REF_6_idx6b = structure(paste0("\n::::: {#fig-6-idx6b}\n"),
        ##                                            parent = 'SUBSECTION_6_idx6'),
        ##                        FIG_6_qaqc6b = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_idx_subregion_indicator_CI.png)\n"),
        ##                                        parent = "FIG_REF_6_idx6b"),
        ##                        FIG_CAP_6_idx6b = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n"),
        ##                                            parent = 'FIG_REF_6_idx6b'),
        ##                        FIG_REF_6_idx6b_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_idx6')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt6 Index plot 6.', return=TRUE)
    ## ----end
    
    ## 13. Generate QAQC figure 10 
    ## ---- Generate QAQC figure 10 
    MMP_tryCatch(
    {

        reneeYear.region.indicator <- reneeYear.subregion.indicator %>%
            mutate(Weight = 1) %>%
            RC_aggregate(grouping_cols = c('reneeYear','Region','Indicator'),
                         gradetype = 'MMP') %>%
            mutate(Grade = MMP_generateOldGrades(Score))

        reneeYear.region.indicator.boot <- reneeYear.subregion.indicator.boot$dist %>%
            mutate(Weight = 1) %>%
            RC_boot_aggregate(seed = 123, size = 100,
                              grouping_cols = c('reneeYear','Region','Indicator'),
                              over = 'Subregion')
        reneeYear.region.indicator <- reneeYear.region.indicator %>%
            full_join(reneeYear.region.indicator.boot$sum)

        ## Worms
        p <- mmp__indicator_trends(reneeYear.region.indicator, level = 7, type = '6',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_idx_region_indicator",
                               Plot = p  + facet_grid(Indicator ~ Region,
                                                      as.table = TRUE),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 1*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional/Indicator Index", fig_name_suffix = "wq_alt6_idx_region_indicator",
                           label_suffix = "_6_idx7", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n")) 


        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_idx_region_indicator_CI",
                               Plot = p  + facet_grid(Indicator ~ Region,
                                                      as.table = TRUE) +
                                   geom_linerange(aes(ymin = Lower, ymax = Upper)),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 1*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional/Indicator Index Alt.", fig_name_suffix = "wq_alt6_idx_region_indicator_CI",
                           label_suffix = "_6_idx7a", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n")) 

        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_indicator.pdf'),
        ##        p + facet_grid(Indicator~Region,as.table=TRUE),
        ##        width = 2*(159.2)/25.4,
        ##        height = 1*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_indicator.png'),
        ##        p + facet_grid(Indicator~Region,as.table=TRUE),
        ##        width = 2*(159.2)/25.4,
        ##        height = 1*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 100
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_indicator_large.png'),
        ##        p + facet_grid(Indicator~Region,as.table=TRUE),
        ##        width = 2*(159.2)/25.4,
        ##        height = 1*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 600
        ##    )

        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_indicator_CI.pdf'),
        ##        p + facet_grid(Indicator~Region,as.table=TRUE) +
        ##        geom_linerange(aes(ymin=Lower, ymax=Upper)),
        ##        width = 2*(159.2)/25.4,
        ##        height = 1*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_indicator_CI.png'),
        ##        p + facet_grid(Indicator~Region,as.table=TRUE) +
        ##        geom_linerange(aes(ymin=Lower, ymax=Upper)),
        ##        width = 2*(159.2)/25.4,
        ##        height = 1*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 100
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_indicator_CI_large.png'),
        ##        p + facet_grid(Indicator~Region,as.table=TRUE) +
        ##        geom_linerange(aes(ymin=Lower, ymax=Upper)),
        ##        width = 2*(159.2)/25.4,
        ##        height = 1*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 600
        ##    )

        wq.alt6.idx.region.indicator <- p$data
        save(wq.alt6.idx.region.indicator,
             file = paste0(DATA_PATH, '/indices/wq.alt6.idx.region.indicator.RData'))
        reneeYear.region.indicator.worm <- p$data

        wq.alt6.idx.region <- reneeYear.region.indicator.worm
        save(wq.alt6.idx.region,
             file = paste0(DATA_PATH, '/final/wq.alt6.idx.region.RData'))

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_6_idx7 = structure(paste0("### Region/indicator Index\n"),
        ##                                                      parent = 'TABSET_6'),
        ##                        FIG_REF_6_idx7 = structure(paste0("\n::::: {#fig-6-idx7}\n"),
        ##                                            parent = 'SUBSECTION_6_idx7'),
        ##                        FIG_6_qaqc7 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_idx_region_indicator.png)\n"),
        ##                                        parent = "FIG_REF_6_idx7"),
        ##                        FIG_CAP_6_idx7 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n"),
        ##                                            parent = 'FIG_REF_6_idx7'),
        ##                        FIG_REF_6_idx7_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_idx7'),

        ##                        FIG_REF_6_idx7b = structure(paste0("\n::::: {#fig-6-idx7b}\n"),
        ##                                            parent = 'SUBSECTION_6_idx7'),
        ##                        FIG_6_qaqc7b = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_idx_region_indicator_CI.png)\n"),
        ##                                        parent = "FIG_REF_6_idx7b"),
        ##                        FIG_CAP_6_idx7b = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n"),
        ##                                            parent = 'FIG_REF_6_idx7b'),
        ##                        FIG_REF_6_idx7b_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_idx7')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt6 Index plot 6.', return=TRUE)
    ## ----end
    
    ## 14. Generate QAQC figure 11 
    ## ---- Generate QAQC figure 11 
    MMP_tryCatch(
    {
        p <-
            ggplot(wq.alt6.idx.region.indicator, aes(y = Index, x = reportCardYear)) +
            geom_hline(yintercept = 0, linetype = 'dashed') +
            geom_linerange(aes(ymin = Lower, ymax = Upper)) +
            geom_line(data = wq.alt6.idx.region.subindicator, aes(color = Subindicator)) +
            geom_line() +
            geom_point(aes(fill = Grade), shape = 21, size = 2,
                       show.legend = TRUE, color = 'black') +
            scale_y_continuous('Water Quality Index', limits = c(-1,1)) +
            scale_x_date('', limits = c(as.Date('2006-01-01'),
                                        as.Date(paste0(reportYear,'-01-01')))) +
            scale_fill_manual('Condition', breaks = c('A','B','C','D','E'),
                              values = rev(trafficLightPalette),
                              limits = c('A','B','C','D','E'),
                              labels = c('Very Good','Good','Moderate','Poor','Very Poor')) +
            theme_mmp +
            theme(strip.background = element_blank(),
                  panel.margin.x = unit(1,'line')) +
            scale_color_manual('Indicator',
                               breaks = c('Particulates', 'Productivity', 'Water Clarity'),
                               values = c('#66c2a5', '#fc8d62', '#8da0cb')) +
            facet_grid(~Region, as.table = TRUE) +
            guides(colour = guide_legend(override.aes = list(shape =NA),
                                       label.theme = element_text(size =7),
                                       title.theme = element_text(size =8), 
                                       keyheight = unit(8, 'pt')),
                   fill=guide_legend(label.theme = element_text(size = 7),
                                     title.theme = element_text(size = 8),
                                     keyheight = unit(8, 'pt'))) #+

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt6_idx_region_and_subindicators",
                               Plot = p  + facet_grid(Indicator ~ Region,
                                                      as.table = TRUE),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 1*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional/(sub)Indicator Index", fig_name_suffix = "wq_alt6_idx_region_and_subindicators",
                           label_suffix = "_6_idx8", tabset_parent = "TABSET_6",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n")) 

        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_and_subindicators.pdf'),
        ##        p + facet_grid(Indicator~Region,as.table=TRUE),
        ##        width = 2*(159.2)/25.4,
        ##        height = 1*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_and_subindicators.png'),
        ##        p + facet_grid(Indicator~Region,as.table=TRUE),
        ##        width = 2*(159.2)/25.4,
        ##        height = 1*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 100
        ##    )
        ## ggsave(filename = paste0(FIGURE_OUTPUT_PATH, 'wq_alt6_idx_region_and_subindicators_large.png'),
        ##        p + facet_grid(Indicator~Region,as.table=TRUE),
        ##        width = 2*(159.2)/25.4,
        ##        height = 1*(2*(159.2*(2/10))/25.4),
        ##        pointsize=10,
        ##        dpi = 600
        ##    )

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_6_idx8 = structure(paste0("### Region/(sub)indicator Index\n"),
        ##                                                      parent = 'TABSET_6'),
        ##                        FIG_REF_6_idx8 = structure(paste0("\n::::: {#fig-6-idx8}\n"),
        ##                                            parent = 'SUBSECTION_6_idx8'),
        ##                        FIG_6_qaqc8 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_alt6_idx_region_and_subindicators.png)\n"),
        ##                                        parent = "FIG_REF_6_idx8"),
        ##                        FIG_CAP_6_idx8 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the alt6 (formulation 6) indices.\n"),
        ##                                            parent = 'FIG_REF_6_idx8'),
        ##                        FIG_REF_6_idx8_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_6_idx8')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt6 Index plot 7.', return=TRUE)
    ## ----end

    ## 15. Save data for excel 
    ## ---- Save data for excel
    MMP_tryCatch(
    {
        save(reneeYear.site.measure,
             file = paste0(DATA_PATH, '/final/reneeYear.site.measure.RData'))
        save(reneeYear.subregion.measure,
             file = paste0(DATA_PATH, '/final/reneeYear.subregion.measure.RData'))
        save(reneeYear.region.measure,
             file = paste0(DATA_PATH, '/final/reneeYear.region.measure.RData'))
        save(reneeYear.subregion.subindicator,
             file = paste0(DATA_PATH, '/final/reneeYear.subregion.subindicator.RData'))
        save(reneeYear.subregion.indicator,
             file = paste0(DATA_PATH, '/final/reneeYear.subregion.indicator.RData'))
        save(reneeYear.region.indicator,
             file = paste0(DATA_PATH, '/final/reneeYear.region.indicator.RData'))

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Save data for excel.', return=TRUE)
    ## ----end

    ## 16. Save additional derivatives 
    ## ---- Save additional derivatives
    MMP_tryCatch(
    {
        alt6.idx.tab1 <- reneeYear.subregion.measure.worm %>%
            filter(reneeYear == reportYear) %>%
            dplyr::select(Subregion, Subindicator, Measure,
                          SM.Index = Index, SM.Grade = Grade) %>%
            full_join(
                reneeYear.subregion.subindicator.worm %>%
                filter(reneeYear == reportYear) %>%
                dplyr::select(Subregion, Subindicator,
                              SS.Index = Index, SS.Grade = Grade)
            ) %>%
            dplyr::select(Subregion, Measure, SM.Index, SM.Grade,
                          Subindicator, SS.Index, SS.Grade)
        save(alt6.idx.tab1,
             file = paste0(DATA_PATH, '/final/alt6.idx.tab1.RData'))
        
        alt6.idx.tab2 <- reneeYear.subregion.subindicator.worm %>%
            filter(reneeYear==reportYear) %>%
            dplyr::select(Region, Subregion, Subindicator,
                          SS.Index = Index, SS.Grade = Grade) %>%
            full_join(
                reneeYear.subregion.indicator.worm %>%
                filter(reneeYear == reportYear) %>%
                dplyr::select(Subregion, Indicator,
                              SI.Index = Index, SI.Grade = Grade)
            ) %>%
            dplyr::select(Subregion, Subindicator, SS.Index,
                          SS.Grade, Indicator, SI.Index, SI.Grade)

        save(alt6.idx.tab2,
             file=paste0(DATA_PATH, '/final/alt6.idx.tab2.RData'))
        
        alt6.idx.tab3 <- reneeYear.subregion.indicator.worm %>%
            filter(reneeYear == reportYear) %>%
            dplyr::select(Region, Subregion, SI.Index = Index,
                          SI.Grade = Grade) %>%
            full_join(
                reneeYear.region.indicator.worm %>%
                filter(reneeYear == reportYear) %>%
                dplyr::select(Region, RI.Index = Index,
                              RI.Grade = Grade)
            ) %>%
            dplyr::select(Subregion, SI.Index, SI.Grade,
                          Region, RI.Index, RI.Grade)

        save(alt6.idx.tab3,
             file = paste0(DATA_PATH, '/final/alt6.idx.tab3.RData'))

        save(wq.alt6.idx,
             file = paste0(INDICES_OUTPUT_PATH, 'wq.alt6.idx.RData'))
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Save additional derivatives.', return=TRUE)
    ## ----end

    
    MMP_checkData(name = "wq.alt6.idx.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.prefix = "Processed",
                  PATH = INDICES_OUTPUT_PATH,
                  progressive = FALSE)
    MMP_openning_banner()
}
