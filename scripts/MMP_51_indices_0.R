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
        p <- mmp__qaqc(wq.historic.qaqc, level = 1, type = '0',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )
        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_historic_qaqc",
                                    Plot = p, units = "in",
                                    fig.width = 180/25.4, fig.height = 180/25.4, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "QAQC", fig_name_suffix = "wq_historic_qaqc",
                           label_suffix = "_0_qaqc", tabset_parent = "TABSET_0",
                           fig.caption = paste0("\nObserved ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality data associated with the historic (formulation 0) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n")) 
    
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate QAQC plot 1.', return=TRUE)
    ## ----end

    ## 5. Generate QAQC figure 2 
    ## ---- Generate QAQC figure 2 
    MMP_tryCatch(
    {
        ##QAQC figure 1
        p <- mmp__qaqc(wq.historic.qaqc1, level = 2, type = '0',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_historic_qaqc1",
                                    Plot = p, units = "in",
                                    fig.width = 180/25.4, fig.height = 180/25.4, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "QAQC 1", fig_name_suffix = "wq_historic_qaqc1",
                           label_suffix = "_0_qaqc1", tabset_parent = "TABSET_0",
                           fig.caption = paste0("\nSeasonal or annual ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality averages associated with the historic (formulation 0) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n")) 

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate QAQC plot 2.', return=TRUE)
    ## ----end

    ## 6. Generate QAQC figure 3 
    ## ---- Generate QAQC figure 3 
    MMP_tryCatch(
    {
        ##QAQC figure idx
        p <- mmp__qaqc(wq.historic.qaqc2, level = 3, type = '0',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_historic_qaqc2",
                                    Plot = p, units = "in",
                                    fig.width = 180/25.4, fig.height = 180/25.4, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "QAQC 2", fig_name_suffix = "wq_historic_qaqc2",
                           label_suffix = "_0_qaqc2", tabset_parent = "TABSET_0",
                           fig.caption = paste0("\nSeasonal or annual ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality averages associated with the historic (formulation 0) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n")) 

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate QAQC plot 3.', return=TRUE)
    ## ----end

    ## 7. Generate QAQC figure 4 
    ## ---- Generate QAQC figure 4 
    MMP_tryCatch(
    {
        ## Regional Worms
        p <- mmp__indicator_trends(wq.historic.idx.region, level = 1, type = '0',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )
        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_historic_idx_region",
                               Plot = p + facet_grid(~Region,as.table=FALSE),
                               units = "in",
                               fig.width = 180/25.4, fig.height = 180*(2/7)/25.4, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional Index", fig_name_suffix = "wq_historic_idx_region",
                           label_suffix = "_0_idx1", tabset_parent = "TABSET_0",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n")) 

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_historic_idx_region1",
                               Plot = p + facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'),
                               units = "in",
                               fig.width = 8, fig.height = 2, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional Index alt.", fig_name_suffix = "wq_historic_idx_region1",
                           label_suffix = "_0_idx1a", tabset_parent = "TABSET_0",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n")) 

        wq.historic.idx.region <- p$data
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate QAQC plot 4.', return=TRUE)
    ## ----end

    ## 8. Generate QAQC figure 5 
    ## ---- Generate QAQC figure 5 
    MMP_tryCatch(
    {
        ## Subregion Worms
        p <- mmp__indicator_trends(wq.historic.idx.subregion, level = 2, type = '0',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_historic_idx_subregion",
                               Plot = p + facet_grid(~Subregion,as.table=FALSE),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 2*180*(2/10)/25.4, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregion Index", fig_name_suffix = "wq_historic_idx_subregion",
                           label_suffix = "_0_idx2", tabset_parent = "TABSET_0",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Subregion for the historic (formulation 0) indices.\n")) 

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_historic_idx_subregion1",
                               Plot = p + facet_wrap(~Subregion,as.table=TRUE,nrow=2,scales='free'),
                               units = "in",
                               fig.width = 2*(180)/25.4, fig.height = 2*(159.2*(4/7))/25.4,
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional Index alt.", fig_name_suffix = "wq_historic_idx_subregion1",
                           label_suffix = "_0_idx2a", tabset_parent = "TABSET_0",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Subregion for the historic (formulation 0) indices.\n")) 

        wq.historic.idx.subregion <- p$data
        
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
        save(wq.historic.idx.region,
             file = paste0(DATA_PATH, '/final/wq.historic.idx.region.RData'))
        save(wq.historic.idx.subregion,
             file = paste0(DATA_PATH, '/final/wq.historic.idx.subregion.RData'))
        save(wq.historic.idx.subregion,
             file = paste0(INDICES_OUTPUT_PATH, 'wq.historic.idx.subregion.RData'))
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Save historic indices', return=TRUE)
    ## ----end

    
    MMP_checkData(name = "wq.historic.idx.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.prefix = "Processed",
                  PATH = INDICES_OUTPUT_PATH,
                  progressive = FALSE)
    MMP_openning_banner()
}

