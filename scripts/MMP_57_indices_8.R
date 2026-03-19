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

## Type 8 index  
## ---- Type 8
CURRENT_ITEM <- "Type8"
## mmp__add_status(stage = paste0("STAGE", CURRENT_STAGE),
##                 item = CURRENT_ITEM,
##                 name = "Type 8",
##                 status = "progress")
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(INDICES_OUTPUT_PATH,"wq.historic.idx.RData"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'wq.all.reef.RData')) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'wq.historic.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/old.wq.guidelines.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/wq.units.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/new_hierarchy.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/names_lookup.RData')) 
    ) {
  
  MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
                         SUBSECTION_8 = structure(paste0("## Type 8\n"),
                                                  parent = 'TABSET'),
                         TABSET_8 = structure(paste0("\n:::: panel-tabset\n"),
                                              parent = 'SUBSECTION_8'),
                         TABSET_8_END = structure(paste0("\n:::: \n"),
                                                  parent = 'SUBSECTION_8')
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
        hier <- get(load(file=paste0(PARAMS_INPUT_PATH, 'new_hierarchy.RData')))
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
      wq.alt8.idx <-
        ## (wq.alt8.qaqc2 <-
        (wq.alt8.qaqc1 <-
           (wq.alt8.qaqc <- 
              wq.historic %>%
              ## indicate which are the historic reefs
              mutate(HistoricReef = MMP_HistoricReef(MMP_SITE_NAME)) %>% 
              filter(Source %in% c('AIMS Niskin', 'AIMS FLNTU'), HistoricReef == TRUE) %>%
              ungroup %>% #arrange(MMP_SITE_NAME,Measure,financialYear) %>%
              left_join(hier) |> 
              left_join(old.wq.guidelines) %>%
              ungroup() 
           ) %>%
           group_by(MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                    GBRMPA_water_area,Region,Reg,Subregion,Subreg,Source,
                    Indicator, Subindicator,
                    Measure,GL,DirectionOfFailure) %>%
           arrange(MMP_SITE_NAME,Source,Measure,reneeYear) %>%
           filter(!is.na(MMP_SITE_NAME)) %>% 
           summarise(Year = unique(reneeYear),
                     Value = rollAv(Value, reneeYear, location = 'Mean')
                     ) |> 
           mutate(reneeYear = Year)
        ) %>%
        ungroup() %>%
        ## mutate(Index = MMP_WQI_lastYear(.))
        mutate(Index = MMP_WQI_lastYear(.),
               Score = RC_index(x = Value, GL = GL, fold = 2,
                                DOF = DirectionOfFailure, type = "MAMP",
                                capped = TRUE, scaled = TRUE)
               ) 

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Calculate historic indices.', return=TRUE)
    ## ----end

  ## 4. Generate QAQC figure 
  ## ---- Generate QAQC figure 
  MMP_tryCatch(
  {
    ##QAQC figure
    p <- mmp__qaqc(wq.alt8.qaqc, level = 1, type = '8',
                   wq.units = wq.units,
                   wq.sites = wq.sites
                   )

    MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_qaqc",
                           Plot = p, units = "in",
                           fig.width = 180/25.4, fig.height = 180/25.4, pt.size = 10)

    MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                       Section = "QAQC", fig_name_suffix = "wq_alt8_qaqc",
                       label_suffix = "_8_qaqc", tabset_parent = "TABSET_8",
                       fig.caption = paste0("\nObserved ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality data associated with the alt8 (formulation 8) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n")) 
    
  },
  LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt8 QAQC plot 1.', return=TRUE)
  ## ----end

  ## 5. Generate QAQC figure 2 
  ## ---- Generate QAQC figure 2 
  MMP_tryCatch(
  {
    ##QAQC figure 1
    p <- mmp__qaqc(wq.alt8.qaqc1, level = 2, type = '8',
                   wq.units = wq.units,
                   wq.sites = wq.sites
                   )

    MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_qaqc1",
                           Plot = p, units = "in",
                           fig.width = 180/25.4, fig.height = 180/25.4, pt.size = 10)

    MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                       Section = "QAQC 1", fig_name_suffix = "wq_alt8_qaqc1",
                       label_suffix = "_8_qaqc1", tabset_parent = "TABSET_8",
                       fig.caption = paste0("\nSeasonal or annual ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality averages associated with the alt8 (formulation 8) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n")) 

  },
  LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt8 QAQC plot 2.', return=TRUE)
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
        reneeYear.site.measure.season <- wq.alt8.idx %>%
            mutate(Weight = 1) %>%
            dplyr::select(reneeYear, Region, Subregion, MMP_SITE_NAME,
                          GL, Indicator, Subindicator, Measure, Weight, Score) %>%
            RC_aggregate(grouping_cols = c('reneeYear', 'Region', 'Subregion',
                                         'MMP_SITE_NAME', 'GL', 'Indicator',
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
          mutate(Grade = MMP_generateOldGrades(Score)) |>
          mutate(Year = reneeYear) |>
          filter(!is.na(Indicator), !is.na(Subindicator))
        
        
        ##QAQC figure idx
        p <- mmp__qaqc(reneeYear.site.measure, level = 3, type = '8',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_qaqc2",
                                    Plot = p, units = "in",
                                    fig.width = 180/25.4, fig.height = 180/25.4, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "QAQC 2", fig_name_suffix = "wq_alt8_qaqc2",
                           label_suffix = "_8_qaqc2", tabset_parent = "TABSET_8",
                           fig.caption = paste0("\nSeasonal or annual ",as.numeric(reportYear),"/",as.numeric(reportYear)," water quality averages associated with the alt8 (formulation 8) indices. Red and blue symbols represent dry and wet season samples. The purple band defines half and twice the annual guideline values.\n")) 

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt8 QAQC plot 3.', return=TRUE)
    ## ----end

    ## 7. Generate QAQC figure 4 
    ## ---- Generate QAQC figure 4 
    MMP_tryCatch(
    {
        ## Regional Worms
        p <- mmp__indicator_trends(reneeYear.site.measure, level = 1, type = '8',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_idx_site_measure",
                               Plot = p  + facet_grid(Subindicator + Measure ~ Subregion,
                                                      as.table = TRUE,
                                                      labeller = labeller(Subregion =
                                                                              label_wrap_gen(10))),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 4*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Site/Measure Index", fig_name_suffix = "wq_alt8_idx_site_measure",
                           label_suffix = "_8_idx1", tabset_parent = "TABSET_8",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt8 (formulation 8) indices.\n")) 

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt8 Index plot 1.', return=TRUE)
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
        p <- mmp__indicator_trends(reneeYear.subregion.measure, level = 2, type = '8',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_idx_subregion_measure",
                               Plot = p  + facet_grid(Subindicator + Measure ~ Subregion,
                                                      as.table = TRUE,
                                                      labeller = labeller(Subregion =
                                                                              label_wrap_gen(10))),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 4*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional/Measure Index", fig_name_suffix = "wq_alt8_idx_subregion_measure",
                           label_suffix = "_8_idx2", tabset_parent = "TABSET_8",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt8 (formulation 8) indices.\n")) 


        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_idx_subregion_measure_CI",
                               Plot = p  + facet_grid(Subindicator + Measure ~ Subregion,
                                                      as.table = TRUE,
                                                      labeller = labeller(Subregion =
                                                                              label_wrap_gen(10))) +
                                   geom_linerange(aes(ymin = Lower, ymax = Upper)),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 4*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional/Measure Index Alt.", fig_name_suffix = "wq_alt8_idx_subregion_measure_CI",
                           label_suffix = "_8_idx2a", tabset_parent = "TABSET_8",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt8 (formulation 8) indices.\n")) 

        reneeYear.subregion.measure.worm <- p$data
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt8 Index plot 2.', return=TRUE)
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
        p <- mmp__indicator_trends(reneeYear.region.measure, level = 3, type = '8',
                                   wq.units = wq.units,
                                   wq.sites = wq.sites
                                   )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_idx_region_measure",
                               Plot = p  + facet_grid(Subindicator + Measure ~ Region,
                                                      as.table = TRUE),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 4*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional/Measure Index", fig_name_suffix = "wq_alt8_idx_region_measure",
                           label_suffix = "_8_idx3", tabset_parent = "TABSET_8",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt8 (formulation 8) indices.\n")) 


        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_idx_region_measure_CI",
                               Plot = p  + facet_grid(Subindicator + Measure ~ Region,
                                                      as.table = TRUE) +
                                   geom_linerange(aes(ymin = Lower, ymax = Upper)),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 4*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional/Measure Index Alt.", fig_name_suffix = "wq_alt8_idx_region_measure_CI",
                           label_suffix = "_8_idx3a", tabset_parent = "TABSET_8",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt8 (formulation 8) indices.\n")) 

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt8 Index plot 3.', return=TRUE)
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
        p <- mmp__indicator_trends(reneeYear.subregion.subindicator, level = 4, type = '8',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_idx_subregion_subindicator",
                               Plot = p  + facet_grid(Subindicator ~ Subregion,
                                                      as.table = TRUE,
                                                      labeller = labeller(Subregion =
                                                                              label_wrap_gen(10))),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 2*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional/Subindicator Index", fig_name_suffix = "wq_alt8_idx_subregion_subindicator",
                           label_suffix = "_8_idx4", tabset_parent = "TABSET_8",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt8 (formulation 8) indices.\n")) 


        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_idx_subregion_subindicator_CI",
                               Plot = p  + facet_grid(Subindicator ~ Subregion,
                                                      as.table = TRUE,
                                                      labeller = labeller(Subregion =
                                                                              label_wrap_gen(10))) +
                                   geom_linerange(aes(ymin = Lower, ymax = Upper)),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 2*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional/Subindicator Index Alt.", fig_name_suffix = "wq_alt8_idx_subregion_subindicator_CI",
                           label_suffix = "_8_idx4a", tabset_parent = "TABSET_8",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt8 (formulation 8) indices.\n")) 

        wq.alt8.idx.subregion.subindicator <- reneeYear.subregion.subindicator.worm <- p$data
        save(wq.alt8.idx.subregion.subindicator,
             file = paste0(DATA_PATH, '/indices/wq.alt8.idx.subregion.subindicator.RData'))

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt8 Index plot 4.', return=TRUE)
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
        p <- mmp__indicator_trends(reneeYear.region.subindicator, level = 5, type = '7',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_idx_region_subindicator",
                               Plot = p  + facet_grid(Subindicator ~ Region,
                                                      as.table = TRUE),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 2*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional/Subindicator Index", fig_name_suffix = "wq_alt8_idx_region_subindicator",
                           label_suffix = "_8_idx5", tabset_parent = "TABSET_8",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt8 (formulation 8) indices.\n")) 


        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_idx_region_subindicator_CI",
                               Plot = p  + facet_grid(Subindicator ~ Region,
                                                      as.table = TRUE) +
                                   geom_linerange(aes(ymin = Lower, ymax = Upper)),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 2*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional/Subindicator Index Alt.", fig_name_suffix = "wq_alt8_idx_region_subindicator_CI",
                           label_suffix = "_8_idx5a", tabset_parent = "TABSET_8",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt8 (formulation 8) indices.\n")) 

        wq.alt8.idx.region.subindicator <- p$data
        save(wq.alt8.idx.region.subindicator,
             file = paste0(DATA_PATH, '/indices/wq.alt8.idx.region.subindicator.RData'))

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt8 Index plot 5.', return=TRUE)
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
        p <- mmp__indicator_trends(reneeYear.subregion.indicator, level = 6, type = '8',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_idx_subregion_indicator",
                               Plot = p  + facet_grid(Indicator ~ Subregion,
                                                      as.table = TRUE,
                                                      labeller = labeller(Subregion =
                                                                              label_wrap_gen(10))),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 1*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional/Indicator Index", fig_name_suffix = "wq_alt8_idx_subregion_indicator",
                           label_suffix = "_8_idx6", tabset_parent = "TABSET_8",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt8 (formulation 8) indices.\n")) 


        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_idx_subregion_indicator_CI",
                               Plot = p  + facet_grid(Indicator ~ Subregion,
                                                      as.table = TRUE,
                                                      labeller = labeller(Subregion =
                                                                              label_wrap_gen(10))) +
                                   geom_linerange(aes(ymin = Lower, ymax = Upper)),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 1*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional/Indicator Index Alt.", fig_name_suffix = "wq_alt8_idx_subregion_indicator_CI",
                           label_suffix = "_8_idx6a", tabset_parent = "TABSET_8",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt8 (formulation 8) indices.\n")) 


        wq.alt8.idx.subregion.indicator <- p$data
        save(wq.alt8.idx.subregion.indicator,
             file = paste0(DATA_PATH, '/indices/wq.alt8.idx.subregion.indicator.RData'))
        reneeYear.subregion.indicator.worm <- p$data
        wq.alt8.idx.subregion <- reneeYear.subregion.indicator.worm
        save(wq.alt8.idx.subregion,
             file = paste0(DATA_PATH, '/final/wq.alt8.idx.subregion.RData'))

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt8 Index plot 7.', return=TRUE)
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
        p <- mmp__indicator_trends(reneeYear.region.indicator, level = 7, type = '8',
                       wq.units = wq.units,
                       wq.sites = wq.sites
                       )

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_idx_region_indicator",
                               Plot = p  + facet_grid(Indicator ~ Region,
                                                      as.table = TRUE),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 1*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional/Indicator Index", fig_name_suffix = "wq_alt8_idx_region_indicator",
                           label_suffix = "_8_idx7", tabset_parent = "TABSET_8",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt8 (formulation 8) indices.\n")) 


        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_idx_region_indicator_CI",
                               Plot = p  + facet_grid(Indicator ~ Region,
                                                      as.table = TRUE) +
                                   geom_linerange(aes(ymin = Lower, ymax = Upper)),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 1*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional/Indicator Index Alt.", fig_name_suffix = "wq_alt8_idx_region_indicator_CI",
                           label_suffix = "_8_idx7a", tabset_parent = "TABSET_8",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt8 (formulation 8) indices.\n")) 


        wq.alt8.idx.region.indicator <- p$data
        save(wq.alt8.idx.region.indicator,
             file = paste0(DATA_PATH, '/indices/wq.alt8.idx.region.indicator.RData'))
        reneeYear.region.indicator.worm <- p$data

        wq.alt8.idx.region <- reneeYear.region.indicator.worm
        save(wq.alt8.idx.region,
             file = paste0(DATA_PATH, '/final/wq.alt8.idx.region.RData'))

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt8 Index plot 7.', return=TRUE)
    ## ----end

    ## 14. Generate QAQC figure 11 
    ## ---- Generate QAQC figure 11 
    MMP_tryCatch(
    {
        p <-
            ggplot(wq.alt8.idx.region.indicator, aes(y = Index, x = reportCardYear)) +
            geom_hline(yintercept = 0, linetype = 'dashed') +
            geom_linerange(aes(ymin = Lower, ymax = Upper)) +
            geom_line(data = wq.alt8.idx.region.subindicator, aes(color = Subindicator)) +
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
            ## scale_color_manual('Indicator',
            ##                    breaks = c('Particulates', 'Productivity', 'Water Clarity'),
            ##                    values = c('#66c2a5', '#fc8d62', '#8da0cb')) +
            facet_grid(~Region, as.table = TRUE) +
            guides(colour = guide_legend(override.aes = list(shape =NA),
                                       label.theme = element_text(size =7),
                                       title.theme = element_text(size =8), 
                                       keyheight = unit(8, 'pt')),
                   fill=guide_legend(label.theme = element_text(size = 7),
                                     title.theme = element_text(size = 8),
                                     keyheight = unit(8, 'pt'))) #+

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_alt8_idx_region_and_subindicators",
                               Plot = p  + facet_grid(Indicator ~ Region,
                                                      as.table = TRUE),
                               units = "in",
                               fig.width = 2*180/25.4, fig.height = 1*(2*(180*(2/10))/25.4),
                               pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional/(sub)Indicator Index", fig_name_suffix = "wq_alt8_idx_region_and_subindicators",
                           label_suffix = "_8_idx8", tabset_parent = "TABSET_8",
                           fig.caption = paste0("\nTemporal trends in the water quality index conditional on Region for the alt8 (formulation 8) indices.\n")) 

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Generate alt8 Index plot 7.', return=TRUE)
    ## ----end

    ## 15. Save data for excel 
    ## ---- Save data for excel
    MMP_tryCatch(
    {
        save(reneeYear.site.measure,
             file = paste0(DATA_PATH, '/final/reneeYear.site.measure_8.RData'))
        save(reneeYear.subregion.measure,
             file = paste0(DATA_PATH, '/final/reneeYear.subregion.measure_8.RData'))
        save(reneeYear.region.measure,
             file = paste0(DATA_PATH, '/final/reneeYear.region.measure_8.RData'))
        save(reneeYear.subregion.subindicator,
             file = paste0(DATA_PATH, '/final/reneeYear.subregion.subindicator_8.RData'))
        save(reneeYear.subregion.indicator,
             file = paste0(DATA_PATH, '/final/reneeYear.subregion.indicator_8.RData'))
        save(reneeYear.region.indicator,
             file = paste0(DATA_PATH, '/final/reneeYear.region.indicator_8.RData'))

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Save data for excel (Version 8).', return=TRUE)
    ## ----end
  
    ## 16. Save additional derivatives 
    ## ---- Save additional derivatives
    MMP_tryCatch(
    {
        alt8.idx.tab1 <- reneeYear.subregion.measure.worm %>%
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
        save(alt8.idx.tab1,
             file = paste0(DATA_PATH, '/final/alt8.idx.tab1.RData'))
        
        alt8.idx.tab2 <- reneeYear.subregion.subindicator.worm %>%
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

        save(alt8.idx.tab2,
             file=paste0(DATA_PATH, '/final/alt8.idx.tab2.RData'))
        
        alt8.idx.tab3 <- reneeYear.subregion.indicator.worm %>%
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

        save(alt8.idx.tab3,
             file = paste0(DATA_PATH, '/final/alt8.idx.tab3.RData'))

        save(wq.alt8.idx,
             file = paste0(INDICES_OUTPUT_PATH, 'wq.alt8.idx.RData'))
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Save additional derivatives (Version 8).', return=TRUE)
    ## ----end

  
    MMP_checkData(name = "wq.alt8.idx.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.prefix = "Processed",
                  PATH = INDICES_OUTPUT_PATH,
                  progressive = FALSE)
    MMP_openning_banner()
}
