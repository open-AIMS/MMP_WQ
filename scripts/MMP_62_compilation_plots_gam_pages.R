source("MMP_functions.R")
source("MMP_functions_models.R")
source("MMP_functions_indices.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}


MAXDATE <- as.Date(paste0(reportYear,'-09-30'))
MINDATE <- MAXDATE-years(1)+days(1)
minDate <- as.Date("2005-09-01")
GAM_OUTPUT_PATH <- paste0(DATA_PATH, "/models/")
FIGURE_OUTPUT_PATH <- paste0(OUTPUT_PATH, "/figures/models/")
PARAMS_INPUT_PATH <- paste0(DATA_PATH, "/primary/other/")
INDICES_OUTPUT_PATH <- paste0(DATA_PATH, "/indices/")

## Type 0 index (Full old, historic spatial and temporal formulation) - wq.alt1
## ---- Type 0
CURRENT_ITEM <- "GAMPages"
## mmp__add_status(stage = paste0("STAGE", CURRENT_STAGE),
##                 item = CURRENT_ITEM,
##                 name = "Type 0",
##                 status = "progress")
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"gam.tbl.RData"))) &
    file.exists(paste0(GAM_OUTPUT_PATH, 'wq.gams.RData')) &
    file.exists(paste0(DATA_PATH, '/indices/wq.alt6.idx.subregion.subindicator.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, 'names_lookup.RData')) 
    ) {
    ## 1. Read in data
    ## ---- Read in data
    MMP_tryCatch(
    {
        load(file=paste0(DATA_PATH, '/indices/wq.historic.idx.subregion.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt6.idx.subregion.RData'))
        load(file=paste0(GAM_OUTPUT_PATH, 'wq.gams.RData'))
        load(file=paste0(GAM_OUTPUT_PATH, 'wq.aims.jcu.gams.RData'))
        load(file=paste0(GAM_OUTPUT_PATH, 'wq.aims.jcu.omo.gams.RData'))
        load(file=paste0(DATA_PATH, '/primary/other/wq.units.RData'))
        
        load(file = paste0(INDICES_OUTPUT_PATH, 'wq.alt1.idx.subregion.subindicator.RData'))
        load(file = paste0(INDICES_OUTPUT_PATH, 'wq.alt6.idx.subregion.subindicator.RData'))
        reneeYear.subregion.subindicator.worm <- wq.alt6.idx.subregion.subindicator
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Reading in data.', return=TRUE)
    ## ----end

    ## 2. Hist vs Alt6
    ## ---- Hist vs Alt6
    MMP_tryCatch(
    {
        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_0 = structure(paste0("## Hist vs Alt6\n"),
                                                        parent = 'TABSET'),
                               TABSET_0 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_0'),
                               TABSET_0_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_0')
                               )

        gam.tbl <-
            data.frame(Subregion = unique(wq.gams$Subregion)) %>%
            group_by(Subregion) %>%
            nest() %>%
            mutate(Subregion = as.character(Subregion)) %>%
            mutate(Historic.idx.subregion = map(.x = Subregion,
                                                .f = ~ wq.historic.idx.subregion %>%
                                                    dplyr::filter(Year>2007,
                                                           Year<as.numeric(as.character(reportYear))+1,
                                                           Subregion == .x)),
                   alt6.idx.subregion = map(.x = Subregion,
                                            .f = ~ wq.alt6.idx.subregion %>%
                                                dplyr::filter(Subregion == .x))
                   ) %>%
            mutate(Index_Plot = map2(.x = Historic.idx.subregion,
                             .y = alt6.idx.subregion,
                             .f = ~MMP__worm_historic_and_alt(hist.idx = .x, alt.idx = .y,
                                                              minDate = minDate, MAXDATE = MAXDATE))
                             ) %>%
            mutate(AIMS_CHL_Plot = map(.x = Subregion,
                                       .f = ~ MMP__gam_chl_niskin_flntu(niskin = wq.gams,
                                                                        flntu = wq.flntu.gams,
                                                                        subr = .x))) %>%
            mutate(AIMS_NTU_Plot = map(.x = Subregion,
                                       .f = ~ MMP__gam_ntu_flntu(flntu = wq.flntu.gams,
                                                                 subr = .x))) %>%
            mutate(Plot1 = pmap(.l = list(Subregion, Index_Plot, AIMS_CHL_Plot, AIMS_NTU_Plot),
                                .f = ~ MMP__gam_plot1(..1, ..2, ..3, ..4, wq.gams)
                                ))

        pwalk(list(Subregion = gam.tbl$Subregion,
                   Plot1 = gam.tbl$Plot1,
                   Cnt = 1:length(gam.tbl$Subregion)),
              .f = function(Subregion, Plot1, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = "_summary",
                                          Plot = Plot1,
                                          fig.width=7, fig.height=9,
                                          pt.size = 10) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = "_summary",
                                          label_suffix="_Hist_Alt6_",
                                          Cnt, tabset_parent = "TABSET_0",
                                          fig.caption = paste0("\nTemporal trends in a) water quality index as well as b-j) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and red/blue lines represent smooth trends fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
              }
              )          
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt6.', return=TRUE)
    ## ----end


    ## 3b. GAM table 
    ## ---- AIMS GAM table
    MMP_tryCatch(
    {
        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_1b = structure(paste0("## AIMS GAM table\n"),
                                                        parent = 'TABSET'),
                               TABSET_1b = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_1b'),
                               TABSET_1b_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_1b')
                               )
        wq.gams <- wq.gams %>%
            mutate(Subregion = factor(Subregion, levels = c("Barron Daintree",
                                                            "Johnstone Russell Mulgrave",
                                                            "Tully Herbert",
                                                            "Burdekin",
                                                            "Mackay Whitsunday",
                                                            "Fitzroy"))) %>%
            arrange(Subregion)
        pwalk(list(subregion = unique(wq.gams$Subregion),
                   Cnt = 1:length(unique(wq.gams$Subregion))),
              .f = function(subregion, Cnt) { 
                  gam.tbl <- wq.gams %>%
                      filter(Subregion == subregion) %>%
                      dplyr::select(Measure, Change) %>%
                      unnest(c(Change)) %>%
                      left_join(wq.units %>%
                                dplyr::select(Measure, Name.tab.abbr) %>%
                                distinct() %>%
                                mutate(Order = 1:n())) %>%
                      dplyr::select(-Measure) %>%
                      arrange(Order) %>%
                      dplyr::select(-Order) %>%
                      dplyr::select(Measure = Name.tab.abbr, everything()) %>%
                      filter(!is.na(Measure)) %>%
                      suppressMessages() %>%
                      suppressWarnings()

                  MMP__gam_table_quarto(CURRENT_STAGE, subregion,
                                        label_suffix = "_GAM_",
                                        gam.tbl,
                                        Cnt, tabset_parent = "TABSET_1b",
                                        tbl.caption = paste0("\n:Changes in modelled concentrations (95% CI) of all major measures since 2015 in AIMS data. The ratio reflects the fractional change between the last and first years of the focal period.  The lower and upper CI represent the lower and upper 95% confidence interval associated with the fractional change.")
                                        )
              }
              )          
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='AIMS GAM tables', return=TRUE)
    ## ----end
    
    ## 3. 2020 separate the worms 
    ## ---- Hist vs Alt6
    MMP_tryCatch(
    {
        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_1 = structure(paste0("## Hist vs Alt6 2020\n"),
                                                        parent = 'TABSET'),
                               TABSET_1 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_1'),
                               TABSET_1_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_1')
                               )
        gam.tbl <- gam.tbl %>%
            mutate(Index_hist_Plot = map(.x = Historic.idx.subregion,
                                         .f = ~MMP__worm_historic(hist.idx = .x,
                                                                  minDate = minDate, MAXDATE = MAXDATE))
                   ) %>%
            mutate(ReneeYear.subregion.subindicator.worm =
                       map(.x = Subregion,
                           .f = ~ reneeYear.subregion.subindicator.worm %>%
                               filter(Subregion == .x))) %>%
            mutate(Index_alt_Plot = map2(.x = alt6.idx.subregion,
                                        .y = ReneeYear.subregion.subindicator.worm,
                                        .f = ~MMP__worm_alt(alt.idx = .x,
                                                            subregion.worm = .y,
                                                            minDate = minDate, MAXDATE = MAXDATE))
                   ) %>%
            mutate(Plot2 = pmap(.l = list(Subregion, Index_hist_Plot, Index_alt_Plot,
                                          AIMS_CHL_Plot, AIMS_NTU_Plot),
                                .f = ~ MMP__gam_plot2(..1, ..2, ..3, ..4, ..5, wq.gams)
                                ))
        
        pwalk(list(Subregion = gam.tbl$Subregion,
                   Plot2 = gam.tbl$Plot2,
                   Cnt = 1:length(gam.tbl$Subregion)),
              .f = function(Subregion, Plot2, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = "_2020_summary",
                                          Plot = Plot2,
                                          fig.width=7, fig.height=9,
                                          pt.size = 10) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = "_2020_summary",
                                          label_suffix="_Hist_Alt6a_",
                                          Cnt, tabset_parent = "TABSET_1",
                                          fig.caption = paste0("\nTemporal trends in a) water quality index (2015 methodologies), b) water quality index for the current design (with individual coloured subindicators) as well as c-l) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and red/blue lines represent smooth trends fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
              }
              )          
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt6 (separate worms).', return=TRUE)
    ## ----end

    ## 4. 2019 Renee would prefer two separate pages of plots 
    ## ---- Hist vs Alt6
    MMP_tryCatch(
    {
        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_2 = structure(paste0("## Hist vs Alt6 Part 1\n"),
                                                        parent = 'TABSET'),
                               TABSET_2 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_2'),
                               TABSET_2_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_2')
                               )

        gam.tbl <- gam.tbl %>%
            mutate(Plot3a = pmap(.l = list(Subregion, Index_Plot, 
                                          AIMS_CHL_Plot, AIMS_NTU_Plot),
                                .f = ~ MMP__gam_plot3a(..1, ..2, ..3, ..4, wq.gams)
                                )) %>%
            mutate(Plot3b = pmap(.l = list(Subregion),
                                .f = ~ MMP__gam_plot3b(..1, wq.gams)
                                ))
        
        pwalk(list(Subregion = gam.tbl$Subregion,
                   Plot3a = gam.tbl$Plot3a,
                   Cnt = 1:length(gam.tbl$Subregion)),
              .f = function(Subregion, Plot3a, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = "_summary_part1",
                                          Plot = Plot3a,
                                          fig.width=159.2/25.4, fig.height=(159.2*(6/7))/25.4,
                                          pt.size = 12) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = "_summary_part1",
                                          label_suffix="_Hist_Alt6_1_",
                                          Cnt, tabset_parent = "TABSET_2",
                                          fig.caption = paste0("\nTemporal trends in a) water quality index (2015 methodologies, full trend, circular symbols) and (current methodologies, diamond symbols) as well as b-e) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and red/blue lines represent smooth trends fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
              }
              )          

        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_3 = structure(paste0("## Hist vs Alt6 Part 2\n"),
                                                        parent = 'TABSET'),
                               TABSET_3 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_3'),
                               TABSET_3_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_3')
                               )

        pwalk(list(Subregion = gam.tbl$Subregion,
                   Plot3b = gam.tbl$Plot3b,
                   Cnt = 1:length(gam.tbl$Subregion)),
              .f = function(Subregion, Plot3b, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = "_summary_part2",
                                          Plot = Plot3b,
                                          fig.width=159.2/25.4, fig.height=(159.2*(6/7))/25.4,
                                          pt.size = 12) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = "_summary_part2",
                                          label_suffix="_Hist_Alt6_2_",
                                          Cnt, tabset_parent = "TABSET_3",
                                          fig.caption = paste0("\nTemporal trends in a-f) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and blue lines represent smooth trends fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.\n")) 
              }
              )          
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt6 (separate worms).', return=TRUE)
    ## ----end

    ## 5. 2020 Renee would like the historical and recent worms presented in two separate figures
    ##         Renee would prefertwo separate pages of plots 
    ## ---- Hist vs Alt6
    MMP_tryCatch(
    {
        gam.tbl <- gam.tbl %>%
            mutate(Plot4 = pmap(.l = list(Subregion, Index_hist_Plot, Index_alt_Plot, 
                                          AIMS_CHL_Plot, AIMS_NTU_Plot),
                                .f = ~ MMP__gam_plot4(..1, ..2, ..3, ..4, ..5, wq.gams)
                                )) 
        
        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_4 = structure(paste0("## Hist vs Alt6 2020 Part 1\n"),
                                                        parent = 'TABSET'),
                               TABSET_4 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_4'),
                               TABSET_4_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_4')
                               )

        pwalk(list(Subregion = gam.tbl$Subregion,
                   Plot4 = gam.tbl$Plot4,
                   Cnt = 1:length(gam.tbl$Subregion)),
              .f = function(Subregion, Plot4, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = "_summary_2020_part1",
                                          Plot = Plot4,
                                          fig.width=159.2/25.4, fig.height=(159.2*(6/7))/25.4,
                                          pt.size = 12) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = "_summary_2020_part1",
                                          label_suffix="_Hist_Alt6a_1_",
                                          Cnt, tabset_parent = "TABSET_4",
                                          fig.caption = paste0("\nTemporal trends in a) water quality index (2015 methodologies), b) water quality index for the current design (with individual coloured subindicators) as well as c-f) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and red/blue lines represent smooth trends fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
              }
              )          
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt6 (separate worms).', return=TRUE)
    ## ----end


    ## 6. Hist vs Alt6 (AIMS and JCU GAMS)
    ## ---- Hist vs Alt6
    MMP_tryCatch(
    {

        ## Renee does not wish to display PP for CY.  As a result, there are no
        ## GAMs for PP in CY.  We need to replace this item with a blank ggplot
        subs <- c("Endeavour", "Pascoe", "Stewart", "Normanby")
        wq.gams.tmp <- data.frame(Subregion = subs, Measure = "PP.wm") %>%
            group_by(Subregion, Measure) %>%
            nest() %>%
            mutate(plot = map(.x = Subregion,
                              .f = ~ggplot(data = NULL) + theme_nothing())) %>%
            dplyr::select(-data)
        wq.aims.jcu.gams <-
            wq.aims.jcu.gams %>%
            bind_rows(wq.gams.tmp)

        wq.aims.jcu.gams <- wq.aims.jcu.gams %>%
            mutate(Subregion = factor(Subregion, levels = c("Pascoe",
                                                            "Stewart",
                                                            "Normanby",
                                                            "Endeavour",
                                                            "Barron Daintree",
                                                            "Johnstone Russell Mulgrave",
                                                            "Tully Herbert",
                                                            "Burdekin",
                                                            "Mackay Whitsunday",
                                                            "Fitzroy"))) %>%
            arrange(Subregion)
        gam.aims.jcu.tbl <-
            data.frame(Subregion = unique(wq.aims.jcu.gams$Subregion)) %>%
            group_by(Subregion) %>%
            nest() %>%
            mutate(Subregion = as.character(Subregion)) %>%
            mutate(Historic.idx.subregion = map(.x = Subregion,
                                                .f = ~ wq.historic.idx.subregion %>%
                                                    dplyr::filter(Year>2007,
                                                           Year<as.numeric(as.character(reportYear))+1,
                                                           Subregion == .x)),
                   alt6.idx.subregion = map(.x = Subregion,
                                            .f = ~ wq.alt6.idx.subregion %>%
                                                dplyr::filter(Subregion == .x))
                   ) 

        gam.aims.jcu.tbl <-
            gam.aims.jcu.tbl %>%
            mutate(AIMS_CHL_Plot.AIMS.JCU = map(.x = Subregion,
                                       .f = ~ MMP__gam_chl_niskin_flntu(niskin = wq.aims.jcu.gams,
                                                                        flntu = wq.flntu.gams,
                                                                        subr = .x))) %>%
            mutate(Index_Plot = map2(.x = Historic.idx.subregion,
                             .y = alt6.idx.subregion,
                             .f = ~MMP__worm_historic_and_alt(hist.idx = .x, alt.idx = .y,
                                                              minDate = minDate, MAXDATE = MAXDATE))
                             ) %>%
            mutate(AIMS_NTU_Plot = map(.x = Subregion,
                                       .f = ~ MMP__gam_ntu_flntu(flntu = wq.flntu.gams,
                                                                 subr = .x))) %>%
            mutate(Plot1.AIMS.JCU = pmap(.l = list(Subregion, Index_Plot, AIMS_CHL_Plot.AIMS.JCU, AIMS_NTU_Plot),
                                .f = ~ MMP__gam_plot1(..1, ..2, ..3, ..4, wq.aims.jcu.gams)
                                ))

        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_5 = structure(paste0("## Hist vs Alt6 (AIMS & JCU)\n"),
                                                        parent = 'TABSET'),
                               TABSET_5 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_5'),
                               TABSET_5_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_5')
                               )

        pwalk(list(Subregion = gam.aims.jcu.tbl$Subregion,
                   Plot1.AIMS.JCU = gam.aims.jcu.tbl$Plot1.AIMS.JCU,
                   Cnt = 1:length(gam.aims.jcu.tbl$Subregion)),
              .f = function(Subregion, Plot1.AIMS.JCU, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_summary",
                                          Plot = Plot1.AIMS.JCU,
                                          fig.width=7, fig.height=9, pt.size = 10) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_summary",
                                          label_suffix="_Hist_Alt6b_",
                                          Cnt, tabset_parent = "TABSET_5",
                                          fig.caption = paste0("\nTemporal trends in a) water quality index as well as b-j) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and red/blue lines represent smooth trends of AIMS + JCU data fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
              }
              )          

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt6.', return=TRUE)
    ## ----end

    ## 6b. GAM table 
    ## ---- AIMS GAM table
    MMP_tryCatch(
    {
        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_5b = structure(paste0("## AIMS & JCU GAM table\n"),
                                                        parent = 'TABSET'),
                               TABSET_5b = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_5b'),
                               TABSET_5b_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_5b')
                               )
        pwalk(list(subregion = unique(wq.aims.jcu.gams$Subregion),
                   Cnt = 1:length(unique(wq.aims.jcu.gams$Subregion))),
              .f = function(subregion, Cnt) { 
                  gam.tbl <- wq.aims.jcu.gams %>%
                      filter(Subregion == subregion) %>%
                      dplyr::select(Measure, Change) %>%
                      unnest(c(Change)) %>%
                      left_join(wq.units %>%
                                dplyr::select(Measure, Name.tab.abbr) %>%
                                distinct() %>%
                                mutate(Order = 1:n())) %>%
                      dplyr::select(-Measure) %>%
                      arrange(Order) %>%
                      dplyr::select(-Order) %>%
                      dplyr::select(Measure = Name.tab.abbr, everything()) %>%
                      filter(!is.na(Measure)) %>%
                      suppressMessages() %>%
                      suppressWarnings()

                  MMP__gam_table_quarto(CURRENT_STAGE, subregion,
                                        label_suffix = "_GAM_AIMS_JCU",
                                        gam.tbl,
                                        Cnt, tabset_parent = "TABSET_5b",
                                        tbl.caption = paste0("\n:Changes in modelled concentrations (95% CI) of all major measures since 2015 in AIMS and JCU data. The ratio reflects the fractional change between the last and first years of the focal period.  The lower and upper CI represent the lower and upper 95% confidence interval associated with the fractional change.")
                                        )
              }
              )          
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='AIMS & JCU GAM tables', return=TRUE)
    ## ----end

    ## 7. 2020 separate the worms 
    ## ---- Hist vs Alt6
    MMP_tryCatch(
    {
        gam.aims.jcu.tbl <- gam.aims.jcu.tbl %>%
            ## mutate(Index_hist_Plot = map(.x = Historic.idx.subregion,
            ##                              .f = ~MMP__worm_historic(hist.idx = .x,
            ##                                                       minDate = minDate, MAXDATE = MAXDATE))
            ##        ) %>%
            mutate(Index_hist_Plot = map2(.x = Historic.idx.subregion,
                             .y = alt6.idx.subregion,
                             .f = ~MMP__worm_historic_and_alt(hist.idx = .x, alt.idx = .y,
                                                              minDate = minDate, MAXDATE = MAXDATE))
                             ) %>%
            mutate(ReneeYear.subregion.subindicator.worm =
                       map(.x = Subregion,
                           .f = ~ reneeYear.subregion.subindicator.worm %>%
                               filter(Subregion == .x))) %>%
            mutate(Index_alt_Plot = map2(.x = alt6.idx.subregion,
                                        .y = ReneeYear.subregion.subindicator.worm,
                                        .f = ~MMP__worm_alt(alt.idx = .x,
                                                            subregion.worm = .y,
                                                            minDate = minDate, MAXDATE = MAXDATE))
                   ) %>%
            mutate(Plot2.AIMS.JCU = pmap(.l = list(Subregion, Index_hist_Plot, Index_alt_Plot,
                                          AIMS_CHL_Plot.AIMS.JCU, AIMS_NTU_Plot),
                                .f = ~ MMP__gam_plot2(..1, ..2, ..3, ..4, ..5, wq.aims.jcu.gams)
                                ))

        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_6 = structure(paste0("## Hist vs Alt6 (AIMS & JCU, 2020)\n"),
                                                        parent = 'TABSET'),
                               TABSET_6 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_6'),
                               TABSET_6_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_6')
                               )

        pwalk(list(Subregion = gam.aims.jcu.tbl$Subregion,
                   Plot2.AIMS.JCU = gam.aims.jcu.tbl$Plot2.AIMS.JCU,
                   Cnt = 1:length(gam.aims.jcu.tbl$Subregion)),
              .f = function(Subregion, Plot2.AIMS.JCU, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_2020_summary",
                                          Plot = Plot2.AIMS.JCU,
                                          fig.width=7, fig.height=9, pt.size = 10) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_2020_summary",
                                          label_suffix="_Hist_Alt6_3",
                                          Cnt, tabset_parent = "TABSET_6",
                                          fig.caption = paste0("\nTemporal trends in a) water quality index (2015 methodologies), b) water quality index for the current design (with individual coloured subindicators) as well as c-l) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and red/blue lines represent smooth trends of AIMS + JCU data fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
              }
              )          
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt6 (separate worms).', return=TRUE)
    ## ----end

    ## 8. 2019 Renee would prefer two separate pages of plots 
    ## ---- Hist vs Alt6
    MMP_tryCatch(
    {
        gam.aims.jcu.tbl <- gam.aims.jcu.tbl %>%
            mutate(Plot3a.AIMS.JCU = pmap(.l = list(Subregion, Index_Plot, 
                                          AIMS_CHL_Plot.AIMS.JCU, AIMS_NTU_Plot),
                                .f = ~ MMP__gam_plot3a(..1, ..2, ..3, ..4, wq.aims.jcu.gams)
                                )) %>%
            mutate(Plot3b.AIMS.JCU = pmap(.l = list(Subregion),
                                .f = ~ MMP__gam_plot3b(..1, wq.aims.jcu.gams)
                                ))
        
        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_7 = structure(paste0("## Hist vs Alt6 Part 1 (AIMS & JCU)\n"),
                                                        parent = 'TABSET'),
                               TABSET_7 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_7'),
                               TABSET_7_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_7')
                               )

        pwalk(list(Subregion = gam.aims.jcu.tbl$Subregion,
                   Plot3a.AIMS.JCU = gam.aims.jcu.tbl$Plot3a.AIMS.JCU,
                   Cnt = 1:length(gam.aims.jcu.tbl$Subregion)),
              .f = function(Subregion, Plot3a.AIMS.JCU, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_summary_part1",
                                          Plot = Plot3a.AIMS.JCU,
                                          fig.width=159.2/25.4, fig.height=(159.2*(6/7))/25.4,
                                          pt.size = 12) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_summary_part1",
                                          label_suffix="_Hist_Alt6_3a",
                                          Cnt, tabset_parent = "TABSET_7",
                                          fig.caption = paste0("\nTemporal trends in a) water quality index (2015 methodologies, full trend, circular symbols) and (current methodologies, diamond symbols) as well as b-e) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and red/blue lines represent smooth trends of AIMS + JCU data fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
              }
              )          

        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_8 = structure(paste0("## Hist vs Alt6 Part 2 (AIMS & JCU)\n"),
                                                        parent = 'TABSET'),
                               TABSET_8 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_8'),
                               TABSET_8_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_8')
                               )
        pwalk(list(Subregion = gam.aims.jcu.tbl$Subregion,
                   Plot3b.AIMS.JCU = gam.aims.jcu.tbl$Plot3b.AIMS.JCU,
                   Cnt = 1:length(gam.aims.jcu.tbl$Subregion)),
              .f = function(Subregion, Plot3b.AIMS.JCU, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_summary_part2",
                                          Plot = Plot3b.AIMS.JCU,
                                          fig.width=159.2/25.4, fig.height=(159.2*(6/7))/25.4,
                                          pt.size = 12) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_summary_part2",
                                          label_suffix="_Hist_Alt6_3b",
                                          Cnt, tabset_parent = "TABSET_8",
                                          fig.caption = paste0("\nTemporal trends in a-f) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and blue lines represent smooth trends of AIMS + JCU data fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.\n")) 
              }
              )          
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt6 (separate worms).', return=TRUE)
    ## ----end

    ## 9. 2020 Renee would like the historical and recent worms presented in two separate figuresi
    ##         Renee would prefertwo separate pages of plots 
    ## ---- Hist vs Alt6
    MMP_tryCatch(
    {
        gam.aims.jcu.tbl <- gam.aims.jcu.tbl %>%
            mutate(Plot4.AIMS.JCU = pmap(.l = list(Subregion, Index_hist_Plot, Index_alt_Plot, 
                                          AIMS_CHL_Plot.AIMS.JCU, AIMS_NTU_Plot),
                                .f = ~ MMP__gam_plot4(..1, ..2, ..3, ..4, ..5, wq.aims.jcu.gams)
                                )) 
        
        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_9 = structure(paste0("## Hist vs Alt6 Part 1 (AIMS & JCU, 2020)\n"),
                                                        parent = 'TABSET'),
                               TABSET_9 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_9'),
                               TABSET_9_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_9')
                               )
        pwalk(list(Subregion = gam.aims.jcu.tbl$Subregion,
                   Plot4.AIMS.JCU = gam.aims.jcu.tbl$Plot4.AIMS.JCU,
                   Cnt = 1:length(gam.aims.jcu.tbl$Subregion)),
              .f = function(Subregion, Plot4.AIMS.JCU, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_summary_2020_part1",
                                          Plot = Plot4.AIMS.JCU,
                                          fig.width=159.2/25.4, fig.height=(159.2*(6/7))/25.4,
                                          pt.size = 12) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_summary_2020_part1",
                                          label_suffix="_Hist_Alt6_4",
                                          Cnt, tabset_parent = "TABSET_9",
                                          fig.caption = paste0("\nTemporal trends in a) water quality index (2015 methodologies), b) water quality index for the current design (with individual coloured subindicators) as well as c-f) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and red/blue lines represent smooth trends of AIMS + JCU data fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
              }
              )          

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt6 (separate worms).', return=TRUE)
    ## ----end
    
    ## 10. Hist vs Alt6 (AIMS and JCU OMO GAMS)
    ## ---- Hist vs Alt6
    MMP_tryCatch(
    {

        ## Renee does not wish to display PP for CY.  As a result, there are no
        ## GAMs for PP in CY.  We need to replace this item with a blank ggplot
        subs <- c("Endeavour", "Pascoe", "Stewart", "Normanby")
        wq.gams.tmp <- data.frame(Subregion = subs, Measure = "PP.wm") %>%
            group_by(Subregion, Measure) %>%
            nest() %>%
            mutate(plot = map(.x = Subregion,
                              .f = ~ggplot(data = NULL) + theme_nothing())) %>%
            dplyr::select(-data)
        wq.aims.jcu.omo.gams <-
            wq.aims.jcu.omo.gams %>%
            bind_rows(wq.gams.tmp)
        wq.aims.jcu.omo.gams <- wq.aims.jcu.omo.gams %>%
            mutate(Subregion = factor(Subregion, levels = c("Pascoe",
                                                            "Stewart",
                                                            "Normanby",
                                                            "Endeavour",
                                                            "Barron Daintree",
                                                            "Johnstone Russell Mulgrave",
                                                            "Tully Herbert",
                                                            "Burdekin",
                                                            "Mackay Whitsunday",
                                                            "Fitzroy"))) %>%
            arrange(Subregion)
        gam.aims.jcu.omo.tbl <-
            data.frame(Subregion = unique(wq.aims.jcu.omo.gams$Subregion)) %>%
            group_by(Subregion) %>%
            nest() %>%
            mutate(Subregion = as.character(Subregion)) %>%
            mutate(Historic.idx.subregion = map(.x = Subregion,
                                                .f = ~ wq.historic.idx.subregion %>%
                                                    dplyr::filter(Year>2007,
                                                           Year<as.numeric(as.character(reportYear))+1,
                                                           Subregion == .x)),
                   alt6.idx.subregion = map(.x = Subregion,
                                            .f = ~ wq.alt6.idx.subregion %>%
                                                dplyr::filter(Subregion == .x))
                   ) 

        gam.aims.jcu.omo.tbl <-
            gam.aims.jcu.omo.tbl %>%
            mutate(AIMS_CHL_Plot.AIMS.JCU.OMO = map(.x = Subregion,
                                       .f = ~ MMP__gam_chl_niskin_flntu(niskin = wq.aims.jcu.omo.gams,
                                                                        flntu = wq.flntu.gams,
                                                                        subr = .x))) %>%
            mutate(Index_Plot = map2(.x = Historic.idx.subregion,
                             .y = alt6.idx.subregion,
                             .f = ~MMP__worm_historic_and_alt(hist.idx = .x, alt.idx = .y,
                                                              minDate = minDate, MAXDATE = MAXDATE))
                             ) %>%
            mutate(AIMS_NTU_Plot = map(.x = Subregion,
                                       .f = ~ MMP__gam_ntu_flntu(flntu = wq.flntu.gams,
                                                                 subr = .x))) %>%
            mutate(Plot1.AIMS.JCU.OMO = pmap(.l = list(Subregion, Index_Plot,
                                                       AIMS_CHL_Plot.AIMS.JCU.OMO, AIMS_NTU_Plot),
                                .f = ~ MMP__gam_plot1(..1, ..2, ..3, ..4, wq.aims.jcu.omo.gams)
                                ))

        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_10 = structure(paste0("## Hist vs Alt6 (AIMS & JCU, OMO)\n"),
                                                        parent = 'TABSET'),
                               TABSET_10 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_10'),
                               TABSET_10_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_10')
                               )

        pwalk(list(Subregion = gam.aims.jcu.omo.tbl$Subregion,
                   Plot1.AIMS.JCU.OMO = gam.aims.jcu.omo.tbl$Plot1.AIMS.JCU.OMO,
                   Cnt = 1:length(gam.aims.jcu.omo.tbl$Subregion)),
              .f = function(Subregion, Plot1.AIMS.JCU.OMO, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_OMO_summary",
                                          Plot = Plot1.AIMS.JCU.OMO,
                                          fig.width=7, fig.height=9,
                                          pt.size = 10) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_OMO_summary",
                                          label_suffix="_Hist_Alt6_5",
                                          Cnt, tabset_parent = "TABSET_10",
                                          fig.caption = paste0("\nTemporal trends in a) water quality index as well as b-j) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU, Open Coasta/Midshelf/Offshore only) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and red/blue lines represent smooth trends of AIMS + JCU data fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
              }
              )          
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt6.', return=TRUE)
    ## ----end

    ## 10b. GAM table 
    ## ---- AIMS GAM table
    MMP_tryCatch(
    {
        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_10b = structure(paste0("## AIMS & JCU (OMO) GAM table\n"),
                                                        parent = 'TABSET'),
                               TABSET_10b = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_10b'),
                               TABSET_10b_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_10b')
                               )
        wq.aims.jcu.omo.gams <- wq.aims.jcu.omo.gams %>%
            mutate(Subregion = factor(Subregion, levels = c("Pascoe",
                                                            "Stewart",
                                                            "Normanby",
                                                            "Endeavour",
                                                            "Barron Daintree",
                                                            "Johnstone Russell Mulgrave",
                                                            "Tully Herbert",
                                                            "Burdekin",
                                                            "Mackay Whitsunday",
                                                            "Fitzroy"))) %>%
            arrange(Subregion)
        pwalk(list(subregion = unique(wq.aims.jcu.omo.gams$Subregion),
                   Cnt = 1:length(unique(wq.aims.jcu.omo.gams$Subregion))),
              .f = function(subregion, Cnt) { 
                  gam.tbl <- wq.aims.jcu.omo.gams %>%
                      filter(Subregion == subregion) %>%
                      dplyr::select(Measure, Change) %>%
                      unnest(c(Change)) %>%
                      left_join(wq.units %>%
                                dplyr::select(Measure, Name.tab.abbr) %>%
                                distinct() %>%
                                mutate(Order = 1:n())) %>%
                      dplyr::select(-Measure) %>%
                      arrange(Order) %>%
                      dplyr::select(-Order) %>%
                      dplyr::select(Measure = Name.tab.abbr, everything()) %>%
                      filter(!is.na(Measure)) %>%
                      suppressMessages() %>%
                      suppressWarnings()

                  MMP__gam_table_quarto(CURRENT_STAGE, subregion,
                                        label_suffix = "_GAM_AIMS_JCU_OMO",
                                        gam.tbl,
                                        Cnt, tabset_parent = "TABSET_10b",
                                        tbl.caption = paste0("\n:Changes in modelled concentrations (95% CI) of all major measures since 2015 for AIMS and JCU (OMO) data. The ratio reflects the fractional change between the last and first years of the focal period.  The lower and upper CI represent the lower and upper 95% confidence interval associated with the fractional change.")
                                        )
              }
              )          
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='AIMS & JCU (OMO) GAM tables', return=TRUE)
    ## ----end

    ## 11. 2020 separate the worms 
    ## ---- Hist vs Alt6
    MMP_tryCatch(
    {
        gam.aims.jcu.omo.tbl <- gam.aims.jcu.omo.tbl %>%
            mutate(Index_hist_Plot = map(.x = Historic.idx.subregion,
                                         .f = ~MMP__worm_historic(hist.idx = .x,
                                                                  minDate = minDate, MAXDATE = MAXDATE))
                   ) %>%
            mutate(ReneeYear.subregion.subindicator.worm =
                       map(.x = Subregion,
                           .f = ~ reneeYear.subregion.subindicator.worm %>%
                               filter(Subregion == .x))) %>%
            mutate(Index_alt_Plot = map2(.x = alt6.idx.subregion,
                                        .y = ReneeYear.subregion.subindicator.worm,
                                        .f = ~MMP__worm_alt(alt.idx = .x,
                                                            subregion.worm = .y,
                                                            minDate = minDate, MAXDATE = MAXDATE))
                   ) %>%
            mutate(Plot2.AIMS.JCU.OMO = pmap(.l = list(Subregion, Index_hist_Plot, Index_alt_Plot,
                                          AIMS_CHL_Plot.AIMS.JCU.OMO, AIMS_NTU_Plot),
                                .f = ~ MMP__gam_plot2(..1, ..2, ..3, ..4, ..5, wq.aims.jcu.omo.gams)
                                ))
        
        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_11 = structure(paste0("## Hist vs Alt6 (AIMS & JCU, OMO, 2020)\n"),
                                                        parent = 'TABSET'),
                               TABSET_11 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_11'),
                               TABSET_11_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_11')
                               )

        pwalk(list(Subregion = gam.aims.jcu.omo.tbl$Subregion,
                   Plot2.AIMS.JCU.OMO = gam.aims.jcu.omo.tbl$Plot2.AIMS.JCU.OMO,
                   Cnt = 1:length(gam.aims.jcu.omo.tbl$Subregion)),
              .f = function(Subregion, Plot2.AIMS.JCU.OMO, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_OMO_2020_summary",
                                          Plot = Plot2.AIMS.JCU.OMO,
                                          fig.width=7, fig.height=9,
                                          pt.size = 10) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_OMO_2020_summary",
                                          label_suffix="_Hist_Alt6_6",
                                          Cnt, tabset_parent = "TABSET_11",
                                          fig.caption = paste0("\nTemporal trends in a) water quality index (2015 methodologies), b) water quality index for the current design (with individual coloured subindicators) as well as c-l) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU, Open Coasta/Midshelf/Offset only) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and red/blue lines represent smooth trends of AIMS + JCU data fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
              }
              )          

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt6 (separate worms).', return=TRUE)
    ## ----end
    
    ## 12. 2019 Renee would prefer two separate pages of plots 
    ## ---- Hist vs Alt6
    MMP_tryCatch(
    {
        gam.aims.jcu.omo.tbl <- gam.aims.jcu.omo.tbl %>%
            mutate(Plot3a.AIMS.JCU.OMO = pmap(.l = list(Subregion, Index_Plot, 
                                          AIMS_CHL_Plot.AIMS.JCU.OMO, AIMS_NTU_Plot),
                                .f = ~ MMP__gam_plot3a(..1, ..2, ..3, ..4, wq.aims.jcu.omo.gams)
                                )) %>%
            mutate(Plot3b.AIMS.JCU.OMO = pmap(.l = list(Subregion),
                                .f = ~ MMP__gam_plot3b(..1, wq.aims.jcu.omo.gams)
                                ))
        
        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_12 = structure(paste0("## Hist vs Alt6 Part 1 (AIMS & JCU, OMO)\n"),
                                                        parent = 'TABSET'),
                               TABSET_12 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_12'),
                               TABSET_12_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_12')
                               )

        pwalk(list(Subregion = gam.aims.jcu.omo.tbl$Subregion,
                   Plot3a.AIMS.JCU.OMO = gam.aims.jcu.omo.tbl$Plot3a.AIMS.JCU.OMO,
                   Cnt = 1:length(gam.aims.jcu.omo.tbl$Subregion)),
              .f = function(Subregion, Plot3a.AIMS.JCU.OMO, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_OMO_summary_part1",
                                          Plot = Plot3a.AIMS.JCU.OMO,
                                          fig.width=159.2/25.4, fig.height=(159.2*(6/7))/25.4,
                                          pt.size = 12) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_OMO_summary_part1",
                                          label_suffix="_Hist_Alt6_7",
                                          Cnt, tabset_parent = "TABSET_12",
                                          fig.caption = paste0("\nTemporal trends in a) water quality index (2015 methodologies, full trend, circular symbols) and (current methodologies, diamond symbols) as well as b-e) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU, Open Coastal/Midshelf/Offshore only) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and red/blue lines represent smooth trends of AIMS + JCU data fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
              }
              )          

        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_13 = structure(paste0("## Hist vs Alt6 Part 2 (AIMS & JCU, OMO)\n"),
                                                        parent = 'TABSET'),
                               TABSET_13 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_13'),
                               TABSET_13_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_13')
                               )

        pwalk(list(Subregion = gam.aims.jcu.omo.tbl$Subregion,
                   Plot3b.AIMS.JCU.OMO = gam.aims.jcu.omo.tbl$Plot3b.AIMS.JCU.OMO,
                   Cnt = 1:length(gam.aims.jcu.omo.tbl$Subregion)),
              .f = function(Subregion, Plot3b.AIMS.JCU.OMO, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_OMO_summary_part2",
                                          Plot = Plot3b.AIMS.JCU.OMO,
                                          fig.width=159.2/25.4, fig.height=(159.2*(6/7))/25.4,
                                          pt.size = 12) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_OMO_summary_part2",
                                          label_suffix="_Hist_Alt6_8",
                                          Cnt, tabset_parent = "TABSET_12",
                                          fig.caption = paste0("\nTemporal trends in a-f) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU, Open Coastal/Midshelf/Offshore only) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and blue lines represent smooth trends of AIMS + JCU data fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.\n")) 
              }
              )          
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt6 (separate worms).', return=TRUE)
    ## ----end

    ## 13. 2020 Renee would like the historical and recent worms presented in two separate figuresi
    ##         Renee would prefertwo separate pages of plots 
    ## ---- Hist vs Alt6
    MMP_tryCatch(
    {
        gam.aims.jcu.omo.tbl <- gam.aims.jcu.omo.tbl %>%
            mutate(Plot4.AIMS.JCU.OMO = pmap(.l = list(Subregion, Index_hist_Plot, Index_alt_Plot, 
                                          AIMS_CHL_Plot.AIMS.JCU.OMO, AIMS_NTU_Plot),
                                .f = ~ MMP__gam_plot4(..1, ..2, ..3, ..4, ..5, wq.aims.jcu.omo.gams)
                                )) 
        
        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_14 = structure(paste0("## Hist vs Alt6 Part 1 (AIMS & JCU, OMO, 2020)\n"),
                                                        parent = 'TABSET'),
                               TABSET_14 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_14'),
                               TABSET_14_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_14')
                               )

        pwalk(list(Subregion = gam.aims.jcu.omo.tbl$Subregion,
                   Plot4.AIMS.JCU.OMO = gam.aims.jcu.omo.tbl$Plot4.AIMS.JCU.OMO,
                   Cnt = 1:length(gam.aims.jcu.omo.tbl$Subregion)),
              .f = function(Subregion, Plot4.AIMS.JCU.OMO, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_OMO_summary_2020_part1",
                                          Plot = Plot4.AIMS.JCU.OMO,
                                          fig.width=159.2/25.4, fig.height=(159.2*(6/7))/25.4,
                                          pt.size = 12) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_OMO_summary_2020_part1",
                                          label_suffix="_Hist_Alt6_9",
                                          Cnt, tabset_parent = "TABSET_13",
                                          fig.caption = paste0("\nTemporal trends in a) water quality index (2015 methodologies), b) water quality index for the current design (with individual coloured subindicators) as well as c-f) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU, Open Coastal/Midshelf/Offshore only) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and red/blue lines represent smooth trends of AIMS + JCU data fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
              }
              )          

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt6 (separate worms).', return=TRUE)
    ## ----end

    ## 14. 2023 Renee would like the historical and recent worms presented in a single top and bottom
    ## figure
    ## ---- Hist vs Alt6 2023
    MMP_tryCatch(
    {
        wq.alt1.idx.subregion.subindicator <- wq.alt1.idx.subregion.subindicator %>%
            mutate(Index = ifelse(Subregion == "Fitzroy" &
                                  reportCardYear == as.Date("2016-01-01") &
                                  Measure == "CombinedTurb", NA, Index))
        gam.aims.jcu.omo.tbl <- gam.aims.jcu.omo.tbl %>%
            mutate(Alt1.subregion.subindicator = map(.x = Subregion,
                                                      .f = ~ wq.alt1.idx.subregion.subindicator %>%
                                                          filter(Subregion == .x) %>%
                                                          mutate(Measure = factor(ifelse(Measure == 'DRIFTCHL_UGPERL.wm', 'Chl-a',
                                                                                 ifelse(Measure == 'NOx.wm', 'NOx',
                                                                                 ifelse(Measure == 'NTU', 'NTU',
                                                                                 ifelse(Measure == 'PN.wm', 'PN',
                                                                                 ifelse(Measure == 'PP.wm', 'PP',
                                                                                 ifelse(Measure == 'CombinedTurb', 'Water Clarity', '')))))))) %>%
                                                         group_by(Subregion, reportCardYear, Measure) %>%
                                                         summarise(Index=mean(Index))
                                                     )
                   ) %>%
            mutate(Index_hist_plot2 = map2(.x = Historic.idx.subregion,
                                           .y = Alt1.subregion.subindicator,
                                           .f = ~ MMP__worm_hist_2(hist.idx = .x,
                                                                   subregion.worm = .y,
                                                                   minDate = minDate,
                                                                   MAXDATE = MAXDATE))
                   ) %>%
            mutate(Index_alt_Plot2 = map2(.x = alt6.idx.subregion,
                                        .y = ReneeYear.subregion.subindicator.worm,
                                        .f = ~MMP__worm_alt_2023(alt.idx = .x,
                                                            subregion.worm = .y,
                                                            minDate = minDate, MAXDATE = MAXDATE))
                   ) %>%
            mutate(Index_plots = map2(.x = Index_hist_plot2,
                                      .y = Index_alt_Plot2,
                                      .f = ~ .x/.y))
        
        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_13 = structure(paste0("## Hist vs Alt6 (AIMS & JCU, OMO, 2023 part 1)\n"),
                                                        parent = 'TABSET'),
                               TABSET_13 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_13'),
                               TABSET_13_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_13')
                               )
        pwalk(list(Subregion = gam.aims.jcu.omo.tbl$Subregion,
                   IndexPlots.AIMS.JCU.OMO.2023.part1 = gam.aims.jcu.omo.tbl$Index_plots,
                   Cnt = 1:length(gam.aims.jcu.omo.tbl$Subregion)),
              .f = function(Subregion, IndexPlots.AIMS.JCU.OMO.2023.part1, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_OMO_2023_summary_part1",
                                          Plot = IndexPlots.AIMS.JCU.OMO.2023.part1,
                                          fig.width=6, fig.height=6,
                                          pt.size = 12) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_OMO_2023_summary_part1",
                                          label_suffix="_Hist_Alt6_6_1",
                                          Cnt, tabset_parent = "TABSET_13",
                                          fig.caption = paste0("\nTemporal trends in a) water quality index (2015 methodologies), b) water quality index for the current design (with individual coloured subindicators) for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU, Open Coasta/Midshelf/Offset only) and hierarchical formulation (diamond symbols).\n")) 
              }
              )          
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt6 (separate worms).', return=TRUE)
    ## ----end
        
    ## 15. 2023 Renee would like the historical and recent worms presented in a single top and bottom
    ## figure
    ## ---- Hist vs Alt6 2023 part 2
    MMP_tryCatch(
    {
        gam.aims.jcu.omo.tbl <- gam.aims.jcu.omo.tbl %>%
            mutate(GAM_plots_2023 = pmap(.l = list(Subregion,
                                                   AIMS_CHL_Plot.AIMS.JCU.OMO,
                                                   AIMS_NTU_Plot),
                                      .f = ~ MMP__gam_plot2023(..1, ..2, ..3, wq.aims.jcu.omo.gams)))
        
        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_14 = structure(paste0("## Hist vs Alt6 (AIMS & JCU, OMO, 2023 part 2)\n"),
                                                        parent = 'TABSET'),
                               TABSET_14 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_14'),
                               TABSET_14_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_14')
                               )
        pwalk(list(Subregion = gam.aims.jcu.omo.tbl$Subregion,
                   IndexPlots.AIMS.JCU.OMO.2023.part2 = gam.aims.jcu.omo.tbl$GAM_plots_2023,
                   Cnt = 1:length(gam.aims.jcu.omo.tbl$Subregion)),
              .f = function(Subregion, IndexPlots.AIMS.JCU.OMO.2023.part2, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_OMO_2023_summary_part2",
                                          Plot = IndexPlots.AIMS.JCU.OMO.2023.part2,
                                          fig.width=7, fig.height=9,
                                          pt.size = 10) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = ".AIMS_JCU_OMO_2023_summary_part2",
                                          label_suffix="_Hist_Alt6_6_2",
                                          Cnt, tabset_parent = "TABSET_14",
                                          fig.caption = paste0("\nTemporal trends in a-j) generalized additive models of various water quality measures for the ",Subregion," subregion. Black dots represent depth weighted averages and red/blue lines represent smooth trends of AIMS + JCU data fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
              }
              )          

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt6 (separate worms).', return=TRUE)
    ## ----end
    ## save(gam.tbl, file = paste0(GAM_OUTPUT_PATH, "gam.tbl.RData"))
    ## save(gam.aims.jcu.tbl, file = paste0(GAM_OUTPUT_PATH, "gam.aims.jcu.tbl.RData"))
    ## save(gam.aims.jcu.omo.tbl, file = paste0(GAM_OUTPUT_PATH, "gam.aims.jcu.omo.tbl.RData"))

    MMP_checkData(name = "gam.tbl.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.prefix = "Processed",
                  PATH = GAM_OUTPUT_PATH,
                  progressive = FALSE)
    MMP_openning_banner()
}
