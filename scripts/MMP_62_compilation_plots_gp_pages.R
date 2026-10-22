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
CURRENT_ITEM <- "GPPages"
## mmp__add_status(stage = paste0("STAGE", CURRENT_STAGE),
##                 item = CURRENT_ITEM,
##                 name = "Type 0",
##                 status = "progress")
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()


if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"gam.tbl.RData"))) &
      file.exists(paste0(GAM_OUTPUT_PATH, 'wq_gp.rds')) &
      file.exists(paste0(DATA_PATH, '/indices/wq.alt7.idx.subregion.subindicator.RData')) &
      file.exists(paste0(PARAMS_INPUT_PATH, 'names_lookup.RData')) 
) {

  ## 1. Read in data
  ## ---- Read in data
  MMP_tryCatch(
  {
    load(file=paste0(DATA_PATH, '/indices/wq.historic.idx.subregion.RData'))
    load(file=paste0(DATA_PATH, '/final/wq.alt7.idx.subregion.RData'))
    load(file=paste0(DATA_PATH, '/final/wq.alt8.idx.subregion.RData'))
    load(file=paste0(DATA_PATH, '/final/wq.alt9.idx.subregion.RData'))
    wq_gp <- readRDS(file = paste0(GAM_OUTPUT_PATH, "wq_gp.rds"))
    ## wq.aims.jcu_gp <- readRDS(file = paste0(GAM_OUTPUT_PATH, "wq.aims.jcu_gp.rds"))
    wq.aims.jcu.omo_gp <- readRDS(file = paste0(GAM_OUTPUT_PATH, "wq.aims.jcu.omo_gp.rds"))
    wq.flntu_gp <- readRDS(file = paste0(GAM_OUTPUT_PATH, "wq.flntu_gp.rds"))
    ## load(file=paste0(GAM_OUTPUT_PATH, 'wq.gams.RData'))
    ## load(file=paste0(GAM_OUTPUT_PATH, 'wq.aims.jcu.gams.RData'))
    ## load(file=paste0(GAM_OUTPUT_PATH, 'wq.aims.jcu.omo.gams.RData'))
    load(file=paste0(DATA_PATH, '/primary/other/wq.units.RData'))
    
    load(file = paste0(INDICES_OUTPUT_PATH, 'wq.alt1.idx.subregion.subindicator.RData'))
    load(file = paste0(INDICES_OUTPUT_PATH, 'wq.alt7.idx.subregion.subindicator.RData'))
    load(file = paste0(INDICES_OUTPUT_PATH, 'wq.alt8.idx.subregion.subindicator.RData'))
    load(file = paste0(INDICES_OUTPUT_PATH, 'wq.alt9.idx.subregion.subindicator.RData'))
    reneeYear.subregion.subindicator.worm <- wq.alt7.idx.subregion.subindicator
  },
  LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GP pages):', msg='Reading in data.', return=TRUE)
  ## ----end
   
  ## 2. Hist vs Alt7
  ## ---- Hist vs Alt7
  MMP_tryCatch(
  {
    MMP_add_to_report_list(CURRENT_STAGE, "compilations",
      SUBSECTION_0 = structure(paste0("## Hist (Alt 9) vs New (Alt 7)\n"),
        parent = 'TABSET'),
      TABSET_0 = structure(paste0("\n:::: panel-tabset\n"),
        parent = 'SUBSECTION_0'),
      TABSET_0_END = structure(paste0("\n:::: \n"),
        parent = 'SUBSECTION_0')
    )

    ## If we dont only want to report the results or the data are not yet stored
    if (!reportOnly | !file.exists(paste0(GAM_OUTPUT_PATH, "gp_tbl_1a.rds"))) {
      gp_tbl <-
        ## data.frame(Subregion = unique(wq.gams$Subregion)) %>%
        data.frame(Subregion = unique(wq_gp$Subregion)) %>%
        group_by(Subregion) %>%
        nest() %>%
        mutate(Subregion = as.character(Subregion)) %>%
        mutate(Historic.idx.subregion = map(.x = Subregion,
          ## .f = ~ wq.historic.idx.subregion %>%
          .f = ~ wq.alt9.idx.subregion %>%
            mutate(Year = reneeYear) |> 
            dplyr::filter(Year>2007,
              Year<as.numeric(as.character(reportYear))+1,
              Subregion == .x)),
          alt7.idx.subregion = map(.x = Subregion,
            .f = ~ wq.alt7.idx.subregion %>%
              dplyr::filter(Subregion == .x))
        ) %>%
        mutate(Index_Plot = map2(.x = Historic.idx.subregion,
          .y = alt7.idx.subregion,
          .f = ~MMP__worm_historic_and_alt(hist.idx = .x, alt.idx = .y,
            minDate = minDate, MAXDATE = MAXDATE))
        ) %>%
        mutate(AIMS_CHL_Plot = map(.x = Subregion,
          ## .f = ~ MMP__gam_chl_niskin_flntu(niskin = wq.gams,
          ##                                  flntu = wq.flntu.gams,
          ##                                  subr = .x))) %>%
          .f = ~ MMP__gp_chl_niskin_flntu(niskin = wq_gp,
            subr = .x))) %>%
        mutate(AIMS_NTU_Plot = map(.x = Subregion,
          .f = ~ MMP__gp_ntu_flntu(flntu = wq.flntu_gp,
            subr = .x))) %>%
        mutate(Plot1 = pmap(.l = list(Subregion, Index_Plot, AIMS_CHL_Plot, AIMS_NTU_Plot),
          .f = ~ MMP__gp_plot1(..1, ..2, ..3, ..4, wq_gp)
        )) |>
        mutate(Plot1_file = paste(FIGURE_OUTPUT_PATH, "gamm_", gsub(" ", "_", Subregion),
          "_summary", "png"))

      ## saveRDS(gp_tbl, file = paste0(GAM_OUTPUT_PATH, "gp_tbl.rds"))
      saveRDS(gp_tbl |> dplyr::select(Subregion, Plot1_file),
        file = paste0(GAM_OUTPUT_PATH, "gp_tbl_1a.rds"))

      pwalk(list(Subregion = gp_tbl$Subregion,
        Plot1 = gp_tbl$Plot1,
        Cnt = 1:length(gp_tbl$Subregion)),
        .f = function(Subregion, Plot1, Cnt) { 
          MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
            fig_name_suffix = "_summary",
            Plot = Plot1,
            fig.width=7, fig.height=9,
            pt.size = 10) 
        }
      )
    } else {
      ## gp_tbl <- readRDS(file = paste0(GAM_OUTPUT_PATH, "gp_tbl_1a.rds"))
    }

    gp_tbla <- readRDS(file = paste0(GAM_OUTPUT_PATH, "gp_tbl_1a.rds"))
    if (exists("gp_tbla")) {
      pwalk(list(Subregion = gp_tbl$Subregion,
        Cnt = 1:length(gp_tbl$Subregion)),
        .f = function(Subregion, Cnt) { 
          MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
            fig_name_suffix = "_summary",
            label_suffix="_Hist9_Alt7_",
            Cnt, tabset_parent = "TABSET_0",
            fig.caption = paste0("\nTemporal trends in a) water quality index (Verion 9 and 7) as well as b-j) gaussian process models of various water quality measures for the ", Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS) and hierarchical formulation (diamond symbols).  Points represent raw data and are transparent such that the opacity is proportional to the number of observations of those coordinates. The gaussian process temporal trend is represented by 1200 posterior realisations. Blue, and red regions of the realised trends represent regions for which there is strong evidence (P>0.95) of decline and increase respectively and grey regions represent temporal spans for which evidence of change is not strong.  The shadowed white overlayed temporal trend represents the median of the posterior trends. The horizontal dashed line represents old Water Quality Guidelines.\n")) 
        }
      )          
    }
  },
  LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt7.', return=TRUE)
  ## ----end

      
  ## 10. Hist vs Alt7 (AIMS and JCU OMO GAMS) - From now on it is also using the new Historic index
  ## ---- Hist vs Alt7
  MMP_tryCatch(
  {
    ## Renee does not wish to display PP for CY.  As a result, there are no
    ## GAMs for PP in CY.  We need to replace this item with a blank ggplot
    if (!reportOnly | !file.exists(paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_1a.rds"))) {
      subs <- c("Endeavour", "Pascoe", "Stewart", "Normanby")
      wq_gp_tmp <- data.frame(Subregion = subs, Measure = "PP.wm") %>%
        group_by(Subregion, Measure) %>%
        nest() %>%
        mutate(plot = map(.x = Subregion,
          .f = ~ggplot(data = NULL) + theme_nothing())) %>%
        dplyr::select(-data)
      wq.aims.jcu.omo_gp <-
        wq.aims.jcu.omo_gp %>%
        bind_rows(wq_gp_tmp)
      wq.aims.jcu.omo_gp <- wq.aims.jcu.omo_gp %>%
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
      gp.aims.jcu.omo_tbl <-
        data.frame(Subregion = unique(wq.aims.jcu.omo_gp$Subregion)) %>%
        group_by(Subregion) %>%
        nest() %>%
        mutate(Subregion = as.character(Subregion)) %>%
        mutate(Historic.idx.subregion = map(.x = Subregion,
          ## .f = ~ wq.historic.idx.subregion %>%
          .f = ~ wq.alt9.idx.subregion %>%
            mutate(Year = reneeYear) |> 
            dplyr::filter(Year>2007,
              ## Year < 2016,
              Year<as.numeric(as.character(reportYear))+1,
              Subregion == .x)),
          alt7.idx.subregion = map(.x = Subregion,
            .f = ~ wq.alt7.idx.subregion %>%
              dplyr::filter(Subregion == .x))
        ) 

      gp.aims.jcu.omo_tbl <-
        gp.aims.jcu.omo_tbl %>%
        mutate(AIMS_CHL_Plot.AIMS.JCU.OMO = map(.x = Subregion,
          .f = ~ MMP__gp_chl_niskin_flntu(niskin = wq.aims.jcu.omo_gp,
            ## flntu = wq.flntu.gams,
            subr = .x))) %>%
        mutate(Index_Plot = map2(.x = Historic.idx.subregion,
          .y = alt7.idx.subregion,
          .f = ~MMP__worm_historic_and_alt(hist.idx = .x, alt.idx = .y,
            minDate = minDate, MAXDATE = MAXDATE))
        ) %>%
        mutate(AIMS_NTU_Plot = map(.x = Subregion,
          .f = ~ MMP__gp_ntu_flntu(flntu = wq.flntu_gp,
            subr = .x))) %>%
        mutate(Plot1.AIMS.JCU.OMO = pmap(.l = list(Subregion, Index_Plot,
          AIMS_CHL_Plot.AIMS.JCU.OMO, AIMS_NTU_Plot),
          .f = ~ MMP__gp_plot1(..1, ..2, ..3, ..4, wq.aims.jcu.omo_gp)
        )) |> 
        mutate(Plot1_AIMS.JCU.OMO_file = paste(FIGURE_OUTPUT_PATH, "gamm_", gsub(" ", "_", Subregion),
          ".AIMS_JCU_OMO_summary", "png"))
      ## saveRDS(gp.aims.jcu.omo_tbl,
      ##   file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl.rds"))
      saveRDS(gp.aims.jcu.omo_tbl |> dplyr::select(Subregion, Plot1_AIMS.JCU.OMO_file),
        file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_1a.rds"))
    } else {
      ## gp.aims.jcu.omo_tbl <- readRDS(file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_1a.rds"))
    }

    if(!reportOnly) {
      pwalk(list(Subregion = gp.aims.jcu.omo_tbl$Subregion,
        Plot1.AIMS.JCU.OMO = gp.aims.jcu.omo_tbl$Plot1.AIMS.JCU.OMO,
        Cnt = 1:length(gp.aims.jcu.omo_tbl$Subregion)),
        .f = function(Subregion, Plot1.AIMS.JCU.OMO, Cnt) { 
          MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
            fig_name_suffix = ".AIMS_JCU_OMO_summary",
            Plot = Plot1.AIMS.JCU.OMO,
            fig.width=7, fig.height=9,
            pt.size = 10) 
        }
      )          
    }
    
    gp.aims.jcu.omo_tbla <- readRDS(file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_1a.rds"))
    if (exists("gp.aims.jcu.omo_tbla")) {
      MMP_add_to_report_list(CURRENT_STAGE, "compilations",
        SUBSECTION_10 = structure(paste0("## Hist (Alt 9) vs New (Alt 7) (AIMS & JCU, OMO)\n"),
          parent = 'TABSET'),
        TABSET_10 = structure(paste0("\n:::: panel-tabset\n"),
          parent = 'SUBSECTION_10'),
        TABSET_10_END = structure(paste0("\n:::: \n"),
          parent = 'SUBSECTION_10')
      )

      pwalk(list(Subregion = gp.aims.jcu.omo_tbla$Subregion,
        Cnt = 1:length(gp.aims.jcu.omo_tbla$Subregion)),
        .f = function(Subregion, Cnt) { 
          MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
            fig_name_suffix = ".AIMS_JCU_OMO_summary",
            label_suffix="_Hist9_Alt7_5",
            Cnt, tabset_parent = "TABSET_10",
            fig.caption = paste0("\nTemporal trends in a) water quality index (Verion 9 and 7) as well as b-j) gaussian process models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU, Open Coastal/Midshelf/Offshore only) and hierarchical formulation (diamond symbols).  Points represent raw data and are transparent such that the opacity is proportional to the number of observations of those coordinates.  The gaussian process temporal trend is represented by 1200 posterior realisations.  Blue, and red regions of the realised trends represent regions for which there is strong evidence (P>0.95) of decline and increase respectively and grey regions represent temporal spans for which evidence of change is not strong.  The shadowed white overlayed temporal trend represents the median of the posterior trends. The orizontal dashed line represents old Water Quality Guidelines.\n")) 
        }
      )          
    }
  },
  LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist9 vs Alt7 (AIMS & JCU, OMO)', return=TRUE)
  ## ----end

  ## 10b. GAM table 
  ## ---- AIMS GAM table
  MMP_tryCatch(
  {
    mmp__adapt_measure_name_in_units <- function(dat) {
      dat |>
        mutate(Measure = str_replace(Measure, "_(UGPERL|MGPERL|DEPTH).wm", "")) |> 
        mutate(Measure = str_replace(Measure, ".wm", ""))
    }
    MMP_add_to_report_list(CURRENT_STAGE, "compilations",
      SUBSECTION_10b = structure(paste0("## AIMS & JCU (OMO) GP table\n"),
        parent = 'TABSET'),
      TABSET_10b = structure(paste0("\n:::: panel-tabset\n"),
        parent = 'SUBSECTION_10b'),
      TABSET_10b_END = structure(paste0("\n:::: \n"),
        parent = 'SUBSECTION_10b')
    )
    wq.aims.jcu.omo_gp <- wq.aims.jcu.omo_gp %>%
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
    pwalk(list(subregion = unique(wq.aims.jcu.omo_gp$Subregion),
      Cnt = 1:length(unique(wq.aims.jcu.omo_gp$Subregion))),
      .f = function(subregion, Cnt) { 
        gp_tbl <- wq.aims.jcu.omo_gp %>%
          filter(Subregion == subregion) %>%
          dplyr::select(Measure, periods_of_change) %>%
          filter(lengths(periods_of_change)>0) |> 
          mutate(periods_of_change = map(.x = periods_of_change, readRDS)) |> 
          unnest(c(periods_of_change)) %>%
          left_join(wq.units %>%
                      mmp__adapt_measure_name_in_units() |> 
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
        MMP__gp_table_quarto(CURRENT_STAGE, subregion,
          label_suffix = "_GAM_AIMS_JCU_OMO",
          gp_tbl,
          Cnt, tabset_parent = "TABSET_10b",
          tbl.caption = paste0("\n:Changes in modelled concentrations (95% CI) of all major measures since 2015 for AIMS and JCU (OMO) data. The ratio reflects the fractional change between the last and first years of the focal period.  The lower and upper CI represent the lower and upper 95% confidence interval associated with the fractional change.")
        )

        ## MMP__gam_table_quarto(CURRENT_STAGE, subregion,
        ##   label_suffix = "_GAM_AIMS_JCU_OMO",
        ##   gp_tbl,
        ##   Cnt, tabset_parent = "TABSET_10b",
        ##   tbl.caption = paste0("\n:Changes in modelled concentrations (95% CI) of all major measures since 2015 for AIMS and JCU (OMO) data. The ratio reflects the fractional change between the last and first years of the focal period.  The lower and upper CI represent the lower and upper 95% confidence interval associated with the fractional change.")
        ## )
      }
    )          
  },
  LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='AIMS & JCU (OMO) GP tables', return=TRUE)
  ## ----end
  
  ## 11. 2020 separate the worms 
  ## ---- Hist vs Alt7
  MMP_tryCatch(
  {
    if (!reportOnly) {
      ## if (!reportOnly | !file.exists(paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_2a.rds"))) {
      ## gp.aims.jcu.omo_tbl <- readRDS(file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl.rds"))
      gp.aims.jcu.omo_tbl <- gp.aims.jcu.omo_tbl %>%
        mutate(Index_hist_Plot = map(.x = Historic.idx.subregion,
          .f = ~MMP__worm_historic(hist.idx = .x,
            minDate = minDate, MAXDATE = MAXDATE))
        ) %>%
        mutate(ReneeYear.subregion.subindicator.worm =
                 map(.x = Subregion,
                   .f = ~ reneeYear.subregion.subindicator.worm %>%
                     filter(Subregion == .x))) %>%
        mutate(Index_alt_Plot = map2(.x = alt7.idx.subregion,
          .y = ReneeYear.subregion.subindicator.worm,
          .f = ~MMP__worm_alt(alt.idx = .x,
            subregion.worm = .y,
            minDate = minDate, MAXDATE = MAXDATE))
        ) %>%
        mutate(Plot2.AIMS.JCU.OMO = pmap(.l = list(Subregion, Index_hist_Plot, Index_alt_Plot,
          AIMS_CHL_Plot.AIMS.JCU.OMO, AIMS_NTU_Plot),
          .f = ~ MMP__gp_plot2(..1, ..2, ..3, ..4, ..5, wq.aims.jcu.omo_gp)
        )) |> 
        mutate(Plot2_AIMS.JCU.OMO_file = paste(FIGURE_OUTPUT_PATH, "gamm_", gsub(" ", "_", Subregion),
          ".AIMS_JCU_OMO_2020_summary", "png"))
      ## saveRDS(gp.aims.jcu.omo_tbl, file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl.rds"))
      saveRDS(gp.aims.jcu.omo_tbl |> dplyr::select(Subregion, Plot2_AIMS.JCU.OMO_file),
        file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_2a.rds"))

      pwalk(list(Subregion = gp.aims.jcu.omo_tbl$Subregion,
        Plot2.AIMS.JCU.OMO = gp.aims.jcu.omo_tbl$Plot2.AIMS.JCU.OMO,
        Cnt = 1:length(gp.aims.jcu.omo_tbl$Subregion)),
        .f = function(Subregion, Plot2.AIMS.JCU.OMO, Cnt) { 
          MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
            fig_name_suffix = ".AIMS_JCU_OMO_2020_summary",
            Plot = Plot2.AIMS.JCU.OMO,
            fig.width=7, fig.height=9,
            pt.size = 10) 
        }
      )          
    } else {
      ## gp.aims.jcu.omo_tbl <- readRDS(file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_2a.rds"))
    }
    
    MMP_add_to_report_list(CURRENT_STAGE, "compilations",
      SUBSECTION_11 = structure(paste0("## Hist (Alt 9) vs New (Alt 7) (AIMS & JCU, OMO, 2020)\n"),
        parent = 'TABSET'),
      TABSET_11 = structure(paste0("\n:::: panel-tabset\n"),
        parent = 'SUBSECTION_11'),
      TABSET_11_END = structure(paste0("\n:::: \n"),
        parent = 'SUBSECTION_11')
    )
    gp.aims.jcu.omo_tbla <- readRDS(file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_2a.rds"))
    pwalk(list(Subregion = gp.aims.jcu.omo_tbla$Subregion,
      Cnt = 1:length(gp.aims.jcu.omo_tbla$Subregion)),
      .f = function(Subregion, Cnt) { 
        MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
          fig_name_suffix = ".AIMS_JCU_OMO_2020_summary",
          label_suffix="_Hist9_Alt7_6",
          Cnt, tabset_parent = "TABSET_11",
          fig.caption = paste0("\nTemporal trends in a) water quality index (Verion 9 and 7), b) water quality index for the current design (with individual coloured subindicators) as well as c-j) gaussian process models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU, Open Coastal/Midshelf/Offshore only) and hierarchical formulation (diamond symbols).  Points represent raw data and are transparent such that the opacity is proportional to the number of observations of those coordinates.  The gaussian process temporal trend is represented by 1200 posterior realisations.  Blue, and red regions of the realised trends represent regions for which there is strong evidence (P>0.95) of decline and increase respectively and grey regions represent temporal spans for which evidence of change is not strong.  The shadowed white overlayed temporal trend represents the median of the posterior trends. The orizontal dashed line represents old Water Quality Guidelines.\n")) 
      }
    )          

  },
  LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist9 vs Alt7 (separate worms, AIMS & JCU, OMO 2020).', return=TRUE)
  ## ----end
  
  ## 12. 2019 Renee would prefer two separate pages of plots 
  ## ---- Hist vs Alt7
  MMP_tryCatch(
  {
    if (!reportOnly) {
      gp.aims.jcu.omo_tbl <- gp.aims.jcu.omo_tbl %>%
        mutate(Plot3a.AIMS.JCU.OMO = pmap(.l = list(Subregion, Index_Plot, 
          AIMS_CHL_Plot.AIMS.JCU.OMO, AIMS_NTU_Plot),
          .f = ~ MMP__gp_plot3a(..1, ..2, ..3, ..4, wq.aims.jcu.omo_gp)
        )) %>%
        mutate(Plot3b.AIMS.JCU.OMO = pmap(.l = list(Subregion),
          .f = ~ MMP__gp_plot3b(..1, wq.aims.jcu.omo_gp)
        )) |> 
        mutate(Plot3a_AIMS.JCU.OMO_file = paste(FIGURE_OUTPUT_PATH, "gamm_", gsub(" ", "_", Subregion),
          ".AIMS_JCU_OMO_summary_part1", "png")) |> 
        mutate(Plot3b_AIMS.JCU.OMO_file = paste(FIGURE_OUTPUT_PATH, "gamm_", gsub(" ", "_", Subregion),
          ".AIMS_JCU_OMO_summary_part2", "png"))

      saveRDS(gp.aims.jcu.omo_tbl |> dplyr::select(Subregion, Plot3a_AIMS.JCU.OMO_file, Plot3b_AIMS.JCU.OMO_file),
        file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_3a.rds"))

      pwalk(list(Subregion = gp.aims.jcu.omo_tbl$Subregion,
        Plot3a.AIMS.JCU.OMO = gp.aims.jcu.omo_tbl$Plot3a.AIMS.JCU.OMO,
        Cnt = 1:length(gp.aims.jcu.omo_tbl$Subregion)),
        .f = function(Subregion, Plot3a.AIMS.JCU.OMO, Cnt) { 
          MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
            fig_name_suffix = ".AIMS_JCU_OMO_summary_part1",
            Plot = Plot3a.AIMS.JCU.OMO,
            fig.width=159.2/25.4, fig.height=(159.2*(6/7))/25.4,
            pt.size = 12) 
        }
      )          
      pwalk(list(Subregion = gp.aims.jcu.omo_tbl$Subregion,
        Plot3b.AIMS.JCU.OMO = gp.aims.jcu.omo_tbl$Plot3b.AIMS.JCU.OMO,
        Cnt = 1:length(gp.aims.jcu.omo_tbl$Subregion)),
        .f = function(Subregion, Plot3b.AIMS.JCU.OMO, Cnt) { 
          MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
            fig_name_suffix = ".AIMS_JCU_OMO_summary_part2",
            Plot = Plot3b.AIMS.JCU.OMO,
            fig.width=159.2/25.4, fig.height=(159.2*(6/7))/25.4,
            pt.size = 12) 
        }
      )          

    }

    MMP_add_to_report_list(CURRENT_STAGE, "compilations",
      SUBSECTION_12 = structure(paste0("## Hist (Alt 9) vs New (Alt 7) Part 1 (AIMS & JCU, OMO)\n"),
        parent = 'TABSET'),
      TABSET_12 = structure(paste0("\n:::: panel-tabset\n"),
        parent = 'SUBSECTION_12'),
      TABSET_12_END = structure(paste0("\n:::: \n"),
        parent = 'SUBSECTION_12')
    )
    gp.aims.jcu.omo_tbla <- readRDS(file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_3a.rds"))

    pwalk(list(Subregion = gp.aims.jcu.omo_tbla$Subregion,
      Cnt = 1:length(gp.aims.jcu.omo_tbla$Subregion)),
      .f = function(Subregion, Cnt) { 
        MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
          fig_name_suffix = ".AIMS_JCU_OMO_summary_part1",
          label_suffix="_Hist9_Alt7_7",
          Cnt, tabset_parent = "TABSET_12",
          ## fig.caption = paste0("\nTemporal trends in a) water quality index (2015 methodologies, full trend, circular symbols) and (current methodologies, diamond symbols) as well as b-e) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 sampling design (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU, Open Coastal/Midshelf/Offshore only) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and red/blue lines represent smooth trends of AIMS + JCU data fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
          fig.caption = paste0("\nTemporal trends in a) water quality index (Verion 9 and 7) as well as b-e) gaussian process models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU, Open Coastal/Midshelf/Offshore only) and hierarchical formulation (diamond symbols).  Points represent raw data and are transparent such that the opacity is proportional to the number of observations of those coordinates.  The gaussian process temporal trend is represented by 1200 posterior realisations.  Blue, and red regions of the realised trends represent regions for which there is strong evidence (P>0.95) of decline and increase respectively and grey regions represent temporal spans for which evidence of change is not strong.  The shadowed white overlayed temporal trend represents the median of the posterior trends. The orizontal dashed line represents old Water Quality Guidelines.\n")) 
      }
    )          

    MMP_add_to_report_list(CURRENT_STAGE, "compilations",
      SUBSECTION_13 = structure(paste0("## Hist (Alt 9) vs New (Alt 7) Part 2 (AIMS & JCU, OMO)\n"),
        parent = 'TABSET'),
      TABSET_13 = structure(paste0("\n:::: panel-tabset\n"),
        parent = 'SUBSECTION_13'),
      TABSET_13_END = structure(paste0("\n:::: \n"),
        parent = 'SUBSECTION_13')
    )

    pwalk(list(Subregion = gp.aims.jcu.omo_tbla$Subregion,
      Cnt = 1:length(gp.aims.jcu.omo_tbla$Subregion)),
      .f = function(Subregion, Cnt) { 
        MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
          fig_name_suffix = ".AIMS_JCU_OMO_summary_part2",
          label_suffix="_Hist9_Alt7_8",
          Cnt, tabset_parent = "TABSET_13",
          fig.caption = paste0("\nTemporal trends in a-f) gaussian process models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 sampling design (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU, Open Coastal/Midshelf/Offshore only) and hierarchical formulation (diamond symbols).  Points represent raw data and are transparent such that the opacity is proportional to the number of observations of those coordinates.  The gaussian process temporal trend is represented by 1200 posterior realisations.  Blue, and red regions of the realised trends represent regions for which there is strong evidence (P>0.95) of decline and increase respectively and grey regions represent temporal spans for which evidence of change is not strong.  The shadowed white overlayed temporal trend represents the median of the posterior trends. The orizontal dashed line represents old Water Quality Guidelines.\n")) 
      }
    )          
  },
  LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist9 vs Alt7 Part 2 (separate worms, AIMS & JCU, OMO).', return=TRUE)
  ## ----end

  ## 13. 2020 Renee would like the historical and recent worms presented in two separate figuresi
  ##         Renee would prefertwo separate pages of plots 
  ## ---- Hist vs Alt7
  MMP_tryCatch(
  {
    if (!reportOnly) {
      gp.aims.jcu.omo_tbl <- gp.aims.jcu.omo_tbl %>%
        mutate(Plot4.AIMS.JCU.OMO = pmap(.l = list(Subregion, Index_hist_Plot, Index_alt_Plot, 
          AIMS_CHL_Plot.AIMS.JCU.OMO, AIMS_NTU_Plot),
          .f = ~ MMP__gp_plot4(..1, ..2, ..3, ..4, ..5, wq.aims.jcu.omo_gp)
        )) |> 
        mutate(Plot4_AIMS.JCU.OMO_file = paste(FIGURE_OUTPUT_PATH, "gamm_", gsub(" ", "_", Subregion),
          ".AIMS_JCU_OMO_summary_2020_part1", "png")) 
      
      saveRDS(gp.aims.jcu.omo_tbl |> dplyr::select(Subregion, Plot4_AIMS.JCU.OMO_file),
        file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_4a.rds"))

      pwalk(list(Subregion = gp.aims.jcu.omo_tbl$Subregion,
        Plot4.AIMS.JCU.OMO = gp.aims.jcu.omo_tbl$Plot4.AIMS.JCU.OMO,
        Cnt = 1:length(gp.aims.jcu.omo_tbl$Subregion)),
        .f = function(Subregion, Plot4.AIMS.JCU.OMO, Cnt) { 
          MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
            fig_name_suffix = ".AIMS_JCU_OMO_summary_2020_part1",
            Plot = Plot4.AIMS.JCU.OMO,
            fig.width=159.2/25.4, fig.height=(159.2*(6/7))/25.4,
            pt.size = 12)
        }
      )
    }

    MMP_add_to_report_list(CURRENT_STAGE, "compilations",
      SUBSECTION_14 = structure(paste0("## Hist (Alt 9) vs New (Alt 7) Part 1 (AIMS & JCU, OMO, 2020)\n"),
        parent = 'TABSET'),
      TABSET_14 = structure(paste0("\n:::: panel-tabset\n"),
        parent = 'SUBSECTION_14'),
      TABSET_14_END = structure(paste0("\n:::: \n"),
        parent = 'SUBSECTION_14')
    )

    gp.aims.jcu.omo_tbla <- readRDS(file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_4a.rds"))
    pwalk(list(Subregion = gp.aims.jcu.omo_tbla$Subregion,
      Cnt = 1:length(gp.aims.jcu.omo_tbla$Subregion)),
      .f = function(Subregion, Cnt) { 
        MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
          fig_name_suffix = ".AIMS_JCU_OMO_summary_2020_part1",
          label_suffix="_Hist9_Alt7_9",
          Cnt, tabset_parent = "TABSET_14",
          fig.caption = paste0("\nTemporal trends in a) water quality index (Verion 9 and 7), b) water quality index for the current design (with individual coloured subindicators) as well as c-f) gaussian process models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU, Open Coastal/Midshelf/Offshore only) and hierarchical formulation (diamond symbols).  Points represent raw data and are transparent such that the opacity is proportional to the number of observations of those coordinates.  The gaussian process temporal trend is represented by 1200 posterior realisations.  Blue, and red regions of the realised trends represent regions for which there is strong evidence (P>0.95) of decline and increase respectively and grey regions represent temporal spans for which evidence of change is not strong.  The shadowed white overlayed temporal trend represents the median of the posterior trends. The orizontal dashed line represents old Water Quality Guidelines.\n")) 
      }
    )          


  },
  LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist9 vs Alt7 Part 1 (separate worms, AIMS & JCU, OMO 2020).', return=TRUE)
  ## ----end

  ## 14. 2023 Renee would like the historical and recent worms presented in a single top and bottom
  ## figure
  ## ---- Hist vs Alt7 2023
  MMP_tryCatch(
  {
    if (!reportOnly) {
      ## wq.alt1.idx.subregion.subindicator <- wq.alt1.idx.subregion.subindicator %>%
      ## wq.alt9.idx.subregion.subindicator <- wq.alt9.idx.subregion.subindicator %>%
      ##   mutate(Index = ifelse(Subregion == "Fitzroy" &
      ##                           reportCardYear == as.Date("2016-01-01") &
      ##                           Measure == "CombinedTurb", NA, Index))
      gp.aims.jcu.omo_tbl <- gp.aims.jcu.omo_tbl %>%
        mutate(Alt9.subregion.subindicator = map(.x = Subregion,
          .f =  ~ wq.alt9.idx.subregion.subindicator |>
            filter(Subregion == .x) |>
            mutate(Measure = Subindicator) |> 
            group_by(Subregion, reportCardYear, Measure) |>
            summarise(Index = mean(Index), .groups = "drop")
        )) |> 
        ## mutate(Alt1.subregion.subindicator =
        ## mutate(Alt9.subregion.subindicator =
        ##          map(.x = Subregion,
        ##            ## .f = ~ wq.alt1.idx.subregion.subindicator %>%
        ##            .f = ~ wq.alt9.idx.subregion.subindicator %>%
        ##              filter(Subregion == .x) %>%
        ##              mutate(Measure = factor(ifelse(Measure == 'DRIFTCHL_UGPERL.wm', 'Chlorophyll-a',
        ##                ifelse(Measure == 'NOx.wm', 'Nitrate/Nitrite',
        ##                  ifelse(Measure == 'NTU', 'NTU',
        ##                    ifelse(Measure == 'PN.wm', 'Particulate nitrogen',
        ##                      ifelse(Measure == 'PP.wm', 'Particulate phosphorus',
        ##                        ifelse(Measure == 'CombinedTurb', 'Water Clarity', '')))))))) %>%
        ##              group_by(Subregion, reportCardYear, Measure) %>%
        ##              summarise(Index=mean(Index))
        ##          )
        ## ) %>%
        mutate(Index_hist_plot2 = map2(.x = Historic.idx.subregion,
          ## .y = Alt1.subregion.subindicator,
          .y = Alt9.subregion.subindicator,
          .f = ~ MMP__worm_hist_2(hist.idx = .x,
            subregion.worm = .y,
            minDate = minDate,
            MAXDATE = MAXDATE))
        ) %>%
        mutate(Index_alt_Plot2 =
                 map2(.x = alt7.idx.subregion,
                   .y = ReneeYear.subregion.subindicator.worm,
                   .f = ~MMP__worm_alt_2023(
                     alt.idx = .x %>%
                       filter(!(Subregion %in% c("Pascoe", "Stewart", "Normanby", "Endeavour") &
                                  reportCardYear < as.Date("2021-01-01"))) %>%
                       droplevels(),
                     subregion.worm = .y %>%
                       filter(!(Subregion %in% c("Pascoe", "Stewart", "Normanby", "Endeavour") &                                      reportCardYear < as.Date("2021-01-01"))) %>%
                       droplevels(),
                     minDate = minDate, MAXDATE = MAXDATE))
        ) %>%
        mutate(Index_plots =
                 ifelse(Subregion %in% c("Pascoe", "Stewart", "Normanby", "Endeavour"),
                   Index_alt_Plot2,
                   map2(.x = Index_hist_plot2,
                     .y = Index_alt_Plot2,
                     .f = ~ .x/.y)
                 )
        ) |> 
        mutate(IndexPlots.AIMS.JCU.OMO.2023.part1_file  = paste(FIGURE_OUTPUT_PATH, "gamm_", gsub(" ", "_", Subregion),
          ".AIMS_JCU_OMO_2023_summary_part1", "png")) 
      ## mutate(Index_plots = map2(.x = Index_hist_plot2,
      ##                           .y = Index_alt_Plot2,
      ##                           .f = ~ .x/.y))

      saveRDS(gp.aims.jcu.omo_tbl |> dplyr::select(Subregion, IndexPlots.AIMS.JCU.OMO.2023.part1_file),
        file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_5a.rds"))

      pwalk(list(Subregion = gp.aims.jcu.omo_tbl$Subregion,
        IndexPlots.AIMS.JCU.OMO.2023.part1 = gp.aims.jcu.omo_tbl$Index_plots,
        Cnt = 1:length(gp.aims.jcu.omo_tbl$Subregion)),
        .f = function(Subregion, IndexPlots.AIMS.JCU.OMO.2023.part1, Cnt) { 
          MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
            fig_name_suffix = ".AIMS_JCU_OMO_2023_summary_part1",
            Plot = IndexPlots.AIMS.JCU.OMO.2023.part1,
            fig.width=6,
            fig.height=ifelse(Subregion %in% c("Pascoe", "Stewart", "Normanby", "Endeavour"),3,6),
            pt.size = 12) 
        }
      )          
    }
    
    MMP_add_to_report_list(CURRENT_STAGE, "compilations",
      SUBSECTION_15 = structure(paste0("## Hist (Alt 9) vs New (Alt 7) (AIMS & JCU, OMO, 2023 part 1)\n"),
        parent = 'TABSET'),
      TABSET_15 = structure(paste0("\n:::: panel-tabset\n"),
        parent = 'SUBSECTION_15'),
      TABSET_15_END = structure(paste0("\n:::: \n"),
        parent = 'SUBSECTION_15')
    )

    gp.aims.jcu.omo_tbla <- readRDS(file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_5a.rds"))
    pwalk(list(Subregion = gp.aims.jcu.omo_tbla$Subregion,
      Cnt = 1:length(gp.aims.jcu.omo_tbla$Subregion)),
      .f = function(Subregion, Cnt) { 
        MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
          fig_name_suffix = ".AIMS_JCU_OMO_2023_summary_part1",
          label_suffix="_Hist_Alt7_6_1",
          Cnt, tabset_parent = "TABSET_15",
          fig.caption = paste0("\nTemporal trends in a) water quality index (2015 methodologies), b) water quality index for the current design (with individual coloured subindicators) for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS + JCU, Open Coasta/Midshelf/Offset only) and hierarchical formulation (diamond symbols).\n")) 
      }
    )          
    
  },
  LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist9 vs Alt7 (separate worms, AIMS & JCU, OMO 2023).', return=TRUE)
  ## ----end

  ## 15. 2023 Renee would like the historical and recent worms presented in a single top and bottom
  ## figure
  ## ---- Hist vs Alt7 2023 part 2
  MMP_tryCatch(
  {
    if (!reportOnly) {
      gp.aims.jcu.omo_tbl <- gp.aims.jcu.omo_tbl %>%
        mutate(GAM_plots_2023 = pmap(.l = list(Subregion,
          AIMS_CHL_Plot.AIMS.JCU.OMO,
          AIMS_NTU_Plot),
          .f = ~ MMP__gp_plot2023(..1, ..2, ..3, wq.aims.jcu.omo_gp))) |>
        mutate(GAM_plots_2023_file  = paste(FIGURE_OUTPUT_PATH, "gamm_", gsub(" ", "_", Subregion),
          ".AIMS_JCU_OMO_2023_summary_part2", "png")) 

      saveRDS(gp.aims.jcu.omo_tbl |> dplyr::select(Subregion, GAM_plots_2023_file),
        file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_6a.rds"))

      pwalk(list(Subregion = gp.aims.jcu.omo_tbl$Subregion,
        GAM_plots_2023 = gp.aims.jcu.omo_tbl$GAM_plots_2023,
        Cnt = 1:length(gp.aims.jcu.omo_tbl$Subregion)),
        .f = function(Subregion, GAM_plots_2023, Cnt) { 
          MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
            fig_name_suffix = ".AIMS_JCU_OMO_2023_summary_part2",
            Plot = IndexPlots.AIMS.JCU.OMO.2023.part2,
            fig.width=7, fig.height=9,
            pt.size = 10) 
        }
      )          
    }
    
    MMP_add_to_report_list(CURRENT_STAGE, "compilations",
      SUBSECTION_16 = structure(paste0("## Hist (Alt 9) vs New (Alt 7) (AIMS & JCU, OMO, 2023 part 2)\n"),
        parent = 'TABSET'),
      TABSET_16 = structure(paste0("\n:::: panel-tabset\n"),
        parent = 'SUBSECTION_16'),
      TABSET_16_END = structure(paste0("\n:::: \n"),
        parent = 'SUBSECTION_16')
    )

    gp.aims.jcu.omo_tbla <- readRDS(file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_6a.rds"))
    pwalk(list(Subregion = gp.aims.jcu.omo_tbla$Subregion,
      Cnt = 1:length(gp.aims.jcu.omo_tbla$Subregion)),
      .f = function(Subregion, Cnt) { 
        MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
          fig_name_suffix = ".AIMS_JCU_OMO_2023_summary_part2",
          label_suffix="_Hist_Alt7_6_2",
          Cnt, tabset_parent = "TABSET_16",
          fig.caption = paste0("\nTemporal trends in a-j) generalized additive models of various water quality measures for the ",Subregion," subregion. Black dots represent depth weighted averages and red/blue lines represent smooth trends of AIMS + JCU data fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
      }
    )          

  },
  LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist9 vs Alt7 (separate worms, AIMS & JCU, OMO 2023 part 2).', return=TRUE)
  ## ----end
  
  ## 16. 2026 Renee would like the historical and recent worms presented in a single 
  ## figure in which the long-term trend stops < 2016 and the annual trend is
  ## from 2016 onwards 
  ## ---- Hist vs Alt7 2026
  MMP_tryCatch(
  {
    if (!reportOnly) {
      ## load(file=paste0(GAM_OUTPUT_PATH, 'wq.aims.jcu.omo.gams.RData'))
      ## load(file = paste0(INDICES_OUTPUT_PATH, 'wq.alt1.idx.subregion.subindicator.RData'))
      ## load(file = paste0(INDICES_OUTPUT_PATH, 'wq.alt7.idx.subregion.subindicator.RData'))
      ## reneeYear.subregion.subindicator.worm <- wq.alt7.idx.subregion.subindicator

      ## ## wq.alt1.idx.subregion.subindicator <- wq.alt1.idx.subregion.subindicator %>%
      ## wq.alt9.idx.subregion.subindicator <- wq.alt9.idx.subregion.subindicator %>%
      ##     mutate(Index = ifelse(Subregion == "Fitzroy" &
      ##                           reportCardYear == as.Date("2016-01-01") &
      ##                           Measure == "CombinedTurb", NA, Index))
      ## gp.aims.jcu.omo_tbl <- gp.aims.jcu.omo_tbl %>%
      ##   ## mutate(Alt1.subregion.subindicator =
      ##   mutate(Alt9.subregion.subindicator =
      ##            map(.x = Subregion,
      ##                ## .f = ~ wq.alt1.idx.subregion.subindicator %>%
      ##                .f = ~ wq.alt9.idx.subregion.subindicator %>%
      ##                  filter(Subregion == .x) %>%
      ##                  mutate(Measure = factor(ifelse(Measure == 'DRIFTCHL_UGPERL.wm', 'Chl-a',
      ##                                          ifelse(Measure == 'NOx.wm', 'NOx',
      ##                                          ifelse(Measure == 'NTU', 'NTU',
      ##                                          ifelse(Measure == 'PN.wm', 'PN',
      ##                                          ifelse(Measure == 'PP.wm', 'PP',
      ##                                          ifelse(Measure == 'CombinedTurb', 'Water Clarity', '')))))))) %>%
      ##                  group_by(Subregion, reportCardYear, Measure) %>%
      ##                  summarise(Index=mean(Index))
      ##                )
      ##          ) %>%
      gp.aims.jcu.omo_tbl <- gp.aims.jcu.omo_tbl |> 
        mutate(Index_hist_plot2 = map2(.x = Historic.idx.subregion,
          ## .y = Alt1.subregion.subindicator,
          .y = Alt9.subregion.subindicator,
          .f = ~ MMP__worm_hist_2(hist.idx = .x,
            subregion.worm = .y,
            minDate = minDate,
            MAXDATE = MAXDATE))
        ) %>%
        mutate(Index_alt_Plot2 =
                 map2(.x = alt7.idx.subregion,
                   .y = ReneeYear.subregion.subindicator.worm,
                   .f = ~MMP__worm_alt_2023(
                     alt.idx = .x %>%
                       filter(!(Subregion %in% c("Pascoe", "Stewart", "Normanby", "Endeavour") &
                                  reportCardYear < as.Date("2021-01-01"))) %>%
                       droplevels(),
                     subregion.worm = .y %>%
                       filter(!(Subregion %in% c("Pascoe", "Stewart", "Normanby", "Endeavour") &                                      reportCardYear < as.Date("2021-01-01"))) %>%
                       droplevels(),
                     minDate = minDate, MAXDATE = MAXDATE))
        ) %>%
        mutate(Index_plots =
                 ifelse(Subregion %in% c("Pascoe", "Stewart", "Normanby", "Endeavour"),
                   Index_alt_Plot2,
                   map2(.x = Index_hist_plot2,
                     .y = Index_alt_Plot2,
                     .f = ~ .x/.y)
                 )
        ) |> 
        ## mutate(Index_plots = map2(.x = Index_hist_plot2,
        ##                           .y = Index_alt_Plot2,
        ##                           .f = ~ .x/.y))
        mutate(Index_plots_new =
                 pmap(.l = list(Historic.idx.subregion,                  # Indicator indices for type 9 (e.g. points)
                   ## Alt1.subregion.subindicator,             # Measure/Subindicator for type 8 (e.g. lines)
                   Alt9.subregion.subindicator,             # Measure/Subindicator for type 9 (e.g. lines)
                   alt7.idx.subregion,                      # Indicator for type 7 (e.g. points)
                   ReneeYear.subregion.subindicator.worm),  # Measure/Subindicator for type 7 (e.g. lines)
                   .f =  ~ {
                     hist.idx <- ..1  
                     subregion.worm <- ..2 
                     alt.idx <- ..3 %>%
                       filter(!(Subregion %in%
                                  c("Pascoe", "Stewart", "Normanby", "Endeavour") &
                                  reportCardYear < as.Date("2021-01-01"))) %>%
                       droplevels()
                     subregion.worm.7 <- ..4 %>%
                       filter(!(Subregion %in%
                                  c("Pascoe", "Stewart", "Normanby", "Endeavour") &                                      reportCardYear < as.Date("2021-01-01"))) %>%
                       droplevels()
                     MMP__worm_alt_2026(
                       hist.idx = hist.idx,
                       subregion.worm = subregion.worm,
                       alt.idx =  alt.idx,
                       subregion.worm.7 = subregion.worm.7,
                       minDate = minDate,
                       MAXDATE = MAXDATE
                     )
                   }
                 )
        ) |> 
        mutate(IndexPlots.AIMS.JCU.OMO.2026.part1_file  = paste(FIGURE_OUTPUT_PATH, "gamm_", gsub(" ", "_", Subregion),
          ".AIMS_JCU_OMO_2026_summary_part1", "png")) 
      
      saveRDS(gp.aims.jcu.omo_tbl |> dplyr::select(Subregion, IndexPlots.AIMS.JCU.OMO.2026.part1_file),
        file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_7a.rds"))

      pwalk(list(Subregion = gp.aims.jcu.omo_tbl$Subregion,
        IndexPlots.AIMS.JCU.OMO.2026.part1 = gp.aims.jcu.omo_tbl$Index_plots_new,
        Cnt = 1:length(gp.aims.jcu.omo_tbl$Subregion)),
        .f = function(Subregion, IndexPlots.AIMS.JCU.OMO.2026.part1, Cnt) { 
          MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
            fig_name_suffix = ".AIMS_JCU_OMO_2026_summary_part1",
            Plot = IndexPlots.AIMS.JCU.OMO.2026.part1,
            fig.width=6,
            ## fig.height=ifelse(Subregion %in% c("Pascoe", "Stewart", "Normanby", "Endeavour"),3,6),
            fig.height = 3,
            pt.size = 12) 
        }
      )          
    }

    MMP_add_to_report_list(CURRENT_STAGE, "compilations",
      SUBSECTION_17 = structure(paste0("## Hist (Alt 9) vs New (Alt 7) (AIMS & JCU, OMO, 2026 part 1)\n"),
        parent = 'TABSET'),
      TABSET_17 = structure(paste0("\n:::: panel-tabset\n"),
        parent = 'SUBSECTION_17'),
      TABSET_17_END = structure(paste0("\n:::: \n"),
        parent = 'SUBSECTION_17')
    )

    gp.aims.jcu.omo_tbla <- readRDS(file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl_7a.rds"))
    pwalk(list(Subregion = gp.aims.jcu.omo_tbla$Subregion,
      Cnt = 1:length(gp.aims.jcu.omo_tbla$Subregion)),
      .f = function(Subregion, Cnt) { 
        MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
          fig_name_suffix = ".AIMS_JCU_OMO_2026_summary_part1",
          label_suffix="_Hist_Alt7_6_1b",
          Cnt, tabset_parent = "TABSET_17",
          fig.caption = paste0("\nTemporal trends in water quality index with individual coloured subindicators for the ",Subregion," subregion. Water quality indices calculated via 2015 sampling design (circular symbols) and full data ingest methodologies (AIMS + JCU, Open Coasta/Midshelf/Offset only) and hierarchical formulation (diamond symbols).\n")) 
      }
    )          
    
  },
  LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt7 (separate worms, AIMS & JCU, OMO 2026).', return=TRUE)
  ## ----end

   if (1 == 2) {
    ## Remove all the ggplot objects as they are huge!
  gp_tbl <- gp_tbl |>
    dplyr::select(Subregion, Plot1_file)
    saveRDS(gp_tbl, file = paste0(GAM_OUTPUT_PATH, "gp_tbl.rds"))

  gp.aims.jcu.omo_tbl <- gp.aims.jcu.omo_tbl |>
    dplyr::select(Subregion,
      Plot1_AIMS.JCU.OMO_file,
      Plot2_AIMS.JCU.OMO_file,
      Plot3a_AIMS.JCU.OMO_file,
      Plot3b_AIMS.JCU.OMO_file,
      Plot4_AIMS.JCU.OMO_file,
      IndexPlots.AIMS.JCU.OMO.2023.part1_file,
      GAM_plots_2023_file,
      IndexPlots.AIMS.JCU.OMO.2026.part1_file
      )
    saveRDS(gp.aims.jcu.omo_tbl, file = paste0(GAM_OUTPUT_PATH, "gp.aims.jcu.omo_tbl.rds"))

    MMP_checkData(name = "gp.aims.jcu.omo_tbl.rds",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.prefix = "Processed",
                  PATH = GAM_OUTPUT_PATH,
                  progressive = FALSE)
    MMP_openning_banner()
   }
}

## if (length(list.files("../data/reports", pattern = "STAGE6", full.names = TRUE)) == 0) {
##   if (length(list.files("../data/reports/backup", pattern = "STAGE6", full.names = TRUE)) != 0) {
##     report_files <- list.files("../data/reports/backup", pattern = "STAGE6", full.names = TRUE)
##     fname <- basename(report_files)
##     for (f in 1:length(fname))
##       file.copy(from = report_files[f], paste0("../data/reports/", fname[f]),
##         recursive = TRUE)
##     }
## } else {
##   report_files <- list.files("../data/reports", pattern = "STAGE6", full.names = TRUE)
##   fname <- basename(report_files)
##   for (f in 1:length(fname))
##     file.copy(from = report_files[f], paste0("../data/reports/backup/", fname[f]),
##       recursive = TRUE)
## }
