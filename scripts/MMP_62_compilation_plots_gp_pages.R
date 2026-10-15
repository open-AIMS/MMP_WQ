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
        wq_gp <- readRDS(file = paste0(GAM_OUTPUT_PATH, "wq_gp.rds"))
        wq.aims.jcu_gp <- readRDS(file = paste0(GAM_OUTPUT_PATH, "wq.aims.jcu_gp.rds"))
        wq.aims.jcu.omo_gp <- readRDS(file = paste0(GAM_OUTPUT_PATH, "wq.aims.jcu.omo_gp.rds"))
        wq.flntu_gp <- readRDS(file = paste0(GAM_OUTPUT_PATH, "wq.flntu_gp.rds"))
        ## load(file=paste0(GAM_OUTPUT_PATH, 'wq.gams.RData'))
        ## load(file=paste0(GAM_OUTPUT_PATH, 'wq.aims.jcu.gams.RData'))
        ## load(file=paste0(GAM_OUTPUT_PATH, 'wq.aims.jcu.omo.gams.RData'))
        load(file=paste0(DATA_PATH, '/primary/other/wq.units.RData'))
        
        load(file = paste0(INDICES_OUTPUT_PATH, 'wq.alt1.idx.subregion.subindicator.RData'))
        load(file = paste0(INDICES_OUTPUT_PATH, 'wq.alt7.idx.subregion.subindicator.RData'))
        load(file = paste0(INDICES_OUTPUT_PATH, 'wq.alt8.idx.subregion.subindicator.RData'))
        reneeYear.subregion.subindicator.worm <- wq.alt7.idx.subregion.subindicator
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GP pages):', msg='Reading in data.', return=TRUE)
    ## ----end

    ## 2. Hist vs Alt7
    ## ---- Hist vs Alt7
    MMP_tryCatch(
    {
        MMP_add_to_report_list(CURRENT_STAGE, "compilations",
                               SUBSECTION_0 = structure(paste0("## Hist vs Alt7\n"),
                                                        parent = 'TABSET'),
                               TABSET_0 = structure(paste0("\n:::: panel-tabset\n"),
                                                    parent = 'SUBSECTION_0'),
                               TABSET_0_END = structure(paste0("\n:::: \n"),
                                                        parent = 'SUBSECTION_0')
                               )

        gp_tbl <-
            ## data.frame(Subregion = unique(wq.gams$Subregion)) %>%
            data.frame(Subregion = unique(wq_gp$Subregion)) %>%
            group_by(Subregion) %>%
            nest() %>%
            mutate(Subregion = as.character(Subregion)) %>%
            mutate(Historic.idx.subregion = map(.x = Subregion,
                                                ## .f = ~ wq.historic.idx.subregion %>%
                                                .f = ~ wq.alt8.idx.subregion %>%
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
                                       .f = ~ MMP__gp_ntu_flntu(flntu = wq.flntu.gams,
                                                                 subr = .x))) %>%
            mutate(Plot1 = pmap(.l = list(Subregion, Index_Plot, AIMS_CHL_Plot, AIMS_NTU_Plot),
                                .f = ~ MMP__gp_plot1(..1, ..2, ..3, ..4, wq_gp)
                                ))

        pwalk(list(Subregion = gp_tbl$Subregion,
                   Plot1 = gp_tbl$Plot1,
                   Cnt = 1:length(gp_tbl$Subregion)),
              .f = function(Subregion, Plot1, Cnt) { 
                  MMP__comp_figure_export(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = "_summary",
                                          Plot = Plot1,
                                          fig.width=7, fig.height=9,
                                          pt.size = 10) 
                  MMP__comp_figure_quarto(FIGURE_OUTPUT_PATH, Subregion,
                                          fig_name_suffix = "_summary",
                                          label_suffix="_Hist_Alt7_",
                                          Cnt, tabset_parent = "TABSET_0",
                                          fig.caption = paste0("\nTemporal trends in a) water quality index as well as b-j) generalized additive models of various water quality measures for the ",Subregion," subregion. Water quality indices calculated via 2015 methodologies (full trend, circular symbols) and full data ingest methodologies (AIMS) and hierarchical formulation (diamond symbols).  Black dots represent depth weighted averages and red/blue lines represent smooth trends fitted by Generalized Additive Models (GAMMs).  Transparent ribbons represent estimate ±1 and ±2 SE.  Horizontal dashed line represents old Water Quality Guidelines.  Red and blue GAMM lines and confidence ribbons represent FLNTU and Niskin samples respectively.\n")) 
              }
              )          
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAM pages):', msg='Hist vs Alt7.', return=TRUE)
    ## ----end

  
}
