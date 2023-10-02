source("MMP_functions.R")
source("MMP_functions_models.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

NISKIN_INPUT_PATH <- paste0(DATA_PATH, "/processed/niskin/")
PARAMS_INPUT_PATH <- paste0(DATA_PATH, "/primary/other/")
GAM_OUTPUT_PATH <- paste0(DATA_PATH, "/modelled/GAMMS/")
FIG_OUTPUT_PATH <- paste0(OUTPUT_PATH, "/figures/models/")

assign("CURRENT_STAGE", 7, env = globalenv())

## ---- AIMS niskin process
CURRENT_ITEM <- "Transects"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"wq.gams.RData"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'wq.all.reef.RData')) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'niskin.jcu.event.reef.av1.RData')) & 
    file.exists(paste0(PARAMS_INPUT_PATH, 'wq.sites.RData')) 
    ) {

    unlink(paste0(DATA_PATH, "/reports/STAGE",CURRENT_STAGE, "_", CURRENT_ITEM, "_.RData")) 
    MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                           SECTION = paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                                item = CURRENT_ITEM),"\n\n"),
                           TABSET = paste0("::: panel-tabset \n\n"),
                           TABSET_END = paste0("::: \n\n")
                           )
    ## 1. Read in data
    ## ---- Read in data
    MMP_tryCatch(
    {
        load(file=paste0(NISKIN_INPUT_PATH, 'wq.all.reef.RData'))
        load(file=paste0(NISKIN_INPUT_PATH, 'niskin.jcu.event.reef.av1.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.units.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData')) 
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))

        distances <- read.csv('../parameters/transect_plot_distances.csv', strip.white=TRUE)

        niskin.event.reef <- niskin.jcu.event.reef.av1 %>%
            dplyr::select(DRIFTCHL_UGPERL.wm,TSS_MGPERL.wm,
                          SECCHI_DEPTH.wm,PP.wm,PN.wm,NOx.wm,            #select only necessary info
                          SI.wm,POC.wm,DIN.wm,DOC.wm,DON.wm,DOP.wm,PO4.wm,  #add others of interest (although they dont have guidelines)
                          MMP_SITE_NAME,GBRMPA_group,Water_Samples,
                          GBRMPA_water_area,Region,Reg,Subregion,Subreg,
                          Season,oldSamplingYear,waterYear,reneeYear,
                          cwaterYear,financialYear,cfinancialYear) %>%
            mutate(Source = 'JCU Niskin Event') %>%
            gather(key=Measure, value=Value, DRIFTCHL_UGPERL.wm,TSS_MGPERL.wm,
                   SECCHI_DEPTH.wm,PP.wm,PN.wm,NOx.wm,
                   SI.wm,POC.wm,DIN.wm,DOC.wm,DON.wm,DOP.wm,PO4.wm)

        wq.all.reef <- wq.all.reef %>%
            bind_rows(niskin.event.reef) %>% 
            full_join(distances %>%
                      dplyr::rename(SHORT_NAME=GBRMPA_SITE_NAME) %>%
                      dplyr::select(-Subregion)) %>%
            full_join(wq.sites %>%
                      left_join(names_lookup) %>%
                      dplyr::select(MMP_SITE_NAME,Latitude,Longitude) %>%
                      distinct()) %>%
            left_join(wq.units %>%
                      dplyr::select(Measure,Name.graphs.abbr) %>%
                      distinct()) %>% 
            mutate(Distance = distance.from.mouth.km) %>%
            filter(!is.na(Value),
                   !is.na(Distance)) %>%
            droplevels()

        transect.tbl <- wq.all.reef %>%
            filter(Measure %in% c('DRIFTCHL_UGPERL.wm','NOx.wm',
                                  'PP.wm','PN.wm','TSS_MGPERL.wm',
                                  'SECCHI_DEPTH.wm','NTU')
                   ) %>%
            droplevels() %>%
            arrange(Measure, -Latitude) %>%
            mutate(Subregion = factor(Subregion, levels = unique(Subregion))) %>%
            group_by(Subregion, Measure) %>%
            summarise(data = list(cur_data_all()), .groups = "drop") 

        transect.tbl <- transect.tbl %>%
            mutate(Data = map(.x = data,
                              .f = ~ .x %>%
                                  filter(reneeYear == reportYear) %>%
                                  droplevels() %>%
                                  arrange(-Latitude)
                              ),
                   Data = map(.x = Data,
                              .f = ~ .x %>%
                                  mutate(Season = ifelse(Source == "JCU Niskin Event",
                                                         'Event', Season)) %>%
                                  mutate(Value = ifelse(Value == 0, Value[Value>0]/2, Value)) 
                              ),
                   wq.subs.sum = map(.x = Data,
                                     .f = ~ .x %>%
                                         group_by(Season, Distance, River, Subregion) %>%
                                         summarise(Value = mean(Value, rn.rm = TRUE))
                                     ),
                   newdata = map(.x = wq.subs.sum,
                                 .f = ~ with(.x,
                                             data.frame(Distance = seq(min(Distance),
                                                                       max(Distance),
                                                                       len = 100
                                                                       )))),
                   wq.subs.gams = map(.x = Data,
                                      .f = ~ .x %>%
                                          group_by(Season)%>%
                                          nest() %>%
                                          mutate(Distance=purrr::map(data, make_newdata)) %>%
                                          mutate(model = purrr::map(data, my_gam)) %>%
                                          mutate(Value = purrr::map2(model, Distance, pred_gam)) %>%
                                          dplyr::select(-data, -model) %>%
                                          unnest()
                                      )
                   )
        transect.tbl <- transect.tbl %>%
            mutate(
                wb = map(.x = Data,
                         .f = ~ .x %>%
                             filter(GBRMPA_water_area %in% c('Enclosed Coastal waters',
                                                             'Open Coastal waters',
                                                             'Midshelf waters',
                                                             'Offshore waters')) %>%
                             droplevels %>%
                             mutate(GBRMPA_water_area = factor(GBRMPA_water_area,
                                                               levels = c('Enclosed Coastal waters',
                                                                          'Open Coastal waters',
                                                                          'Midshelf waters',
                                                                          'Offshore waters'))) %>%
                             group_by(GBRMPA_water_area) %>%
                             summarise(min = min(Distance, na.rm = TRUE),
                                       Mx=max(Distance, na.rm = TRUE)) %>%
                             ungroup %>%
                             mutate(max = lead(min),
                                    max=pmax(Mx, max, na.rm = TRUE),
                                    min=ifelse(GBRMPA_water_area == first(GBRMPA_water_area),
                                               0, min),
                                    max=(Mx+max)/2, 
                                    Mn=lag(max),
                                    min=ifelse(is.na(Mn), min, Mn)
                                    ) %>%
                             dplyr::select(GBRMPA_water_area, min, max) %>%
                             mutate(max = ifelse(GBRMPA_water_area == last(GBRMPA_water_area),
                                                 max+(max)*0.02, max)) %>%
                             mutate(Label = ifelse(GBRMPA_water_area == 'Enclosed Coastal waters',
                                                   'EC',
                                            ifelse(GBRMPA_water_area == 'Open Coastal waters', 'OC',
                                            ifelse(GBRMPA_water_area == 'Midshelf waters',
                                                   'MS', 'OS'))))
                         )
            )
        transect.tbl <- transect.tbl %>%
            mutate(
                Plot = pmap(.l = list(Data, wb, wq.subs.gams),
                            .f = ~ {
                                ggplot(..1) +
                                    geom_rugRect(data=..2, aes(x=min,  xmin=min, xmax=max,
                                                               fill=GBRMPA_water_area),
                                                 sides='b', show.legend=FALSE) +
                                    geom_text(data=..2,  aes(x=(max+min)/2,label=Label),
                                              y=-Inf, vjust=-1) +
                                    scale_fill_manual(breaks=c('Enclosed Coastal waters',
                                                               'Open Coastal waters',
                                                               'Midshelf waters',
                                                               'Offshore waters'),
                                                      values=c('black', 'grey60', 'grey90', 'white'),
                                                      limits=c('Enclosed Coastal waters',
                                                               'Open Coastal waters',
                                                               'Midshelf waters',
                                                               'Offshore waters')) +
                                    geom_blank(aes(color='Wet', y=Value, x=Distance)) +
                                    geom_blank(aes(color='Dry', y=Value, x=Distance)) +
                                    geom_point(aes(color=factor(Season), y=Value, x=Distance),
                                               show.legend = FALSE) +
                                    geom_line(data=..3,aes(color=factor(Season),
                                                           y=Value, x=Distance),
                                              show.legend = FALSE) +
                                    scale_color_manual('', breaks=c('1', '2', 'Event'),
                                                       values=c('#fc8d62', '#8da0cb', '#66c2a5'),
                                                       labels=c('Ambient Dry', 'Ambient Wet', 'Event'),
                                                       limits=c('1', '2', 'Event'))+
                                        #geom_smooth(method='custom.smooth', se=FALSE) +
                                    labs(y=parse(text=as.character(unique(..1$Name.graphs.abbr))),
                                         x=paste0(paste0('Distance from mouth (km)'))) +
                                    scale_y_sqrt() +
                                    theme_classic() 
                                ## g
                            }
                            ) 
            )

        ## Make a legend
        wq.sub <- wq.all.reef %>%
            filter(reneeYear == reneeYear, Measure == 'DRIFTCHL_UGPERL.wm') %>%
            droplevels() %>% 
            arrange(-Latitude) %>%
            mutate(Subregion = factor(Subregion, levels=unique(Subregion))) %>%
            mutate(Season = ifelse(Source == 'JCU Niskin Event', 'Event', Season))
        g1 <-ggplot(wq.sub,
                    aes(y = Value, x = Distance, color = factor(Season))) +
            geom_point() +
            geom_smooth(se=FALSE) +
            scale_color_manual('', breaks=c('1', '2', 'Event'),
                               values=c('#fc8d62', '#8da0cb', '#66c2a5'),
                               labels=c('Ambient Dry', 'Ambient Wet', 'Event'))+
            theme_classic() +
            theme(legend.title=element_blank(), legend.margin=margin(0, 0, 0, 0))
        g_legend<-function(a.gplot){
            tmp <- ggplot_gtable(ggplot_build(a.gplot))
            leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
            legend <- tmp$grobs[[leg]]
            legend
        }
        lg <- g_legend(g1)
########

        transect.tbl.plots <- transect.tbl %>%
            dplyr::select(Subregion, Measure, Plot) %>%
            pivot_wider(id_cols = c(Subregion), names_from = Measure, values_from = Plot) %>%
            mutate(P = pmap(.l = list(DRIFTCHL_UGPERL.wm, NOx.wm, TSS_MGPERL.wm,
                                      SECCHI_DEPTH.wm, PP.wm, PN.wm),
                            .f = ~ (..1 +
                                    (..2 + patchwork::inset_element(lg, left=0.4, bottom=0.7, right=0.5, top=1))) /
                                (..3 + ..4) / (..5 + ..6)
                            ))

        ## transect.tbl.plots
        ## transect.tbl.plots[1,'P'][[1]][[1]]
        ## transect.tbl.plots[5,'P'][[1]][[1]]
        ## transect.tbl[1,'Plot'][[1]][[1]]
        pwalk(list(Subregion = transect.tbl.plots$Subregion,
                   Plot = transect.tbl.plots$P),
              .f = function(Subregion, Plot) {
                  ggsave(file = paste0(FIG_OUTPUT_PATH, "transect_", Subregion, ".png"),
                         Plot,
                         width = 8, height = 8, units = "in"
                         )
                  MMP__transect_figure_quarto(CURRENT_STAGE, "Transects", FIG_OUTPUT_PATH,
                                              Section = Subregion,
                                              fig_name_suffix = paste0("transect_", Subregion),
                                              label_suffix = paste0("_", Subregion),
                                              tabset_parent = "TABSET",
                                              fig.caption = paste0("\nVarious measures against distance (conditional on season) from major river(s) within the ",Subregion," Subregion. The Water bodies (EC: enclosed coastal, OC: open coastal, MS: midshelf, OS: offshore) associated with each sampling location are indicated via a shaded series of bars on the x-axis.\n")) 
              }
              )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Reading in data for old indices.', return=TRUE)
    ## ----end

    source("MMP_35_processedData_report.R")

    MMP_checkData(name = "wq.gams.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "fit GAMMs",
                  PATH = paste0(DATA_PATH, "/models/"))
    MMP_openning_banner()
}

