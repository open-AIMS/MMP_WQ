source("MMP_functions.R")
source("MMP_functions_indices.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}


MAXDATE <- as.Date(paste0(reportYear,'-09-30'))
MINDATE <- MAXDATE-years(1)+days(1)
NISKIN_INPUT_PATH <- paste0(DATA_PATH, "/processed/niskin/")
PARAMS_INPUT_PATH <- paste0(DATA_PATH, "/primary/other/")
FLNTU_INPUT_PATH <- paste0(DATA_PATH, "/processed/loggers/")
INDICES_OUTPUT_PATH <- paste0(DATA_PATH, "/indices/")
FIGURE_OUTPUT_PATH <- paste0(OUTPUT_PATH, "/figures/indices/")

## Type 6 index  
## ---- Index comparison
CURRENT_ITEM <- "Comp"
## mmp__add_status(stage = paste0("STAGE", CURRENT_STAGE),
##                 item = CURRENT_ITEM,
##                 name = "Comparison",
##                 status = "progress")
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()


if ((alwaysExtract | !file.exists(paste0(OUTPUT_PATH,"/figures/Plots4Renee.zip"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'wq.all.reef.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/wq.guidelines.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/wq.units.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/hierarchy.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, '/names_lookup.RData')) 
    ) {
    
    MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
                           SUBSECTION_9 = structure(paste0("## Comparisons\n"),
                                                    parent = 'TABSET'),
                           TABSET_9 = structure(paste0("\n:::: panel-tabset\n"),
                                                   parent = 'SUBSECTION_9'),
                           TABSET_9_END = structure(paste0("\n:::: \n"),
                                                       parent = 'SUBSECTION_9')
                           )

    ## 1. Regional worms
    ## ---- Regional worms
    MMP_tryCatch(
    {
        load(file=paste0(DATA_PATH, '/final/wq.historic.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt2.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt3.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt4.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt5.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt6.idx.region.RData'))

        wq.comb.region.g1<-
            ggplot(data = wq.historic.idx.region %>%
                       filter(Year>2007) %>%
                       mutate(Region = factor(Region,
                                              levels=c('Cape York','Wet Tropics',
                                                       'Burdekin','Mackay Whitsunday',
                                                       'Fitzroy'))),
                   aes(y = Index, x = reportCardYear)) +
            geom_hline(yintercept = 0, linetype = 'dashed') +
            geom_line() +
            geom_point(aes(fill = Grade, shape = '0', color = '0'),
                       size = 2) +
            geom_line(data = wq.alt2.idx.region %>%
                          filter(Year>2007)) +
            geom_point(data = wq.alt2.idx.region %>%
                           filter(Year>2007),
                       aes(fill = Grade, shape = '2', color = '2'),
                       size = 2) +
            geom_line(data = wq.alt3.idx.region) +
            geom_point(data = wq.alt3.idx.region,
                       aes(fill = Grade, shape = '3', color = '3'),
                       size = 2) +
            geom_line(data = wq.alt4.idx.region) +
            geom_point(data = wq.alt4.idx.region,
                       aes(fill = Grade, shape = '4', color = '4'),
                       size = 1.5) +
            geom_line(data = wq.alt5.idx.region) +
            geom_point(data = wq.alt5.idx.region,
                       aes(fill = Grade, shape = '5', color = '5'),
                       size = 1.5) +
            geom_line(data = wq.alt6.idx.region) +
            geom_point(data = wq.alt6.idx.region,
                       aes(fill = Grade, shape = '6', color = '6'),
                       size = 2.5) + 
            scale_y_continuous('Water Quality Index',
                               limits = c(-1,1)) +
            scale_x_date('', limits = c(as.Date('2006-01-01'),
                                        as.Date(paste0(reportYear,'-01-01')))) +
            scale_fill_manual('', breaks = c('A','B','C','D','E'),
                              values = rev(trafficLightPalette),
                              limits = c('A','B','C','D','E'),
                              labels = c('Very Good','Good','Moderate','Poor','Very Poor'),
                              guide = FALSE) +
            scale_shape_manual('Option', values = c(21,22,23,24,25,21)) +
            scale_color_manual('Option',
                               values = c('black','black','black','black','black','red')) +
            theme_mmp + theme(strip.background = element_blank(),
                              panel.margin.x = unit(1,'line'))

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_comb_region",
                               Plot = wq.comb.region.g1 + facet_grid(~Region,as.table=FALSE),
                               units = "in",
                               fig.width = 7, fig.height = 2, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional Index", fig_name_suffix = "wq_comb_region",
                           label_suffix = "_9_idx1", tabset_parent = "TABSET_9",
                           fig.caption = paste0("\nComparison of temporal trends in the water quality index for the historic and alt 6 formulations conditional on Region.\n")) 

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_comb_region1",
                               Plot = wq.comb.region.g1 + facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'),
                               units = "in",
                               fig.width = 7, fig.height = 2, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional Index alt.", fig_name_suffix = "wq_comb_region1",
                           label_suffix = "_9_idx1a", tabset_parent = "TABSET_9",
                           fig.caption = paste0("\nComparison of temporal trends in the water quality index for the historic and alt 6 formulations conditional on Region.\n")) 

        ## pdf(file=paste0(FIGURE_OUTPUT_PATH, 'wq_comb_region.pdf'), width=7, height=2)
        ## print(wq.comb.region.g1 + facet_grid(~Region,as.table=FALSE))
        ## dev.off()

        ## pdf(file=paste0(FIGURE_OUTPUT_PATH, 'wq_comb_region1.pdf'), width=7, height=2)
        ## print(wq.comb.region.g1 + facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y')) 
        ## dev.off()

        ## png(file=paste0(FIGURE_OUTPUT_PATH, 'wq_comb_region.png'), width=7, height=2,
        ##     units='in',res=100)
        ## print(wq.comb.region.g1 + facet_grid(~Region,as.table=FALSE))
        ## dev.off()
        ## png(file=paste0(FIGURE_OUTPUT_PATH, 'wq_comb_region_large.png'), width=7, height=2,
        ##     units='in',res=600)
        ## print(wq.comb.region.g1 + facet_grid(~Region,as.table=FALSE))
        ## dev.off()

        ## png(file=paste0(FIGURE_OUTPUT_PATH, 'wq_comb_region1.png'), width=7, height=2,
        ##     units='in',res=100)
        ## print(wq.comb.region.g1 + facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y')) 
        ## dev.off()
        ## png(file=paste0(FIGURE_OUTPUT_PATH, 'wq_comb_region1_large.png'), width=7, height=2,
        ##     units='in',res=600)
        ## print(wq.comb.region.g1 + facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y')) 
        ## dev.off()

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_9_idx1 = structure(paste0("### Regional Index\n"),
        ##                                                      parent = 'TABSET_9'),
        ##                        FIG_REF_9_idx1 = structure(paste0("\n::::: {#fig-9-idx1}\n"),
        ##                                            parent = 'SUBSECTION_9_idx1'),
        ##                        FIG_9_qaqc1 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_comb_region.png)\n"),
        ##                                        parent = "FIG_REF_9_idx1"),
        ##                        FIG_CAP_9_idx1 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n"),
        ##                                            parent = 'FIG_REF_9_idx1'),
        ##                        FIG_REF_9_idx1_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_9_idx1'),

        ##                        FIG_REF_9_idx1b = structure(paste0("\n::::: {#fig-9-idx1b}\n"),
        ##                                            parent = 'SUBSECTION_9_idx1'),
        ##                        FIG_9_qaqc1b = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_comb_region1.png)\n"),
        ##                                        parent = "FIG_REF_9_idx1b"),
        ##                        FIG_CAP_9_idx1b = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n"),
        ##                                            parent = 'FIG_REF_9_idx1b'),
        ##                        FIG_REF_9_idx1b_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_9_idx1')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Comparing indices (region level).', return=TRUE)
    ## ----end

    ## 2. Subregional worms
    ## ---- Subregional worms
    MMP_tryCatch(
    {
        load(file=paste0(DATA_PATH, '/final/wq.historic.idx.subregion.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt2.idx.subregion.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt3.idx.subregion.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt4.idx.subregion.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt5.idx.subregion.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt6.idx.subregion.RData'))

        wq.comb.subregion.g1 <-
            ggplot(wq.historic.idx.subregion %>%
                   filter(Year>2007) %>%
                   mutate(Subregion = factor(Subregion,
                                             levels = levels(wq.alt6.idx.subregion$Subregion))),
                   aes(y = Index, x = reportCardYear)) +
            geom_hline(yintercept = 0, linetype = 'dashed') +
            geom_line() +
            geom_point(aes(fill = Grade, shape = '0', color = "0"), size = 2) +
            geom_line(data = wq.alt2.idx.subregion %>%
                          filter(Year>2007)) +
            geom_point(data = wq.alt2.idx.subregion %>%
                           filter(Year>2007),
                       aes(fill = Grade, shape = '2', color = '2'), size = 2) +
            geom_line(data = wq.alt3.idx.subregion) +
            geom_point(data = wq.alt3.idx.subregion,
                       aes(fill = Grade, shape = '3', color = '3'), size = 2) +
            geom_line(data = wq.alt4.idx.subregion) +
            geom_point(data = wq.alt4.idx.subregion,
                       aes(fill = Grade, shape = '4', color = '4'), size = 1.5) +
            geom_line(data = wq.alt5.idx.subregion) +
            geom_point(data = wq.alt5.idx.subregion,
                       aes(fill = Grade, shape = '5', color = '5'), size = 1.5) + 
            geom_line(data = wq.alt6.idx.subregion) +
            geom_point(data = wq.alt6.idx.subregion,
                       aes(fill = Grade, shape = '6', color = '6'), size = 2.5) + 
            scale_y_continuous('Water Quality Index', limits = c(-1,1)) +
            scale_x_date('', limits = c(as.Date('2006-01-01'),
                                        as.Date(paste0(reportYear,'-01-01')))) +
            scale_fill_manual('', breaks = c('A','B','C','D','E'),
                              values = rev(trafficLightPalette),
                              limits = c('A','B','C','D','E'),
                              labels = c('Very Good','Good','Moderate','Poor','Very Poor'),
                              guide = FALSE) +
            scale_shape_manual('Option', values = c(21,22,23,24,25,21)) +
            scale_color_manual('Option', values = c('black','black','black','black','black','red')) +
            theme_mmp +
            theme(strip.background = element_blank(),
                  panel.margin.x = unit(1,'line'))


        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_comb_subregion",
                               Plot = wq.comb.subregion.g1 + facet_grid(~Subregion,as.table=FALSE),
                               units = "in",
                               fig.width = 18, fig.height = 2, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional Index", fig_name_suffix = "wq_comb_subregion",
                           label_suffix = "_9_idx2", tabset_parent = "TABSET_9",
                           fig.caption = paste0("\nComparison of temporal trends in the water quality index for the historic and alt 6 formulations conditional on Subregion.\n")) 

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_comb_subregion1",
                               Plot = wq.comb.subregion.g1 + facet_wrap(~Subregion,as.table=TRUE,nrow=3,scales='free_y'),
                               units = "in",
                               fig.width = 7, fig.height = 7, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional Index alt.", fig_name_suffix = "wq_comb_subregion1",
                           label_suffix = "_9_idx2a", tabset_parent = "TABSET_9",
                           fig.caption = paste0("\nComparison of temporal trends in the water quality index for the historic and alt 6 formulations conditional on Subregion.\n")) 

        ## pdf(file=paste0(FIGURE_OUTPUT_PATH, 'wq_comb_subregion.pdf'), width=18, height=2)
        ## print(wq.comb.subregion.g1 + facet_grid(~Subregion,as.table=FALSE))
        ## dev.off()
        ## png(file=paste0(FIGURE_OUTPUT_PATH, 'wq_comb_subregion.png'), width=18, height=2,
        ##     units = "in", res = 100)
        ## print(wq.comb.subregion.g1 + facet_grid(~Subregion,as.table=FALSE))
        ## dev.off()
        ## png(file=paste0(FIGURE_OUTPUT_PATH, 'wq_comb_subregion_large.png'), width=18, height=2,
        ##     units = "in", res = 600)
        ## print(wq.comb.subregion.g1 + facet_grid(~Subregion,as.table=FALSE))
        ## dev.off()

        ## pdf(file=paste0(FIGURE_OUTPUT_PATH, 'wq_comb_subregion1.pdf'), width=7, height=7)
        ## print(wq.comb.subregion.g1 + facet_wrap(~Subregion,as.table=FALSE,nrow=3,scales='free_y')) 
        ## dev.off()
        ## png(file=paste0(FIGURE_OUTPUT_PATH, 'wq_comb_subregion1.png'), width=7, height=7,
        ##     units = "in", res = 100)
        ## print(wq.comb.subregion.g1 + facet_wrap(~Subregion,as.table=FALSE,nrow=3,scales='free_y')) 
        ## dev.off()
        ## png(file=paste0(FIGURE_OUTPUT_PATH, 'wq_comb_subregion1_large.png'), width=7, height=7,
        ##     units = "in", res = 600)
        ## print(wq.comb.subregion.g1 + facet_wrap(~Subregion,as.table=FALSE,nrow=3,scales='free_y')) 
        ## dev.off()

        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_9_idx2 = structure(paste0("### Subregion Index\n"),
        ##                                                      parent = 'TABSET_9'),
        ##                        FIG_REF_9_idx2 = structure(paste0("\n::::: {#fig-9-idx2}\n"),
        ##                                            parent = 'SUBSECTION_9_idx2'),
        ##                        FIG_9_qaqc2 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_comb_subregion.png)\n"),
        ##                                        parent = "FIG_REF_9_idx2"),
        ##                        FIG_CAP_9_idx2 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n"),
        ##                                            parent = 'FIG_REF_9_idx2'),
        ##                        FIG_REF_9_idx2_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_9_idx2'),

        ##                        FIG_REF_9_idx2b = structure(paste0("\n::::: {#fig-9-idx2b}\n"),
        ##                                            parent = 'SUBSECTION_9_idx2'),
        ##                        FIG_9_qaqc2b = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_comb_subregion1.png)\n"),
        ##                                        parent = "FIG_REF_9_idx2b"),
        ##                        FIG_CAP_9_idx2b = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n"),
        ##                                            parent = 'FIG_REF_9_idx2b'),
        ##                        FIG_REF_9_idx2b_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_9_idx2')
        ##                        )

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Comparing indices (subregion level).', return=TRUE)
    ## ----end

    ## 3. For Angus
    ## ---- For Angus
    MMP_tryCatch(
    {
        load(file=paste0(DATA_PATH, '/indices/wq.historic.idx.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.historic.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.old.idx.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.old.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.alt5.idx.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt5.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt6.idx.region.RData'))


        CoralDateRanges <- data.frame(min = as.Date('2005-01-01'),
                                      max = as.Date(paste0(year(MAXDATE)+1,'-01-08')))

        dat <- wq.historic.idx.region %>%
            filter(Year>2007 & Year < as.numeric(as.character(reportYear))+1) %>%
            mutate(Region = factor(Region, levels = c('Wet Tropics','Burdekin',
                                                      'Mackay Whitsunday','Fitzroy')))

        g1 <- ggplot(dat, aes(y = Index, x = reportCardYear)) +
            ylim(-1,1)+
            xlim(CoralDateRanges$min, CoralDateRanges$max)+
            geom_hline(yintercept = 0, linetype = 'dashed') +
            geom_line() +
            geom_point(aes(fill = Grade, shape = '0'), size=3, show.legend = FALSE) +
            geom_line(data = wq.alt6.idx.region %>%
                          filter(Region != 'Cape York') %>%
                          droplevels) +
            geom_point(data = wq.alt6.idx.region %>%
                           filter(Region != 'Cape York') %>%
                           droplevels,aes(fill = Grade, shape = '6'),
                       size = 3, show.legend = FALSE) +
            geom_linerange(data = wq.alt6.idx.region %>%
                               filter(Region != 'Cape York') %>%
                               droplevels,
                           aes(ymin = Lower, ymax = Upper)) + 
            scale_y_continuous('Water Quality Index', limits = c(-1,1)) +
            scale_x_date('', limits = c(as.Date('2006-01-01'),
                                        as.Date(paste0(as.numeric(as.character(reportYear))+1,
                                                       '-01-01')))) +
            scale_fill_manual('', breaks = c('A','B','C','D','E'),
                              values = rev(trafficLightPalette),
                              limits = c('A','B','C','D','E'),
                              labels = c('Very Good','Good','Moderate',
                                         'Poor','Very Poor'),
                              guide = FALSE) +
            scale_shape_manual('Option', values = c(21,22,23,24,25)) +
            facet_wrap(~Region, scale = 'free', drop = FALSE, nrow = 1) +
            theme_classic(10)+
            theme(strip.background = element_blank(),
                  strip.text = element_text(),
                  axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.title.x = element_blank(),
                  plot.margin = unit(c(0.2,0.5,0.2,1), "lines"),
                  panel.margin = unit(c(1), "lines"),
                  axis.title.y = element_text(vjust = 1, size = rel(1.25)),
                  axis.line.x = element_line(),
                  axis.line.y = element_line()
          )

        g1=g1+xlim(CoralDateRanges$min, CoralDateRanges$max)
        g1=ggplot2::ggplot_gtable(ggplot_build(g1))
        g1 = cowplot::gtable_remove_grobs(g1, 'panel-4')
        g1 = cowplot::gtable_remove_grobs(g1, 'axis_b-4')
        g1 = cowplot::gtable_remove_grobs(g1, 'axis_l-4')
        g1 = cowplot::gtable_remove_grobs(g1, 'strip_t-4')
        
        wq.comb.region.4Corals = g1

        save(wq.comb.region.4Corals,
             file = paste0(FIGURE_OUTPUT_PATH, 'wq.comb.region.4Corals.RData'))
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Comparing indices (for Angus).', return=TRUE)
    ## ----end

    ## 3. Regional Index 
    ## ---- Regional Index
    MMP_tryCatch(
    {
        load(file=paste0(DATA_PATH, '/indices/wq.historic.idx.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.historic.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.old.idx.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.old.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.alt5.idx.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt5.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt6.idx.region.RData'))

        wq.alt6.idx.region_restrictedCY <- wq.alt6.idx.region %>%
            filter(!(Region == 'Cape York' & reportCardYear < as.Date("2021-01-01"))) %>%
            droplevels()

        wq.comb.region.g1 <- ggplot(wq.historic.idx.region %>%
                                    filter(Year>2007) %>%
                                    mutate(Region = factor(Region,
                                                           levels = levels(wq.alt6.idx.region$Region))),
                                    aes(y = Index, x = reportCardYear)) +
            geom_hline(yintercept = 0, linetype = 'dashed') +
            geom_line() +
            geom_point(aes(fill = Grade,shape = '0'),
                       size = 2,
                       show.legend = TRUE) +
            geom_line(data = wq.alt6.idx.region_restrictedCY##  %>%
                          ## filter(Region != 'Cape York')
                      ) +
            geom_point(data = wq.alt6.idx.region_restrictedCY##  %>%
                           ## filter(Region != 'Cape York')
                      ,
                       aes(fill = Grade, shape = '6'), size = 1.5,
                       show.legend = TRUE) +
            geom_linerange(data = wq.alt6.idx.region_restrictedCY##  %>%
                               ## filter(Region != 'Cape York')
                          ,
                           aes(ymin = Lower, ymax = Upper, fill = Grade,
                               shape = '6'),
                           show.legend = FALSE) + 
            scale_y_continuous('Water Quality Index', limits = c(-1,1)) +
            scale_x_date('', limits = c(as.Date('2006-01-01'),
                                        as.Date(paste0(as.numeric(as.character(reportYear)),
                                                       '-01-01')))) +
            scale_fill_manual('', breaks = c('A','B','C','D','E'),
                              values = rev(trafficLightPalette),
                              limits = c('A','B','C','D','E'),
                              labels = c('Very Good','Good','Moderate','Poor','Very Poor'),
                              guide = "coloursteps") +
          scale_shape_manual('', values = c(21,22,23,24,25),
                             labels = c('Long-term trend', 'Annual condition', '3', '4', '5', 'Annual condition')) +
            theme_mmp +
            theme(strip.background = element_blank(),
                  panel.margin.x = unit(1,'line'),
                  axis.title.x = element_blank(),
                  legend.position = "bottom",
                  legend.direction = "horizontal") +
            guides(shape = guide_legend(keyheight = 0.75,
                                        keywidth=0.5,
                                        override.aes = list(stroke=0.25)),
                   fill = guide_legend(override.aes = list(shape=21,
                                                           linewidth=1,
                                                           stroke=0.25),
                                       keywidth=0.5, keyheight=0.75))


        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_worms_comb_region",
                               Plot = wq.comb.region.g1 + facet_grid(~Region,as.table=FALSE),
                               units = "in",
                               fig.width = 7, fig.height = 2.25, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional Index", fig_name_suffix = "wq_worms_comb_region",
                           label_suffix = "_9_idx3", tabset_parent = "TABSET_9",
                           fig.caption = paste0("\nComparison of temporal trends in the water quality index for the historic and alt 6 formulations conditional on Subregion.\n")) 

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_worms_comb_region1",
                               Plot = wq.comb.region.g1 + facet_wrap(~Region,as.table=TRUE,nrow=1,scales='free_y'),
                               units = "in",
                               fig.width = 7, fig.height = 2.25, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional Index alt.", fig_name_suffix = "wq_worms_comb_region1",
                           label_suffix = "_9_idx3a", tabset_parent = "TABSET_9",
                           fig.caption = paste0("\nComparison of temporal trends in the water quality index for the historic and alt 6 formulations conditional on Subregion.\n")) 

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH,
        ##                   'wq_worms_comb_region.pdf'), width=7, height=2)
        ## print(wq.comb.region.g1 +
        ##       facet_grid(~Region,as.table=FALSE))
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH,
        ##                   'wq_worms_comb_region.png'), width=7, height=2,
        ##     units = "in", res = 100)
        ## print(wq.comb.region.g1 +
        ##       facet_grid(~Region,as.table=FALSE))
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH,
        ##                   'wq_worms_comb_region_large.png'), width=7, height=2,
        ##     units = "in", res = 600)
        ## print(wq.comb.region.g1 +
        ##       facet_grid(~Region,as.table=FALSE))
        ## dev.off()

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_worms_comb_region1.pdf'), width=7, height=2)
        ## print(wq.comb.region.g1 +
        ##       facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'))                       
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_worms_comb_region1.png'),
        ##     width=7, height=2, units = "in", res = 100)
        ## print(wq.comb.region.g1 +
        ##       facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'))                       
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_worms_comb_region1_large.png'),
        ##     width=7, height=2, units = "in", res = 600)
        ## print(wq.comb.region.g1 +
        ##       facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'))                       
        ## dev.off()

        
        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_9_idx3 = structure(paste0("### Region Index\n"),
        ##                                                      parent = 'TABSET_9'),
        ##                        FIG_REF_9_idx3 = structure(paste0("\n::::: {#fig-9-idx3}\n"),
        ##                                            parent = 'SUBSECTION_9_idx3'),
        ##                        FIG_9_qaqc3 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_worms_comb_region.png)\n"),
        ##                                        parent = "FIG_REF_9_idx3"),
        ##                        FIG_CAP_9_idx3 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n"),
        ##                                            parent = 'FIG_REF_9_idx3'),
        ##                        FIG_REF_9_idx3_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_9_idx3'),

        ##                        FIG_REF_9_idx3b = structure(paste0("\n::::: {#fig-9-idx3b}\n"),
        ##                                            parent = 'SUBSECTION_9_idx3'),
        ##                        FIG_9_qaqc3b = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_worms_comb_region1.png)\n"),
        ##                                        parent = "FIG_REF_9_idx3b"),
        ##                        FIG_CAP_9_idx3b = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n"),
        ##                                            parent = 'FIG_REF_9_idx3b'),
        ##                        FIG_REF_9_idx3b_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_9_idx3')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Comparing indices (Regional Index).', return=TRUE)
    ## ----end
    
    ## 3. No Cape York
    ## ---- No Cape York
    MMP_tryCatch(
    {
        load(file=paste0(DATA_PATH, '/indices/wq.historic.idx.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.historic.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.old.idx.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.old.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.alt5.idx.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt5.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt6.idx.region.RData'))

        wq.alt5.idx.region_noCY <- wq.alt5.idx.region %>%
            filter(Region != 'Cape York') %>%
            droplevels()

        wq.alt6.idx.region_noCY <- wq.alt6.idx.region %>%
            filter(Region != 'Cape York') %>%
            droplevels()
        
        wq.comb.region.g1 <- ggplot(wq.historic.idx.region %>%
                                    filter(Year>2007, Region != 'Cape York') %>%
                                    droplevels() %>%
                                    mutate(Region = factor(Region,
                                                           levels = levels(wq.alt5.idx.region_noCY$Region))),
                                    aes(y = Index, x = reportCardYear)) +
            geom_hline(yintercept = 0, linetype = 'dashed') +
            geom_line() +
            geom_point(aes(fill = Grade, shape = '0'), size = 2, show.legend = FALSE) +
            geom_line(data = wq.alt6.idx.region_noCY) +
            geom_point(data = wq.alt6.idx.region_noCY,
                       aes(fill = Grade, shape = '6'), size = 1.5, show.legend = FALSE) +
            geom_linerange(data = wq.alt6.idx.region_noCY,
                           aes(ymin = Lower, ymax = Upper, fill = Grade,
                               shape = '6'),
                           show.legend = FALSE) + 
            scale_y_continuous('Water Quality Index',
                               limits = c(-1,1)) +
            scale_x_date('', limits = c(as.Date('2006-01-01'),
                                        as.Date(paste0(reportYear,'-01-01')))) +
            scale_fill_manual('', breaks = c('A','B','C','D','E'),
                              values = rev(trafficLightPalette),
                              limits = c('A','B','C','D','E'),
                              labels=c('Very Good','Good','Moderate','Poor','Very Poor'),
                              guide=FALSE) +
            scale_shape_manual('Option', values = c(21,22,23,24,25)) +
            theme_mmp +
            theme(strip.background = element_blank(),
                  panel.margin.x = unit(1,'line'))

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_worms_comb_region_NoCapeYork",
                               Plot = wq.comb.region.g1 + facet_grid(~Region,as.table=FALSE),
                               units = "in",
                               fig.width = 7, fig.height = 2, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional Index (No CY)", fig_name_suffix = "wq_worms_comb_region_NoCapeYork",
                           label_suffix = "_9_idx4", tabset_parent = "TABSET_9",
                           fig.caption = paste0("\nComparison of temporal trends in the water quality index for the historic and alt 6 formulations conditional on Subregion.\n")) 

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_worms_comb_region1_NoCapeYork",
                               Plot = wq.comb.region.g1 + facet_wrap(~Region,as.table=TRUE,nrow=1,scales='free_y'),
                               units = "in",
                               fig.width = 7, fig.height = 2, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional Index (No CY) alt.", fig_name_suffix = "wq_worms_comb_region1_NoCapeYork",
                           label_suffix = "_9_idx4a", tabset_parent = "TABSET_9",
                           fig.caption = paste0("\nComparison of temporal trends in the water quality index for the historic and alt 6 formulations conditional on Subregion.\n")) 

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH,
        ##                   'wq_worms_comb_region_NoCapeYork.pdf'), width=7, height=2)
        ## print(wq.comb.region.g1 +
        ##       facet_grid(~Region,as.table=FALSE))
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH,
        ##                   'wq_worms_comb_region_NoCapeYork.png'), width=7, height=2,
        ##     units = "in", res = 100)
        ## print(wq.comb.region.g1 +
        ##       facet_grid(~Region,as.table=FALSE))
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH,
        ##                   'wq_worms_comb_region_NoCapeYork_large.png'), width=7, height=2,
        ##     units = "in", res = 600)
        ## print(wq.comb.region.g1 +
        ##       facet_grid(~Region,as.table=FALSE))
        ## dev.off()

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_worms_comb_region1_NoCapeYork.pdf'), width=7, height=2)
        ## print(wq.comb.region.g1 +
        ##       facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'))                       
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_worms_comb_region1_NoCapeYork.png'),
        ##     width=7, height=2, units = "in", res = 100)
        ## print(wq.comb.region.g1 +
        ##       facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'))                       
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_worms_comb_region1_NoCapeYork_large.png'),
        ##     width=7, height=2, units = "in", res = 600)
        ## print(wq.comb.region.g1 +
        ##       facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'))                       
        ## dev.off()
        
        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_9_idx4 = structure(paste0("### Region Index (No CY)\n"),
        ##                                                      parent = 'TABSET_9'),
        ##                        FIG_REF_9_idx4 = structure(paste0("\n::::: {#fig-9-idx4}\n"),
        ##                                            parent = 'SUBSECTION_9_idx4'),
        ##                        FIG_9_qaqc3 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_worms_comb_region_NoCapeYork.png)\n"),
        ##                                        parent = "FIG_REF_9_idx4"),
        ##                        FIG_CAP_9_idx4 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n"),
        ##                                            parent = 'FIG_REF_9_idx4'),
        ##                        FIG_REF_9_idx4_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_9_idx4'),

        ##                        FIG_REF_9_idx4b = structure(paste0("\n::::: {#fig-9-idx4b}\n"),
        ##                                            parent = 'SUBSECTION_9_idx4'),
        ##                        FIG_9_qaqc3b = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_worms_comb_region1_NoCapeYork.png)\n"),
        ##                                        parent = "FIG_REF_9_idx4b"),
        ##                        FIG_CAP_9_idx4b = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n"),
        ##                                            parent = 'FIG_REF_9_idx4b'),
        ##                        FIG_REF_9_idx4b_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_9_idx4')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Comparing indices (No Cape York or Fitzroy).', return=TRUE)
    ## ----end
    
    ## 4. No Fitzroy 
    ## ---- No Fitzroy
    ## 2021 - Barbara would like a version that does not have Fitzroy but does have Cape York
    MMP_tryCatch(
    {
        load(file=paste0(DATA_PATH, '/indices/wq.historic.idx.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.historic.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.old.idx.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.old.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.alt5.idx.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt5.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt6.idx.region.RData'))

        wq.alt6.idx.region_noF <- wq.alt6.idx.region %>%
            filter(Region != 'Fitzroy') %>%
            droplevels()
        
        wq.comb.region.g1 <- ggplot(wq.historic.idx.region %>%
                                    filter(Year>2007, Region != 'Fitzroy') %>%
                                    droplevels() %>%
                                    mutate(Region = factor(Region,
                                                           ## levels = levels(wq.alt6.idx.region_noCY$Region))),
                                                           levels = levels(wq.alt6.idx.region_noF$Region))),
                                    aes(y = Index, x = reportCardYear)) +
            geom_hline(yintercept = 0, linetype = 'dashed') +
            geom_line() +
            geom_point(aes(fill = Grade, shape = '0'), size = 2, show.legend = TRUE) +
            geom_line(data = wq.alt6.idx.region_noF) +
            geom_point(data = wq.alt6.idx.region_noF,
                       aes(fill = Grade, shape = '6'), size = 1.5, show.legend = TRUE) +
            geom_linerange(data = wq.alt6.idx.region_noF,
                           aes(ymin = Lower, ymax = Upper, fill = Grade,
                               shape = '6'),
                           show.legend = FALSE) + 
            scale_y_continuous('Water Quality Index',
                               limits = c(-1,1)) +
            scale_x_date('', limits = c(as.Date('2006-01-01'),
                                        as.Date(paste0(reportYear,'-01-01')))) +
            scale_fill_manual('', breaks = c('A','B','C','D','E'),
                              values = rev(trafficLightPalette),
                              limits = c('A','B','C','D','E'),
                              labels=c('Very Good','Good','Moderate','Poor','Very Poor'),
                              guide=FALSE) +
            scale_shape_manual('', values = c(21,22),
                               labels = c('Long-term\ntrend', 'Annual\ncondition')) + 
            theme_mmp +
            theme(strip.background = element_blank(),
                  panel.margin.x = unit(1,'line'),
                  legend.spacing.y = unit(0.1, 'pt'),
                  legend.margin = margin(c(0,0,0,0), 'pt')
                  ) +
            guides(shape = guide_legend(keyheight = 1.5,
                                        keywidth=0.5,
                                        override.aes = list(stroke=0.25)),
                   fill = guide_legend(override.aes = list(shape=21,
                                                           linewidth=1,
                                                           stroke=0.25),
                                       keywidth=0.5, keyheight=0.75))

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_worms_comb_region_NoFitzroy",
                               Plot = wq.comb.region.g1 + facet_grid(~Region,as.table=FALSE),
                               units = "in",
                               fig.width = 7, fig.height = 2, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional Index (No Ftz)", fig_name_suffix = "wq_worms_comb_region_NoFitzroy",
                           label_suffix = "_9_idx5", tabset_parent = "TABSET_9",
                           fig.caption = paste0("\nComparison of temporal trends in the water quality index for the historic and alt 6 formulations conditional on Subregion.\n")) 

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_worms_comb_region1_NoFitzroy",
                               Plot = wq.comb.region.g1 + facet_wrap(~Region,as.table=TRUE,nrow=1,scales='free_y'),
                               units = "in",
                               fig.width = 7, fig.height = 2, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Regional Index (No Ftz) alt.", fig_name_suffix = "wq_worms_comb_region1_NoFitzroy",
                           label_suffix = "_9_idx5a", tabset_parent = "TABSET_9",
                           fig.caption = paste0("\nComparison of temporal trends in the water quality index for the historic and alt 6 formulations conditional on Subregion.\n")) 

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH,
        ##                   'wq_worms_comb_region_NoFitzroy.pdf'), width=7, height=2)
        ## print(wq.comb.region.g1 +
        ##       facet_grid(~Region,as.table=FALSE))
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH,
        ##                   'wq_worms_comb_region_NoFitzroy.png'), width=7, height=2,
        ##     units = "in", res = 100)
        ## print(wq.comb.region.g1 +
        ##       facet_grid(~Region,as.table=FALSE))
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH,
        ##                   'wq_worms_comb_region_NoFitzroy_large.png'), width=7, height=2,
        ##     units = "in", res = 600)
        ## print(wq.comb.region.g1 +
        ##       facet_grid(~Region,as.table=FALSE))
        ## dev.off()

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_worms_comb_region1_NoFitzroy.pdf'), width=7, height=2)
        ## print(wq.comb.region.g1 +
        ##       facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'))                       
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_worms_comb_region1_NoFitzroy.png'),
        ##     width=7, height=2, units = "in", res = 100)
        ## print(wq.comb.region.g1 +
        ##       facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'))                       
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_worms_comb_region1_NoFitzroy_large.png'),
        ##     width=7, height=2, units = "in", res = 600)
        ## print(wq.comb.region.g1 +
        ##       facet_wrap(~Region,as.table=FALSE,nrow=1,scales='free_y'))                       
        ## dev.off()
        
        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_9_idx5 = structure(paste0("### Region Index (No Ftz)\n"),
        ##                                                      parent = 'TABSET_9'),
        ##                        FIG_REF_9_idx5 = structure(paste0("\n::::: {#fig-9-idx5}\n"),
        ##                                            parent = 'SUBSECTION_9_idx5'),
        ##                        FIG_9_qaqc3 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_worms_comb_region_NoFitzroy.png)\n"),
        ##                                        parent = "FIG_REF_9_idx5"),
        ##                        FIG_CAP_9_idx5 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n"),
        ##                                            parent = 'FIG_REF_9_idx5'),
        ##                        FIG_REF_9_idx5_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_9_idx5'),

        ##                        FIG_REF_9_idx5b = structure(paste0("\n::::: {#fig-9-idx5b}\n"),
        ##                                            parent = 'SUBSECTION_9_idx5'),
        ##                        FIG_9_qaqc3b = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_worms_comb_region1_NoFitzroy.png)\n"),
        ##                                        parent = "FIG_REF_9_idx5b"),
        ##                        FIG_CAP_9_idx5b = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n"),
        ##                                            parent = 'FIG_REF_9_idx5b'),
        ##                        FIG_REF_9_idx5b_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_9_idx5')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Comparing indices (No Fitzroy).', return=TRUE)
    ## ----end

    ## 5. Subregions 
    ## ---- Subregions
    MMP_tryCatch(
    {
        load(file=paste0(DATA_PATH, '/indices/wq.historic.idx.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.historic.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.old.idx.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.old.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.alt5.idx.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt5.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt6.idx.region.RData'))

        wq.comb.subregion.g1 <-
            ggplot(wq.historic.idx.subregion %>%
                   filter(Year>2007) %>%
                   mutate(Subregion =
                              factor(Subregion,
                                     levels = levels(wq.alt6.idx.subregion$Subregion))),
                   aes(y = Index, x = reportCardYear)) +
            geom_hline(yintercept = 0, linetype = 'dashed') +
            geom_line() +
            geom_point(aes(fill = Grade, shape = '0'), size = 2) +
            geom_line(data = wq.alt6.idx.subregion %>%
                          filter(Region != 'Cape York')) +
            geom_point(data = wq.alt6.idx.subregion %>%
                           filter(Region != 'Cape York'),
                       aes(fill = Grade, shape = '5'), size = 1.5) + 
            geom_linerange(data = wq.alt6.idx.subregion %>%
                               filter(Region != 'Cape York'),
                           aes(ymin = Lower, ymax = Upper, fill = Grade, shape = '6'),
                           show.legend = FALSE) + 
            scale_y_continuous('Water Qualtity Index',
                               limits = c(-1,1)) +
            scale_x_date('', limits = c(as.Date('2006-01-01'),
                                        as.Date(paste0(reportYear,'-01-01')))) +
            scale_fill_manual('', breaks = c('A','B','C','D','E'),
                              values = rev(trafficLightPalette),
                              limits = c('A','B','C','D','E'),
                              labels = c('Very Good','Good','Moderate','Poor','Very Poor'),
                              guide = FALSE) +
            scale_shape_manual('Option', values = c(21,22,23,24,25), guide = FALSE) +
            theme_mmp +
            theme(strip.background = element_blank(),
                  panel.margin.x = unit(1, 'line'))

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_comb_subregion",
                               Plot = wq.comb.subregion.g1 + facet_grid(~Subregion,as.table=FALSE),
                               units = "in",
                               fig.width = 7, fig.height = 2, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional Index", fig_name_suffix = "wq_worms_comb_subregion",
                           label_suffix = "_9_idx6", tabset_parent = "TABSET_9",
                           fig.caption = paste0("\nComparison of temporal trends in the water quality index for the historic and alt 6 formulations conditional on Subregion.\n")) 

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_comb_subregion1",
                               Plot = wq.comb.subregion.g1 + facet_wrap(~Subregion,as.table=FALSE,nrow=1,scales='free_y'),
                               units = "in",
                               fig.width = 7, fig.height = 2, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional Index alt.", fig_name_suffix = "wq_worms_comb_subregion1",
                           label_suffix = "_9_idx6a", tabset_parent = "TABSET_9",
                           fig.caption = paste0("\nComparison of temporal trends in the water quality index for the historic and alt 6 formulations conditional on Subregion.\n")) 

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH,
        ##                   'wq_worms_comb_subregion.pdf'), width=15, height=2)
        ## print(wq.comb.subregion.g1 +
        ##       facet_grid(~Subregion,as.table=FALSE))
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH,
        ##                   'wq_worms_comb_subregion.png'), width=15, height=2,
        ##     units = "in", res = 100)
        ## print(wq.comb.subregion.g1 +
        ##       facet_grid(~Subregion,as.table=FALSE))
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH,
        ##                   'wq_worms_comb_subregion_large.png'), width=15, height=2,
        ##     units = "in", res = 600)
        ## print(wq.comb.subregion.g1 +
        ##       facet_grid(~Subregion,as.table=FALSE))
        ## dev.off()

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_worms_comb_subregion1.pdf'), width=10, height=6)
        ## print(wq.comb.subregion.g1 +
        ##       facet_wrap(~Subregion,as.table=FALSE,nrow=3,scales='free_y'))                       
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_worms_comb_subregion1.png'),
        ##     width=10, height=6, units = "in", res = 100)
        ## print(wq.comb.subregion.g1 +
        ##       facet_wrap(~Subregion,as.table=FALSE,nrow=3,scales='free_y'))                       
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_worms_comb_subregion1_large.png'),
        ##     width=10, height=6, units = "in", res = 600)
        ## print(wq.comb.subregion.g1 +
        ##       facet_wrap(~Subregion,as.table=FALSE,nrow=3,scales='free_y'))                       
        ## dev.off()
        
        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_9_idx6 = structure(paste0("### Subregion Index\n"),
        ##                                                      parent = 'TABSET_9'),
        ##                        FIG_REF_9_idx6 = structure(paste0("\n::::: {#fig-9-idx6}\n"),
        ##                                            parent = 'SUBSECTION_9_idx6'),
        ##                        FIG_9_qaqc3 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_worms_comb_subregion.png)\n"),
        ##                                        parent = "FIG_REF_9_idx6"),
        ##                        FIG_CAP_9_idx6 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n"),
        ##                                            parent = 'FIG_REF_9_idx6'),
        ##                        FIG_REF_9_idx6_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_9_idx6'),

        ##                        FIG_REF_9_idx6b = structure(paste0("\n::::: {#fig-9-idx6b}\n"),
        ##                                            parent = 'SUBSECTION_9_idx6'),
        ##                        FIG_9_qaqc3b = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_worms_comb_subregion1.png)\n"),
        ##                                        parent = "FIG_REF_9_idx6b"),
        ##                        FIG_CAP_9_idx6b = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n"),
        ##                                            parent = 'FIG_REF_9_idx6b'),
        ##                        FIG_REF_9_idx6b_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_9_idx6')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Comparing indices (subregions).', return=TRUE)
    ## ----end
    
    ## 6. Subregions No Cape York or Fitzroy 
    ## ---- Subregions No Cape York or Fitzroy
    MMP_tryCatch(
    {
        load(file=paste0(DATA_PATH, '/indices/wq.historic.idx.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.historic.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.old.idx.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.old.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.alt5.idx.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt5.idx.region.RData'))
        load(file=paste0(DATA_PATH, '/final/wq.alt6.idx.region.RData'))

        wq.alt5.idx.subregion_noCY <- wq.alt5.idx.subregion %>%
            filter(!Subregion %in% c('Endeavour','Normanby', 'Pascoe', 'Stewart')) %>%
            droplevels()

        wq.alt6.idx.subregion_noCY <- wq.alt6.idx.subregion %>%
            filter(!Subregion %in% c('Endeavour','Normanby', 'Pascoe', 'Stewart')) %>%
            droplevels()

        wq.comb.subregion.g1 <-
            ggplot(wq.historic.idx.subregion %>%
                   filter(Year>2007,
                          !Subregion %in% c('Endeavour','Normanby',
                                            'Pascoe', 'Stewart')) %>%
                   droplevels() %>%
                   mutate(Subregion = factor(Subregion,
                                             levels = levels(wq.alt5.idx.subregion_noCY$Subregion))),
                   aes(y = Index, x = reportCardYear)) +
            geom_hline(yintercept = 0, linetype = 'dashed') +
            geom_line() +
            geom_point(aes(fill = Grade, shape = '0'), size = 2) +
            geom_line(data = wq.alt6.idx.subregion_noCY) +
            geom_point(data = wq.alt6.idx.subregion_noCY,
                       aes(fill = Grade, shape = '6'), size = 1.5) +
            geom_linerange(data = wq.alt6.idx.subregion_noCY,
                           aes(ymin = Lower, ymax = Upper, fill = Grade,
                               shape = '6'),
                           show.legend = FALSE) + 
            scale_y_continuous('Water Qualtity Index',
                               limits = c(-1,1)) +
            scale_x_date('', limits = c(as.Date('2006-01-01'),
                                        as.Date(paste0(reportYear,'-01-01')))) +
            scale_fill_manual('', breaks = c('A','B','C','D','E'),
                              values = rev(trafficLightPalette),
                              limits = c('A','B','C','D','E'),
                              labels = c('Very Good','Good','Moderate','Poor','Very Poor'),
                              guide = FALSE) +
            scale_shape_manual('Option', values = c(21,22,23,24,25), guide = FALSE) +
            theme_mmp +
            theme(strip.background = element_blank(),
                  panel.margin.x = unit(1,'line'))


        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_worms_comb_subregion_NoCapeYork",
                               Plot = wq.comb.subregion.g1 + facet_grid(~Subregion,as.table=FALSE),
                               units = "in",
                               fig.width = 15, fig.height = 2, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional Index (No CY)", fig_name_suffix = "wq_worms_comb_subregion_NoCapeYork",
                           label_suffix = "_9_idx7", tabset_parent = "TABSET_9",
                           fig.caption = paste0("\nComparison of temporal trends in the water quality index for the historic and alt 6 formulations conditional on Subregion.\n")) 

        MMP__figure_export_dev(FIGURE_OUTPUT_PATH, fig_name_suffix = "wq_worms_comb_subregion1_NoCapeYork",
                               Plot = wq.comb.subregion.g1 + facet_wrap(~Subregion,as.table=TRUE,nrow=3,scales='free_y'),
                               units = "in",
                               fig.width = 7, fig.height = 2, pt.size = 10)

        MMP__figure_quarto(CURRENT_STAGE, "calculate indices", FIGURE_OUTPUT_PATH,
                           Section = "Subregional Index (No CY) alt.", fig_name_suffix = "wq_worms_comb_subregion1_NoCapeYork",
                           label_suffix = "_9_idx7a", tabset_parent = "TABSET_9",
                           fig.caption = paste0("\nComparison of temporal trends in the water quality index for the historic and alt 6 formulations conditional on Subregion.\n")) 
        
        ## pdf(file = paste0(FIGURE_OUTPUT_PATH,
        ##                   'wq_worms_comb_subregion_NoCapeYork.pdf'), width=15, height=2)
        ## print(wq.comb.subregion.g1 +
        ##       facet_grid(~Subregion,as.table=FALSE))
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH,
        ##                   'wq_worms_comb_subregion_NoCapeYork.png'), width=15, height=2,
        ##     units = "in", res = 100)
        ## print(wq.comb.subregion.g1 +
        ##       facet_grid(~Subregion,as.table=FALSE))
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH,
        ##                   'wq_worms_comb_subregion_NoCapeYork_large.png'), width=15, height=2,
        ##     units = "in", res = 600)
        ## print(wq.comb.subregion.g1 +
        ##       facet_grid(~Subregion,as.table=FALSE))
        ## dev.off()

        ## pdf(file = paste0(FIGURE_OUTPUT_PATH, 'wq_worms_comb_subregion1_NoCapeYork.pdf'), width=10, height=6)
        ## print(wq.comb.subregion.g1 +
        ##       facet_wrap(~Subregion,as.table=FALSE,nrow=3,scales='free_y'))                       
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_worms_comb_subregion1_NoCapeYork.png'),
        ##     width=10, height=6, units = "in", res = 100)
        ## print(wq.comb.subregion.g1 +
        ##       facet_wrap(~Subregion,as.table=FALSE,nrow=3,scales='free_y'))                       
        ## dev.off()
        ## png(file = paste0(FIGURE_OUTPUT_PATH, 'wq_worms_comb_subregion1_NoCapeYork_large.png'),
        ##     width=10, height=6, units = "in", res = 600)
        ## print(wq.comb.subregion.g1 +
        ##       facet_wrap(~Subregion,as.table=FALSE,nrow=3,scales='free_y'))                       
        ## dev.off()
        
        ## MMP_add_to_report_list(CURRENT_STAGE, "calculate indices",
        ##                        SUBSECTION_9_idx7 = structure(paste0("### Subregion Index (No CY)\n"),
        ##                                                      parent = 'TABSET_9'),
        ##                        FIG_REF_9_idx7 = structure(paste0("\n::::: {#fig-9-idx7}\n"),
        ##                                            parent = 'SUBSECTION_9_idx7'),
        ##                        FIG_9_qaqc3 = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_worms_comb_subregion_NoCapeYork.png)\n"),
        ##                                        parent = "FIG_REF_9_idx7"),
        ##                        FIG_CAP_9_idx7 = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n"),
        ##                                            parent = 'FIG_REF_9_idx7'),
        ##                        FIG_REF_9_idx7_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_9_idx7'),

        ##                        FIG_REF_9_idx7b = structure(paste0("\n::::: {#fig-9-idx7b}\n"),
        ##                                            parent = 'SUBSECTION_9_idx7'),
        ##                        FIG_9_qaqc3b = structure(paste0("![](",FIGURE_OUTPUT_PATH,"wq_worms_comb_subregion1_NoCapeYork.png)\n"),
        ##                                        parent = "FIG_REF_9_idx7b"),
        ##                        FIG_CAP_9_idx7b = structure(paste0("\nTemporal trends in the water quality index conditional on Region for the historic (formulation 0) indices.\n"),
        ##                                            parent = 'FIG_REF_9_idx7b'),
        ##                        FIG_REF_9_idx7b_END = structure(paste0("\n::::: \n"),
        ##                                                parent = 'SUBSECTION_9_idx7')
        ##                        )
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Comparing indices (subregions, no Cape York).', return=TRUE)
    ## ----end

    ## 7. Zip figures 
    ## ---- Zip figures 
    MMP_tryCatch(
    {
        files <- paste0(FIGURE_OUTPUT_PATH,
                       c(
                           'wq_historic_idx_region.pdf',
                           'wq_historic_idx_region_large.png',
                           'wq_historic_idx_region1.pdf',
                           'wq_historic_idx_region1_large.png',
                           'wq_historic_idx_subregion.pdf',
                           'wq_historic_idx_subregion_large.png',
                           'wq_historic_idx_subregion1.pdf',
                           'wq_historic_idx_subregion1_large.png',
                           'wq_old_idx_region.pdf',
                           'wq_old_idx_region_large.png',
                           'wq_old_idx_region1.pdf',
                           'wq_old_idx_region1_large.png',
                           'wq_old_idx_subregion.pdf',
                           'wq_old_idx_subregion_large.png',
                           'wq_old_idx_subregion1.pdf',
                           'wq_old_idx_subregion1_large.png',
                           'wq_alt5_qaqc.pdf',
                           'wq_alt5_qaqc_large.png',
                           'wq_alt5_qaqc1.pdf',
                           'wq_alt5_qaqc1_large.png',
                           'wq_alt5_qaqc2.pdf',
                           'wq_alt5_qaqc2_large.png',
                           'wq_alt5_idx_region.pdf',
                           'wq_alt5_idx_region_large.png',
                           'wq_alt5_idx_region1.pdf',
                           'wq_alt5_idx_region1_large.png',
                           'wq_alt5_idx_subregion.pdf',
                           'wq_alt5_idx_subregion_large.png',
                           'wq_alt5_idx_subregion1.pdf',
                           'wq_alt5_idx_subregion1_large.png',
                           'wq_alt6_qaqc.pdf',
                           'wq_alt6_qaqc_large.png',
                           'wq_alt6_qaqc1.pdf',
                           'wq_alt6_qaqc1_large.png',
                           'wq_alt6_qaqc2.pdf',
                           'wq_alt6_qaqc2_large.png',
                           'wq_alt6_idx_site_measure.pdf',
                           'wq_alt6_idx_site_measure_large.png',
                           'wq_alt6_idx_subregion_measure.pdf',
                           'wq_alt6_idx_subregion_measure_large.png',
                           'wq_alt6_idx_subregion_measure_CI.pdf',
                           'wq_alt6_idx_subregion_measure_CI_large.png',
                           'wq_alt6_idx_region_measure.pdf',
                           'wq_alt6_idx_region_measure_large.png',
                           'wq_alt6_idx_region_measure_CI.pdf',
                           'wq_alt6_idx_region_measure_CI_large.png',
                           'wq_alt6_idx_subregion_subindicator.pdf',
                           'wq_alt6_idx_subregion_subindicator_large.png',
                           'wq_alt6_idx_subregion_subindicator_CI.pdf',
                           'wq_alt6_idx_subregion_subindicator_CI_large.png',
                           'wq_alt6_idx_subregion_indicator.pdf',
                           'wq_alt6_idx_subregion_indicator_large.png',
                           'wq_alt6_idx_subregion_indicator_CI.pdf',
                           'wq_alt6_idx_subregion_indicator_CI_large.png',
                           'wq_alt6_idx_region_indicator.pdf',
                           'wq_alt6_idx_region_indicator_large.png',
                           'wq_alt6_idx_region_indicator_CI.pdf',
                           'wq_alt6_idx_region_indicator_CI_large.png',
                           "wq_worms_comb_region.pdf",
                           "wq_worms_comb_region_large.png",
                           "wq_worms_comb_region_NoFitzroy.pdf",
                           "wq_worms_comb_region_NoFitzroy_large.png"
                       )
                       )
        files=paste(files, collapse =' ')
        system(paste0("zip -rjo '", OUTPUT_PATH, "/figures/Plots4Renee.zip' ",files))
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Indices:', msg='Comparing indices - zip figures.', return=TRUE)
    ## ----end

    
    MMP_checkData(name = "Plots4Renee.zip",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.prefix = "Processed",
                  PATH = paste0(OUTPUT_PATH, "/figures/"),
                  progressive = FALSE)
}
