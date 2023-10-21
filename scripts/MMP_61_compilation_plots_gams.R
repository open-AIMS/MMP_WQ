source("MMP_functions.R")
source("MMP_functions_models.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}


GAM_OUTPUT_PATH <- paste0(DATA_PATH, "/models/")
PARAMS_INPUT_PATH <- paste0(DATA_PATH, "/primary/other/")

#####
## This script could be drastically reduced with the help of a function
## that takes Subregion and Measure and wq.gam/wq.aims.jcu.gams/etc
#####


## Type 0 index (Full old, historic spatial and temporal formulation) - wq.alt1
## ---- Type 0
CURRENT_ITEM <- "gams"
## mmp__add_status(stage = paste0("STAGE", CURRENT_STAGE),
##                 item = CURRENT_ITEM,
##                 name = "Type 0",
##                 status = "progress")
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"Plots4Angus"))) &
    file.exists(paste0(GAM_OUTPUT_PATH, 'wq.gams.RData')) &
    file.exists(paste0(DATA_PATH, '/indices/wq.alt6.idx.subregion.subindicator.RData')) &
    file.exists(paste0(PARAMS_INPUT_PATH, 'names_lookup.RData')) 
    ) {

    ## MMP_add_to_report_list(CURRENT_STAGE, "GAMS",
    ##                        SUBSECTION_0 = structure(paste0("## Type 0\n"),
    ##                                                 parent = 'TABSET'),
    ##                        TABSET_0 = structure(paste0("\n:::: panel-tabset\n"),
    ##                                                parent = 'SUBSECTION_0'),
    ##                        TABSET_0_END = structure(paste0("\n:::: \n"),
    ##                                                    parent = 'SUBSECTION_0')
    ##                        )

    ## 1. Read in data
    ## ---- Read in data
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.units.RData'))
        load(file=paste0(DATA_PATH, '/indices/wq.alt6.idx.subregion.subindicator.RData'))
        load(file = paste0(GAM_OUTPUT_PATH, "wq.gams.RData"))
        load(file = paste0(GAM_OUTPUT_PATH, "wq.flntu.gams.RData"))
        load(file = paste0(GAM_OUTPUT_PATH, "wq.aims.jcu.gams.RData"))
        load(file = paste0(GAM_OUTPUT_PATH, "wq.aims.jcu.omo.gams.RData"))

        reneeYear.subregion.subindicator.worm <- wq.alt6.idx.subregion.subindicator


    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAMS):', msg='Reading in data.', return=TRUE)
    ## ----end

    ## 2. Plots for Angus
    ## ---- Plots for Angus
    MMP_tryCatch(
    {
        ## Subregions <- c("Barron Daintree",
        ##                 "Johnstone Russell Mulgrave")
        ## Measures <- c("DRIFTCHL_UGPERL.wm",
        ##               "TSS_MGPERL.wm")
        ## gam_name_lookup <- tribble(
        ##     ~Name, ~Short,
        ##     "Barron Daintree", "Daintree",
        ##     "Johnstone Russell Mulgrave", "Johnstone",
        ##     "DRIFTCHL_UGPERL.wm", "chl",
        ##     )
        ## walk(.x = Subregions,
        ##      ~ walk(Measures,
        ##             function(.y) {
        ##                 Subr <- gam_name_lookup %>% filter(Name == .x) %>% pull(Short)
        ##                 Meas <- gam_name_lookup %>% filter(Name == .y) %>% pull(Short)
        ##                 p <- wq.gams %>% filter(Subregion == .x, Measure == .y) %>%
        ##                     droplevels() %>%
        ##                     pull(plot)
        ##                 nm <- paste0(Subr, "_", Meas, "_gam")
        ##                 assign(nm, p)
        ##                 save(list = nm,
        ##                      file = paste0(GAM_OUTPUT_PATH, nm, ".RData"))
        ##             }
        ##             )
        ##      )

        ## Barron Daintree
        Daintree_chl_gam <- wq.gams %>%
            MMP__gam_extract(subregion = "Barron Daintree",
                             measure = "DRIFTCHL_UGPERL.wm",
                             field = "plot") 
        save(Daintree_chl_gam, file = paste0(GAM_OUTPUT_PATH, "Daintree_chl_gam.RData"))

        Daintree_tss_gam <- wq.gams %>%
            MMP__gam_extract(subregion = "Barron Daintree",
                             measure = "TSS_MGPERL.wm", field = "plot")
        save(Daintree_tss_gam, file = paste0(GAM_OUTPUT_PATH, "Daintree_tss_gam.RData"))

        ## Johnstone Russell Mulgrave
        Johnstone_chl_gam <- wq.gams %>%
            MMP__gam_extract(subregion = "Johnstone Russell Mulgrave",
                             measure = "DRIFTCHL_UGPERL.wm", field = "plot") +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Johnstone Russell Mulgrave",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower2, ymax = upper2, x = Date),
                        fill = 'red', color = NA, alpha = 0.1) +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Johnstone Russell Mulgrave",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower, ymax = upper, x = Date),
                        fill = 'red', color = NA, alpha = 0.3) +
            geom_line(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Johnstone Russell Mulgrave",
                                              measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                      aes(y = pred, x = Date), color = 'red')

        save(Johnstone_chl_gam, file = paste0(GAM_OUTPUT_PATH, "Johnstone_chl_gam.RData"))

        Johnstone_tss_gam <- MMP__gam_extract(wq.flntu.gams,
                                              subregion = "Johnstone Russell Mulgrave",
                                              measure = "NTU", field = "plot")
        save(Johnstone_tss_gam, file = paste0(GAM_OUTPUT_PATH, "Johnstone_tss_gam.RData"))

        ## Tully Herbert
        Tully_chl_gam <- wq.gams %>%
            MMP__gam_extract(subregion = "Tully Herbert",
                             measure = "DRIFTCHL_UGPERL.wm", field = "plot") +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Tully Herbert",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower2, ymax = upper2, x = Date),
                        fill = 'red', color = NA, alpha = 0.1) +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Tully Herbert",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower, ymax = upper, x = Date),
                        fill = 'red', color = NA, alpha = 0.3) +
            geom_line(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Tully Herbert",
                                              measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                      aes(y = pred, x = Date), color = 'red')

        save(Tully_chl_gam, file = paste0(GAM_OUTPUT_PATH, "Tully_chl_gam.RData"))

        Tully_tss_gam <- MMP__gam_extract(wq.flntu.gams,
                                              subregion = "Tully Herbert",
                                              measure = "NTU", field = "plot")
        save(Tully_tss_gam, file = paste0(GAM_OUTPUT_PATH, "Tully_tss_gam.RData"))
            
        ## Burdekin
        Burdekin_chl_gam <- wq.gams %>%
            MMP__gam_extract(subregion = "Burdekin",
                             measure = "DRIFTCHL_UGPERL.wm", field = "plot") +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Burdekin",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower2, ymax = upper2, x = Date),
                        fill = 'red', color = NA, alpha = 0.1) +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Burdekin",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower, ymax = upper, x = Date),
                        fill = 'red', color = NA, alpha = 0.3) +
            geom_line(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Burdekin",
                                              measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                      aes(y = pred, x = Date), color = 'red')

        save(Burdekin_chl_gam, file = paste0(GAM_OUTPUT_PATH, "Burdekin_chl_gam.RData"))

        Burdekin_tss_gam <- MMP__gam_extract(wq.flntu.gams,
                                              subregion = "Burdekin",
                                              measure = "NTU", field = "plot")
        save(Burdekin_tss_gam, file = paste0(GAM_OUTPUT_PATH, "Burdekin_tss_gam.RData"))

        ## Mackay Whitsunday
        Mackay_chl_gam <- wq.gams %>%
            MMP__gam_extract(subregion = "Mackay Whitsunday",
                             measure = "DRIFTCHL_UGPERL.wm", field = "plot") +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Mackay Whitsunday",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower2, ymax = upper2, x = Date),
                        fill = 'red', color = NA, alpha = 0.1) +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Mackay Whitsunday",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower, ymax = upper, x = Date),
                        fill = 'red', color = NA, alpha = 0.3) +
            geom_line(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Mackay Whitsunday",
                                              measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                      aes(y = pred, x = Date), color = 'red')

        save(Mackay_chl_gam, file = paste0(GAM_OUTPUT_PATH, "Mackay_chl_gam.RData"))

        Mackay_tss_gam <- MMP__gam_extract(wq.flntu.gams,
                                              subregion = "Mackay Whitsunday",
                                              measure = "NTU", field = "plot")
        save(Mackay_tss_gam, file = paste0(GAM_OUTPUT_PATH, "Mackay_tss_gam.RData"))
        
        ## Fitzroy
        Fitzroy_chl_gam <- wq.gams %>%
            MMP__gam_extract(subregion = "Fitzroy",
                             measure = "DRIFTCHL_UGPERL.wm", field = "plot") +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Fitzroy",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower2, ymax = upper2, x = Date),
                        fill = 'red', color = NA, alpha = 0.1) +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Fitzroy",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower, ymax = upper, x = Date),
                        fill = 'red', color = NA, alpha = 0.3) +
            geom_line(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Fitzroy",
                                              measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                      aes(y = pred, x = Date), color = 'red')

        save(Fitzroy_chl_gam, file = paste0(GAM_OUTPUT_PATH, "Fitzroy_chl_gam.RData"))

        Fitzroy_tss_gam <- MMP__gam_extract(wq.flntu.gams,
                                              subregion = "Fitzroy",
                                              measure = "NTU", field = "plot")
        save(Fitzroy_tss_gam, file = paste0(GAM_OUTPUT_PATH, "Fitzroy_tss_gam.RData"))

        if(file.exists(paste0(GAM_OUTPUT_PATH, "Plots4Angus.zip")))
            file.remove(paste0(GAM_OUTPUT_PATH, "Plots4Angus.zip"))
        for (s in c('Daintree','Johnstone','Tully','Burdekin','Mackay','Fitzroy')) {
            for (m in c('chl','tss')) {
                system(paste0("zip -rjo '", GAM_OUTPUT_PATH, "Plots4Angus.zip' '",
                             GAM_OUTPUT_PATH, s, "_", m, "_gam.RData'"))
            }
        }

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAMS):', msg='Plots for Angus.', return=TRUE)
    ## ----end

    ## 3. Plots for Angus (AIMS + JCU data)
    ## ---- Plots for Angus
    MMP_tryCatch(
    {
        ## Barron Daintree
        Daintree_chl_gam.AIMS_JCU <- wq.aims.jcu.gams %>%
            MMP__gam_extract(subregion = "Barron Daintree",
                             measure = "DRIFTCHL_UGPERL.wm",
                             field = "plot") 
        save(Daintree_chl_gam.AIMS_JCU, file = paste0(GAM_OUTPUT_PATH, "Daintree_chl_gam.AIMS_JCU.RData"))

        Daintree_tss_gam.AIMS_JCU <- wq.aims.jcu.gams %>%
            MMP__gam_extract(subregion = "Barron Daintree",
                             measure = "TSS_MGPERL.wm", field = "plot")
        save(Daintree_tss_gam.AIMS_JCU, file = paste0(GAM_OUTPUT_PATH, "Daintree_tss_gam.AIMS_JCU.RData"))

        ## Johnstone Russell Mulgrave
        Johnstone_chl_gam.AIMS_JCU <- wq.aims.jcu.gams %>%
            MMP__gam_extract(subregion = "Johnstone Russell Mulgrave",
                             measure = "DRIFTCHL_UGPERL.wm", field = "plot") +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Johnstone Russell Mulgrave",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower2, ymax = upper2, x = Date),
                        fill = 'red', color = NA, alpha = 0.1) +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Johnstone Russell Mulgrave",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower, ymax = upper, x = Date),
                        fill = 'red', color = NA, alpha = 0.3) +
            geom_line(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Johnstone Russell Mulgrave",
                                              measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                      aes(y = pred, x = Date), color = 'red')

        save(Johnstone_chl_gam.AIMS_JCU, file = paste0(GAM_OUTPUT_PATH, "Johnstone_chl_gam.AIMS_JCU.RData"))

        Johnstone_tss_gam.AIMS_JCU <- MMP__gam_extract(wq.flntu.gams,
                                              subregion = "Johnstone Russell Mulgrave",
                                              measure = "NTU", field = "plot")
        save(Johnstone_tss_gam.AIMS_JCU, file = paste0(GAM_OUTPUT_PATH, "Johnstone_tss_gam.AIMS_JCU.RData"))

        ## Tully Herbert
        Tully_chl_gam.AIMS_JCU <- wq.aims.jcu.gams %>%
            MMP__gam_extract(subregion = "Tully Herbert",
                             measure = "DRIFTCHL_UGPERL.wm", field = "plot") +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Tully Herbert",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower2, ymax = upper2, x = Date),
                        fill = 'red', color = NA, alpha = 0.1) +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Tully Herbert",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower, ymax = upper, x = Date),
                        fill = 'red', color = NA, alpha = 0.3) +
            geom_line(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Tully Herbert",
                                              measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                      aes(y = pred, x = Date), color = 'red')

        save(Tully_chl_gam.AIMS_JCU, file = paste0(GAM_OUTPUT_PATH, "Tully_chl_gam.AIMS_JCU.RData"))

        Tully_tss_gam.AIMS_JCU <- MMP__gam_extract(wq.flntu.gams,
                                              subregion = "Tully Herbert",
                                              measure = "NTU", field = "plot")
        save(Tully_tss_gam.AIMS_JCU, file = paste0(GAM_OUTPUT_PATH, "Tully_tss_gam.AIMS_JCU.RData"))
            
        ## Burdekin
        Burdekin_chl_gam.AIMS_JCU <- wq.aims.jcu.gams %>%
            MMP__gam_extract(subregion = "Burdekin",
                             measure = "DRIFTCHL_UGPERL.wm", field = "plot") +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Burdekin",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower2, ymax = upper2, x = Date),
                        fill = 'red', color = NA, alpha = 0.1) +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Burdekin",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower, ymax = upper, x = Date),
                        fill = 'red', color = NA, alpha = 0.3) +
            geom_line(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Burdekin",
                                              measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                      aes(y = pred, x = Date), color = 'red')

        save(Burdekin_chl_gam.AIMS_JCU, file = paste0(GAM_OUTPUT_PATH, "Burdekin_chl_gam.AIMS_JCU.RData"))

        Burdekin_tss_gam.AIMS_JCU <- MMP__gam_extract(wq.flntu.gams,
                                              subregion = "Burdekin",
                                              measure = "NTU", field = "plot")
        save(Burdekin_tss_gam.AIMS_JCU, file = paste0(GAM_OUTPUT_PATH, "Burdekin_tss_gam.AIMS_JCU.RData"))

        ## Mackay Whitsunday
        Mackay_chl_gam.AIMS_JCU <- wq.aims.jcu.gams %>%
            MMP__gam_extract(subregion = "Mackay Whitsunday",
                             measure = "DRIFTCHL_UGPERL.wm", field = "plot") +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Mackay Whitsunday",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower2, ymax = upper2, x = Date),
                        fill = 'red', color = NA, alpha = 0.1) +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Mackay Whitsunday",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower, ymax = upper, x = Date),
                        fill = 'red', color = NA, alpha = 0.3) +
            geom_line(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Mackay Whitsunday",
                                              measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                      aes(y = pred, x = Date), color = 'red')

        save(Mackay_chl_gam.AIMS_JCU, file = paste0(GAM_OUTPUT_PATH, "Mackay_chl_gam.AIMS_JCU.RData"))

        Mackay_tss_gam.AIMS_JCU <- MMP__gam_extract(wq.flntu.gams,
                                              subregion = "Mackay Whitsunday",
                                              measure = "NTU", field = "plot")
        save(Mackay_tss_gam.AIMS_JCU, file = paste0(GAM_OUTPUT_PATH, "Mackay_tss_gam.AIMS_JCU.RData"))
        
        ## Fitzroy
        Fitzroy_chl_gam.AIMS_JCU <- wq.aims.jcu.gams %>%
            MMP__gam_extract(subregion = "Fitzroy",
                             measure = "DRIFTCHL_UGPERL.wm", field = "plot") +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Fitzroy",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower2, ymax = upper2, x = Date),
                        fill = 'red', color = NA, alpha = 0.1) +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Fitzroy",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower, ymax = upper, x = Date),
                        fill = 'red', color = NA, alpha = 0.3) +
            geom_line(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Fitzroy",
                                              measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                      aes(y = pred, x = Date), color = 'red')

        save(Fitzroy_chl_gam.AIMS_JCU, file = paste0(GAM_OUTPUT_PATH, "Fitzroy_chl_gam.AIMS_JCU.RData"))

        Fitzroy_tss_gam.AIMS_JCU <- MMP__gam_extract(wq.flntu.gams,
                                              subregion = "Fitzroy",
                                              measure = "NTU", field = "plot")
        save(Fitzroy_tss_gam.AIMS_JCU, file = paste0(GAM_OUTPUT_PATH, "Fitzroy_tss_gam.AIMS_JCU.RData"))

        if(file.exists(paste0(GAM_OUTPUT_PATH, "Plots4Angus2.zip")))
            file.remove(paste0(GAM_OUTPUT_PATH, "Plots4Angus2.zip"))
        for (s in c('Daintree','Johnstone','Tully','Burdekin','Mackay','Fitzroy')) {
            for (m in c('chl','tss')) {
                system(paste0("zip -rjo '", GAM_OUTPUT_PATH, "Plots4Angus2.zip' '",
                             GAM_OUTPUT_PATH, s, "_", m, "_gam.AIMS_JCU.RData'"))
            }
        }

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAMS):', msg='AIMS + JCU data.', return=TRUE)
    ## ----end

    ## 4. Plots for Angus (AIMS AND JCU DATA BUT WITH ONLY OPEN COASTAL, MIDSHELF AND OFFSHORE)
    ## ---- Plots for Angus
    MMP_tryCatch(
    {
        ## Barron Daintree
        Daintree_chl_gam.AIMS_JCU_OMO <- wq.aims.jcu.omo.gams %>%
            MMP__gam_extract(subregion = "Barron Daintree",
                             measure = "DRIFTCHL_UGPERL.wm",
                             field = "plot") 
        save(Daintree_chl_gam.AIMS_JCU_OMO, file = paste0(GAM_OUTPUT_PATH, "Daintree_chl_gam.AIMS_JCU_OMO.RData"))

        Daintree_tss_gam.AIMS_JCU_OMO <- wq.aims.jcu.omo.gams %>%
            MMP__gam_extract(subregion = "Barron Daintree",
                             measure = "TSS_MGPERL.wm", field = "plot")
        save(Daintree_tss_gam.AIMS_JCU_OMO, file = paste0(GAM_OUTPUT_PATH, "Daintree_tss_gam.AIMS_JCU_OMO.RData"))

        ## Johnstone Russell Mulgrave
        Johnstone_chl_gam.AIMS_JCU_OMO <- wq.aims.jcu.omo.gams %>%
            MMP__gam_extract(subregion = "Johnstone Russell Mulgrave",
                             measure = "DRIFTCHL_UGPERL.wm", field = "plot") +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Johnstone Russell Mulgrave",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower2, ymax = upper2, x = Date),
                        fill = 'red', color = NA, alpha = 0.1) +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Johnstone Russell Mulgrave",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower, ymax = upper, x = Date),
                        fill = 'red', color = NA, alpha = 0.3) +
            geom_line(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Johnstone Russell Mulgrave",
                                              measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                      aes(y = pred, x = Date), color = 'red')

        save(Johnstone_chl_gam.AIMS_JCU_OMO, file = paste0(GAM_OUTPUT_PATH, "Johnstone_chl_gam.AIMS_JCU_OMO.RData"))

        Johnstone_tss_gam.AIMS_JCU_OMO <- MMP__gam_extract(wq.flntu.gams,
                                              subregion = "Johnstone Russell Mulgrave",
                                              measure = "NTU", field = "plot")
        save(Johnstone_tss_gam.AIMS_JCU_OMO, file = paste0(GAM_OUTPUT_PATH, "Johnstone_tss_gam.AIMS_JCU_OMO.RData"))

        ## Tully Herbert
        Tully_chl_gam.AIMS_JCU_OMO <- wq.aims.jcu.omo.gams %>%
            MMP__gam_extract(subregion = "Tully Herbert",
                             measure = "DRIFTCHL_UGPERL.wm", field = "plot") +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Tully Herbert",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower2, ymax = upper2, x = Date),
                        fill = 'red', color = NA, alpha = 0.1) +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Tully Herbert",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower, ymax = upper, x = Date),
                        fill = 'red', color = NA, alpha = 0.3) +
            geom_line(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Tully Herbert",
                                              measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                      aes(y = pred, x = Date), color = 'red')

        save(Tully_chl_gam.AIMS_JCU_OMO, file = paste0(GAM_OUTPUT_PATH, "Tully_chl_gam.AIMS_JCU_OMO.RData"))

        Tully_tss_gam.AIMS_JCU_OMO <- MMP__gam_extract(wq.flntu.gams,
                                              subregion = "Tully Herbert",
                                              measure = "NTU", field = "plot")
        save(Tully_tss_gam.AIMS_JCU_OMO, file = paste0(GAM_OUTPUT_PATH, "Tully_tss_gam.AIMS_JCU_OMO.RData"))
            
        ## Burdekin
        Burdekin_chl_gam.AIMS_JCU_OMO <- wq.aims.jcu.omo.gams %>%
            MMP__gam_extract(subregion = "Burdekin",
                             measure = "DRIFTCHL_UGPERL.wm", field = "plot") +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Burdekin",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower2, ymax = upper2, x = Date),
                        fill = 'red', color = NA, alpha = 0.1) +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Burdekin",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower, ymax = upper, x = Date),
                        fill = 'red', color = NA, alpha = 0.3) +
            geom_line(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Burdekin",
                                              measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                      aes(y = pred, x = Date), color = 'red')

        save(Burdekin_chl_gam.AIMS_JCU_OMO, file = paste0(GAM_OUTPUT_PATH, "Burdekin_chl_gam.AIMS_JCU_OMO.RData"))

        Burdekin_tss_gam.AIMS_JCU_OMO <- MMP__gam_extract(wq.flntu.gams,
                                              subregion = "Burdekin",
                                              measure = "NTU", field = "plot")
        save(Burdekin_tss_gam.AIMS_JCU_OMO, file = paste0(GAM_OUTPUT_PATH, "Burdekin_tss_gam.AIMS_JCU_OMO.RData"))

        ## Mackay Whitsunday
        Mackay_chl_gam.AIMS_JCU_OMO <- wq.aims.jcu.omo.gams %>%
            MMP__gam_extract(subregion = "Mackay Whitsunday",
                             measure = "DRIFTCHL_UGPERL.wm", field = "plot") +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Mackay Whitsunday",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower2, ymax = upper2, x = Date),
                        fill = 'red', color = NA, alpha = 0.1) +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Mackay Whitsunday",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower, ymax = upper, x = Date),
                        fill = 'red', color = NA, alpha = 0.3) +
            geom_line(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Mackay Whitsunday",
                                              measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                      aes(y = pred, x = Date), color = 'red')

        save(Mackay_chl_gam.AIMS_JCU_OMO, file = paste0(GAM_OUTPUT_PATH, "Mackay_chl_gam.AIMS_JCU_OMO.RData"))

        Mackay_tss_gam.AIMS_JCU_OMO <- MMP__gam_extract(wq.flntu.gams,
                                              subregion = "Mackay Whitsunday",
                                              measure = "NTU", field = "plot")
        save(Mackay_tss_gam.AIMS_JCU_OMO, file = paste0(GAM_OUTPUT_PATH, "Mackay_tss_gam.AIMS_JCU_OMO.RData"))
        
        ## Fitzroy
        Fitzroy_chl_gam.AIMS_JCU_OMO <- wq.aims.jcu.omo.gams %>%
            MMP__gam_extract(subregion = "Fitzroy",
                             measure = "DRIFTCHL_UGPERL.wm", field = "plot") +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Fitzroy",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower2, ymax = upper2, x = Date),
                        fill = 'red', color = NA, alpha = 0.1) +
            geom_ribbon(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Fitzroy",
                                                measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                        aes(y = pred, ymin = lower, ymax = upper, x = Date),
                        fill = 'red', color = NA, alpha = 0.3) +
            geom_line(data = MMP__gam_extract(wq.flntu.gams,
                                                subregion = "Fitzroy",
                                              measure = "DRIFTCHL_UGPERL.wm", field = "year"),
                      aes(y = pred, x = Date), color = 'red')

        save(Fitzroy_chl_gam.AIMS_JCU_OMO, file = paste0(GAM_OUTPUT_PATH, "Fitzroy_chl_gam.AIMS_JCU_OMO.RData"))

        Fitzroy_tss_gam.AIMS_JCU_OMO <- MMP__gam_extract(wq.flntu.gams,
                                              subregion = "Fitzroy",
                                              measure = "NTU", field = "plot")
        save(Fitzroy_tss_gam.AIMS_JCU_OMO, file = paste0(GAM_OUTPUT_PATH, "Fitzroy_tss_gam.AIMS_JCU_OMO.RData"))

        for (s in c('Daintree','Johnstone','Tully','Burdekin','Mackay','Fitzroy')) {
            for (m in c('chl','tss')) {
                system(paste0("zip -rjo '", GAM_OUTPUT_PATH, "Plots4Angus.zip' '",
                             GAM_OUTPUT_PATH, s, "_", m, "_gam.AIMS_JCU_OMO.RData'"))
            }
        }

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Compilations (GAMS):', msg='AIMS + JCU data.', return=TRUE)
    ## ----end

    
    MMP_checkData(name = "Plots4Angus.zip",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label.prefix = "Processed",
                  PATH = GAM_OUTPUT_PATH,
                  progressive = FALSE)
    MMP_openning_banner()
}
