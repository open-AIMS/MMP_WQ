source("MMP_functions.R")
source("MMP_functions_models.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

## NISKIN_INPUT_PATH <- paste0(DATA_PATH, "/primary/niskin/")
GAM_INPUT_PATH <- paste0(DATA_PATH, "/processed/niskin/")
GAM_OUTPUT_PATH <- paste0(DATA_PATH, "/modelled/GAMMS/")

assign("CURRENT_STAGE", 4, env = globalenv())

## ---- AIMS niskin process
CURRENT_ITEM <- "fitGAMM"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"wq.gams.RData"))) &
    file.exists(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')) &
    file.exists(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')) 
    ) {

    ## 1. AIMS niskin only 
    ## ---- AIMS niskin gamms
    MMP_tryCatch(
    {
        wq.gamdata.all.reef <- get(load(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')))
        wq.guidelines <- get(load(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')))

        wq.gams <- wq.gamdata.all.reef %>%
            filter(Source=='AIMS Niskin') %>%
            filter(!Measure %in% c('SI_NOx.wm','NOx_PO4.wm','SI_PO4.wm','Dtt.num',
                                   'CDOM_443.wm','Dt.num.wm', 'HAND_NH4.wm')) %>%
            droplevels() %>%
            group_by(Subregion, Measure) %>%
            summarise(data = list(cur_data_all()), .groups = "drop") %>%
            mutate(N = 1:n(),
                   TotalN = max(N)) %>%
            mutate(ModelPlus = pmap(.l = list(data, N, TotalN),
                             .f = ~ {
                                 .x <- ..1
                                 N <- ..2
                                 TotalN <- ..3
                                 if (nrow(.x)==0 | all(is.na(.x$Value))) {
                                     next
                                 }
                                 fit <- WQfitGAM(.x, bs = 'ps', k = 6, fx = T,
                                          fam = Tweedie(2),
                                          ignore_new_model_type = TRUE)
                                 Subregion <- unique(.x$Subregion)
                                 Measure <- unique(.x$Measure)
                                 MMP_log(status = "SUCCESS",
                                         logFile = LOG_FILE,
                                         Category = str_squish(paste("GAMMS", " for ",
                                                                     Subregion, " subregion (",
                                                                     Measure, ")")),
                                         msg=NULL)
                                 mmp__change_name(stage = paste0("STAGE", CURRENT_STAGE),
                                                  item = CURRENT_ITEM,
                                                  name = paste0("fit GAMMS [",N, "/",TotalN,"]"))
                                 MMP_openning_banner()
                                 fit
                             }
                             )
                   ) %>%
            ## Isolate the model
            mutate(model = map(.x = ModelPlus,
                               .f = ~ .x[[1]])) %>%
            ## annual trend
            mutate(year = map2(.x = data, .y = ModelPlus,
                               .f = ~ {
                                   dat <- .x %>%
                                       complete(reneeYear = seq(min(reneeYear), max(reneeYear),
                                                                by=1)) %>%
                                       dplyr::group_by(reneeYear) %>%
                                       summarise(Value=mean(Value, na.rm=TRUE)) %>%
                                       ungroup() %>%
                                       mutate(Obs=ifelse(is.na(Value), 0, 1)) %>%
                                       dplyr::select(reneeYear, Obs) %>%
                                       suppressWarnings() %>%
                                       suppressMessages()
                                   .y[[2]] %>%
                                       mutate(reneeYear = MMP_reneeYear(Date)) %>%
                                       left_join(dat) %>%
                                       suppressWarnings() %>%
                                       suppressMessages()

                               })) %>%
            ## monthly trend
            mutate(month = map(.x = ModelPlus,
                               .f = ~ .x[[3]])) %>%
            ## annual trend residuals
            mutate(yearResid = map(.x = ModelPlus,
                                   .f = ~ .x[[4]])) %>%
            ## monthly trend residuals
            mutate(monthResid = map(.x = ModelPlus,
                                   .f = ~ .x[[5]])) %>%
            ## plot
            mutate(plot = pmap(.l = list(year, yearResid, Subregion, Measure),
                               .f = ~ {
                                   dat <- ..1 %>% mutate(Subregion=..3,Measure=..4)
                                   resid <- ..2
                                   chemPlot(dat,resid, Source='AIMS Niskin')
                                  }))
        save(wq.gams, file = paste0(DATA_PATH, "/models/wq.gams.RData"))
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Fit GAMMS:', msg='Fit AIMS niskin GAMMS.', return=TRUE)
    ## ----end
    MMP_checkData(name = "wq.gams.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "fit GAMMs",
                  PATH = paste0(DATA_PATH, "/models/"))
    MMP_openning_banner()
}


## ---- AIMS and JCU niskin process
CURRENT_ITEM <- "fitAIMSJCUGAMM"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"wq.aims.jcu.gams.RData"))) &
    file.exists(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')) &
    file.exists(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')) 
    ) {

    ## 1. AIMS and JCU niskin only 
    ## ---- AIMS and JCU niskin gamms
    MMP_tryCatch(
    {
        wq.gamdata.all.reef <- get(load(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')))
        wq.guidelines <- get(load(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')))

        wq.aims.jcu.gams <- wq.gamdata.all.reef %>%
            filter(!Measure %in% c('SI_NOx.wm','NOx_PO4.wm','SI_PO4.wm','Dtt.num',
                                   'CDOM_443.wm','Dt.num.wm','HAND_NH4.wm')) %>%
            filter(!(Source=='JCU Niskin' & Measure %in% c('PP.wm'))) %>%  #Apparently, as of 2020,  the JCU PN data are good,  so they can go back into the GAMs (but PP should stay out!)
            filter(!(Source=='CY Niskin' & Measure %in% c('PP.wm'))) %>%
            droplevels() %>%
            group_by(Subregion, Measure) %>%
            summarise(data = list(cur_data_all()), .groups = "drop") %>%
            mutate(N = 1:n(),
                   TotalN = max(N)) %>%
            mutate(ModelPlus = pmap(.l = list(data, N, TotalN),
                             .f = ~ {
                                 .x <- ..1
                                 N <- ..2
                                 TotalN <- ..3
                                 if (nrow(.x)==0 | all(is.na(.x$Value))) {
                                     next
                                 }
                                 fit <- WQfitGAM(.x, bs = 'ps', k = 6, fx = T,
                                          fam = Tweedie(2),
                                          ignore_new_model_type = TRUE)
                                 Subregion <- unique(.x$Subregion)
                                 Measure <- unique(.x$Measure)
                                 MMP_log(status = "SUCCESS",
                                         logFile = LOG_FILE,
                                         Category = str_squish(paste("GAMMS (AIMS/JCU)", " for ",
                                                                     Subregion, " subregion (",
                                                                     Measure, ")")),
                                         msg=NULL)
                                 mmp__change_name(stage = paste0("STAGE", CURRENT_STAGE),
                                                  item = CURRENT_ITEM,
                                                  name = paste0("fit AIMS/JCU GAMMS [",N, "/",TotalN,"]"))
                                 MMP_openning_banner()
                                 fit
                             }
                             )
                   ) %>%
            ## Isolate the model
            mutate(model = map(.x = ModelPlus,
                               .f = ~ .x[[1]])) %>%
            ## annual trend
            mutate(year = map2(.x = data, .y = ModelPlus,
                               .f = ~ {
                                   dat <- .x %>%
                                       complete(reneeYear = seq(min(reneeYear), max(reneeYear),
                                                                by=1)) %>%
                                       dplyr::group_by(reneeYear) %>%
                                       summarise(Value=mean(Value, na.rm=TRUE)) %>%
                                       ungroup() %>%
                                       mutate(Obs=ifelse(is.na(Value), 0, 1)) %>%
                                       dplyr::select(reneeYear, Obs) %>%
                                       suppressWarnings() %>%
                                       suppressMessages()
                                   .y[[2]] %>%
                                       mutate(reneeYear = MMP_reneeYear(Date)) %>%
                                       left_join(dat) %>%
                                       suppressWarnings() %>%
                                       suppressMessages()

                               })) %>%
            ## monthly trend
            mutate(month = map(.x = ModelPlus,
                               .f = ~ .x[[3]])) %>%
            ## annual trend residuals
            mutate(yearResid = map(.x = ModelPlus,
                                   .f = ~ .x[[4]])) %>%
            ## monthly trend residuals
            mutate(monthResid = map(.x = ModelPlus,
                                   .f = ~ .x[[5]])) %>%
            ## plot
            mutate(plot = pmap(.l = list(year, yearResid, Subregion, Measure),
                               .f = ~ {
                                   dat <- ..1 %>% mutate(Subregion=..3,Measure=..4)
                                   resid <- ..2
                                   chemPlot(dat,resid, Source='AIMS/JCU Niskin', color.source = FALSE)
                                  }))
        save(wq.aims.jcu.gams, file = paste0(DATA_PATH, "/models/wq.aims.jcu.gams.RData"))
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Fit GAMMS:', msg='Fit AIMS/JCU niskin GAMMS.', return=TRUE)
    ## ----end
    MMP_checkData(name = "wq.aims.jcu.gams.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "fit AIMS/JCU GAMMs",
                  PATH = paste0(DATA_PATH, "/models/"))
    MMP_openning_banner()
}


## ---- AIMS and JCU (Open, Mid, Offshore) niskin process
CURRENT_ITEM <- "fitAIMSJCUOMOGAMM"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"wq.aims.jcu.omo.gams.RData"))) &
    file.exists(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')) &
    file.exists(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')) 
    ) {

    ## 1. AIMS and JCU (Open, Mid, Offshore) niskin only 
    ## ---- AIMS and JCU (Open, Mid, Offshore) niskin gamms
    MMP_tryCatch(
    {
        wq.gamdata.all.reef <- get(load(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')))
        wq.guidelines <- get(load(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')))

        wq.aims.jcu.omo.gams <- wq.gamdata.all.reef %>%
            filter(GBRMPA_water_area %in% c('Open Coastal waters','Midshelf waters',
                                            'Offshore waters')) %>%
            droplevels() %>%
            filter(!Measure %in% c('SI_NOx.wm','NOx_PO4.wm','SI_PO4.wm','Dtt.num',
                                   'CDOM_443.wm','Dt.num.wm','HAND_NH4.wm')) %>%
            filter(!(Source=='JCU Niskin' & Measure %in% c('PP.wm'))) %>%  #Apparently, as of 2020,  the JCU PN data are good,  so they can go back into the GAMs (but PP should stay out!)
            filter(!(Source=='CY Niskin' & Measure %in% c('PP.wm'))) %>%
            droplevels() %>%
            group_by(Subregion, Measure) %>%
            summarise(data = list(cur_data_all()), .groups = "drop") %>%
            mutate(N = 1:n(),
                   TotalN = max(N)) %>%
            mutate(ModelPlus = pmap(.l = list(data, N, TotalN),
                             .f = ~ {
                                 .x <- ..1
                                 N <- ..2
                                 TotalN <- ..3
                                 if (nrow(.x)==0 | all(is.na(.x$Value))) {
                                     next
                                 }
                                 fit <- WQfitGAM(.x, bs = 'ps', k = 6, fx = T,
                                          fam = Tweedie(2),
                                          ignore_new_model_type = TRUE)
                                 Subregion <- unique(.x$Subregion)
                                 Measure <- unique(.x$Measure)
                                 MMP_log(status = "SUCCESS",
                                         logFile = LOG_FILE,
                                         Category = str_squish(paste("GAMMS (AIMS/JCU, OMO)", " for ",
                                                                     Subregion, " subregion (",
                                                                     Measure, ")")),
                                         msg=NULL)
                                 mmp__change_name(stage = paste0("STAGE", CURRENT_STAGE),
                                                  item = CURRENT_ITEM,
                                                  name = paste0("fit AIMS/JCU OMO GAMMS [",N, "/",TotalN,"]"))
                                 MMP_openning_banner()
                                 fit
                             }
                             )
                   ) %>%
            ## Isolate the model
            mutate(model = map(.x = ModelPlus,
                               .f = ~ .x[[1]])) %>%
            ## annual trend
            mutate(year = map2(.x = data, .y = ModelPlus,
                               .f = ~ {
                                   dat <- .x %>%
                                       complete(reneeYear = seq(min(reneeYear), max(reneeYear),
                                                                by=1)) %>%
                                       dplyr::group_by(reneeYear) %>%
                                       summarise(Value=mean(Value, na.rm=TRUE)) %>%
                                       ungroup() %>%
                                       mutate(Obs=ifelse(is.na(Value), 0, 1)) %>%
                                       dplyr::select(reneeYear, Obs) %>%
                                       suppressWarnings() %>%
                                       suppressMessages()
                                   .y[[2]] %>%
                                       mutate(reneeYear = MMP_reneeYear(Date)) %>%
                                       left_join(dat) %>%
                                       suppressWarnings() %>%
                                       suppressMessages()

                               })) %>%
            ## monthly trend
            mutate(month = map(.x = ModelPlus,
                               .f = ~ .x[[3]])) %>%
            ## annual trend residuals
            mutate(yearResid = map(.x = ModelPlus,
                                   .f = ~ .x[[4]])) %>%
            ## monthly trend residuals
            mutate(monthResid = map(.x = ModelPlus,
                                   .f = ~ .x[[5]])) %>%
            ## plot
            mutate(plot = pmap(.l = list(year, yearResid, Subregion, Measure),
                               .f = ~ {
                                   dat <- ..1 %>% mutate(Subregion=..3,Measure=..4)
                                   resid <- ..2
                                   chemPlot(dat,resid, Source='AIMS/JCU OMO Niskin', color.source = FALSE)
                                  }))
        save(wq.aims.jcu.omo.gams, file = paste0(DATA_PATH, "/models/wq.aims.jcu.omo.gams.RData"))
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Fit GAMMS:', msg='Fit AIMS/JCU OMO niskin GAMMS.', return=TRUE)
    ## ----end
    MMP_checkData(name = "wq.aims.jcu.omo.gams.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "fit AIMS/JCU OMO GAMMs",
                  PATH = paste0(DATA_PATH, "/models/"))
    MMP_openning_banner()
}



## ---- AIMS FLNTU process
CURRENT_ITEM <- "fitFLNTUGAMM"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"wq.flntu.gams.RData"))) &
    file.exists(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')) &
    file.exists(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')) 
    ) {

    ## 1. AIMS FLNTU only 
    ## ---- AIMS FLNTU gamms
    MMP_tryCatch(
    {
        wq.gamdata.all.reef <- get(load(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')))
        wq.guidelines <- get(load(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')))

        wq.flntu.gams <- wq.gamdata.all.reef %>%
            filter(Source=='AIMS FLNTU') %>%
            mutate(Measure=dplyr:::recode(Measure,CHL="DRIFTCHL_UGPERL.wm")) %>%
            droplevels() %>%
            group_by(Subregion, Measure) %>%
            summarise(data = list(cur_data_all()), .groups = "drop") %>%
            mutate(N = 1:n(),
                   TotalN = max(N)) %>%
            mutate(ModelPlus = pmap(.l = list(data, N, TotalN),
                             .f = ~ {
                                 .x <- ..1
                                 N <- ..2
                                 TotalN <- ..3
                                 if (nrow(.x)==0 | all(is.na(.x$Value))) {
                                     next
                                 }
                                 fit <- WQfitGAM(.x, k = 5, fx = T,
                                          fam = Tweedie(2))
                                 Subregion <- unique(.x$Subregion)
                                 Measure <- unique(.x$Measure)
                                 MMP_log(status = "SUCCESS",
                                         logFile = LOG_FILE,
                                         Category = str_squish(paste("FLNTU GAMMS", " for ",
                                                                     Subregion, " subregion (",
                                                                     Measure, ")")),
                                         msg=NULL)
                                 mmp__change_name(stage = paste0("STAGE", CURRENT_STAGE),
                                                  item = CURRENT_ITEM,
                                                  name = paste0("fit FLNTU GAMMS [",N, "/",TotalN,"]"))
                                 MMP_openning_banner()
                                 fit
                             }
                             )
                   ) %>%
            ## Isolate the model
            mutate(model = map(.x = ModelPlus,
                               .f = ~ .x[[1]])) %>%
            ## annual trend
            mutate(year = map2(.x = data, .y = ModelPlus,
                               .f = ~ {
                                   dat <- .x %>%
                                       complete(reneeYear = seq(min(reneeYear), max(reneeYear),
                                                                by=1)) %>%
                                       dplyr::group_by(reneeYear) %>%
                                       summarise(Value=mean(Value, na.rm=TRUE)) %>%
                                       ungroup() %>%
                                       mutate(Obs=ifelse(is.na(Value), 0, 1)) %>%
                                       dplyr::select(reneeYear, Obs) %>%
                                       suppressWarnings() %>%
                                       suppressMessages()
                                   .y[[2]] %>%
                                       mutate(reneeYear = MMP_reneeYear(Date)) %>%
                                       left_join(dat) %>%
                                       suppressWarnings() %>%
                                       suppressMessages()

                               })) %>%
            ## monthly trend
            mutate(month = map(.x = ModelPlus,
                               .f = ~ .x[[3]])) %>%
            ## annual trend residuals
            mutate(yearResid = map(.x = ModelPlus,
                                   .f = ~ .x[[4]])) %>%
            ## monthly trend residuals
            mutate(monthResid = map(.x = ModelPlus,
                                   .f = ~ .x[[5]])) %>%
            ## plot
            mutate(plot = pmap(.l = list(year, yearResid, Subregion, Measure),
                               .f = ~ {
                                   dat <- ..1 %>% mutate(Subregion=..3,Measure=..4)
                                   resid <- ..2
                                   chemPlot(dat,resid, Source='AIMS FLNTU')
                                  }))
        save(wq.flntu.gams, file = paste0(DATA_PATH, "/models/wq.flntu.gams.RData"))
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Fit GAMMS:', msg='Fit AIMS FLNTU GAMMS.', return=TRUE)
    ## ----end
    MMP_checkData(name = "wq.flntu.gams.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "fit FLNTU GAMMs",
                  PATH = paste0(DATA_PATH, "/models/"))
    MMP_openning_banner()
}



