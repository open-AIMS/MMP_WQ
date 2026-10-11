
## Starting instructions

## 1. start a terminal (shell) from the **project root folder**
## 2a. load the singularity module
##    module load singularity
## 3a. run the singularity container (interactively)
##    singularity exec -B .:/home/Project  ../mmp2.sif R
## OR
## 2b and 3b.  run docker image from project root (not scripts folder)
##    docker run --workdir /home/Project --rm -it -v .:/home/Project mmp2
##    ## docker run --workdir /home/Project --rm -it -v .:/home/Project -u $(id -u):$(id -g) mmp
setwd("scripts")
## 4. source the functions
source("MMP_functions.R")
## 5. run fakeArgs
##    MMP_fakeArgs(stage = 4, always_extract = TRUE, reportYear = 2026) 

## If calling this application interactively, you will need to mimic
## the use of command line arguments.  To do so, call the following:
## MMP_fakeArgs(stage = 1:9, always_extract = TRUE, reportYear = 2025)

## if the calling application has landed on this script as the running
## script, then start initialisations

library(tidyverse)
library(brms)
library(tidybayes)
library(posterior)

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

if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"wq_gp.RData"))) &
    file.exists(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')) &
    file.exists(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')) 
    ) {

    ## 1. AIMS niskin only 
    ## ---- AIMS niskin gamms
    MMP_tryCatch(
    {
        wq.gamdata.all.reef <- get(load(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')))
        wq.guidelines <- get(load(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')))

        wq_gp <- wq.gamdata.all.reef %>%
          filter(Source=='AIMS Niskin') %>%
          ## filter(!Measure %in% c('SI_NOx','NOx_PO4','SI_PO4','Dt.num',
          ##                        'CDOM_443','Dtt.num', 'HAND_NH4')) %>%
          filter(Measure %in% c('DRIFTCHL','SECCHI', 'TSS', 'NOx', 'PO4',
                                 'PN', 'PP', 'POC', 'DOC')) %>%
          droplevels() %>%
          group_by(Subregion, Measure) %>%
          nest() |>
          ungroup() |> 
          mutate(N = 1:n(),
                 TotalN = max(N)) |>
          ## Make label
          mutate(label = map2(.x = Subregion, .y = Measure,
                 .f = ~ {
                   paste0("GP_AIMSNiskin_all_", .x, "_", .y) 
                 })) |> 
          ## Prepare data
          mutate(dat = map2(data, Measure,
                            .f = ~ {
                              if (nrow(.x) == 0 | all(is.na(.x$Value))) {
                                dat <- NA
                              }
                              model_data_prep(.x, .y)
                            }
                           )) |>
          ## Prepare formula
          mutate(form = map2(data, Measure, 
                            .f = ~ {
                              model_make_formula(.x, .y)
                            }
                            )) |> 
          ## Prepare priors
          mutate(priors = map(data,
                              .f = ~ {
                                model_make_priors(.x)
                              }
                              )) |>
          ## Fit models
          mutate(mod = pmap(.l = list(form, priors, dat,
                                      label,
                                      N, TotalN),
                            .f = ~ {
                              form <- ..1
                              priors <- ..2
                              dat <- ..3
                              label <- ..4
                              N <- ..5
                              TotalN <- ..6
                              cat(paste0("Fit models: ", N, " out of: ", TotalN, " (", label, ")"), "\n")
                              fname <- paste0(GAM_OUTPUT_PATH, "model_" ,
                                              label, ".rds")
                              if (dat |> filter(!is.na(RESP)) |> nrow() < 10 |
                                  sum(dat$RESP == 0) > 0) {
                                fname <- NA
                                return(fname)
                              }
                              if (!file.exists(fname)) {
                                environment(form) <- environment()
                                mod <- model_fit_model(form, priors, dat)
                                saveRDS(mod, file = fname)
                              }
                              return(fname)
                              }
                            )) |>
          mutate(newdata = map(.x = dat,
                               .f =  ~ {
                                 dat <- .x
                                data.frame(
                                   Dt.num =  seq(min(dat$Dt.num), max(dat$Dt.num),
                                                 length.out =  200),
                                   MMP_SITE_NAME =  NA,
                                   DUPLICATE =  NA,
                                   DEPTH_CODE =  NA,
                                   sample =  NA
                                 ) |>
                                   mutate(Date =  lubridate::date_decimal(Dt.num))
                                 })) |> 
          mutate(draws = pmap(.l = list(mod, newdata, label, N, TotalN),
                              .f = ~ {
                                mod <- ..1
                                newdata <- ..2
                                label <- ..3
                                N <- ..4
                                TotalN <- ..5
                                cat(paste0("Get draws: ", N, " out of: ", TotalN), "\n")
                                fname <- paste0(GAM_OUTPUT_PATH, "draws_" ,
                                                label, ".rds")
                                if (!file.exists(fname)) {
                                  mod <- readRDS(mod) 
                                  draws <- epred_draws(mod, newdata =  newdata, re_formula =  NA)
                                  saveRDS(draws, file = fname)
                                }
                                return(fname)
                              }
                              )) |>
          mutate(derive_draws = map2(draws, label,
                                    .f = ~ {
                                      draws <- .x
                                      label <- .y
                                      fname <- paste0(GAM_OUTPUT_PATH, "draws_" ,
                                                      label, ".rds")
                                      if (!file.exists(fname)) {
                                        draws <- readRDS(draws) 
                                        deriv_draws <- model_regions_of_change(draws)
                                        saveRDS(deriv_draws, file = fname)
                                      }
                                      return(fname)
                                    }
                                    ))
        save(wq_gp, file = paste0(DATA_PATH, "/models/wq_gp.RData"))
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Fit GPs:', msg='Fit AIMS niskin GPs.', return=TRUE)
    ## ----end
    MMP_checkData(name = "wq_gp.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "fit GPs",
                  PATH = paste0(DATA_PATH, "/models/"))
    MMP_openning_banner()
}


## ---- AIMS and JCU niskin process
CURRENT_ITEM <- "fitAIMSJCUGAMM"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"wq.aims.jcu_gp.RData"))) &
    file.exists(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')) &
    file.exists(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')) 
    ) {

    ## 1. AIMS and JCU niskin only 
    ## ---- AIMS and JCU niskin gamms
    MMP_tryCatch(
    {
        wq.gamdata.all.reef <- get(load(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')))
        wq.guidelines <- get(load(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')))

        wq.aims.jcu_gp <- wq.gamdata.all.reef %>%
          ## filter(!Measure %in% c('SI_NOx.wm','NOx_PO4.wm','SI_PO4.wm','Dtt.num',
          ##                        'CDOM_443.wm','Dt.num.wm','HAND_NH4.wm')) %>%
          filter(Measure %in% c('DRIFTCHL','SECCHI', 'TSS', 'NOx', 'PO4',
                                'PN', 'PP', 'POC', 'DOC')) %>%
          droplevels() %>%
          ## As of 2025, PP can now be returned to the analyses
          ## filter(!(Source=='JCU Niskin' & Measure %in% c('PP.wm'))) %>%  #Apparently, as of 2020,  the JCU PN data are good,  so they can go back into the GAMs (but PP should stay out!)
          ## filter(!(Source=='CY Niskin' & Measure %in% c('PP.wm'))) %>%
          ## As of 2025, we should exclude all CY data prior to the reneeYear of 2021
          filter(!(Source == 'CY Niskin' & reneeYear < 2021)) %>% 
          droplevels() %>%
          group_by(Subregion, Measure) %>%
          ## summarise(data = list(cur_data_all()), .groups = "drop") %>%
          nest() |>
          ungroup() |> 
          mutate(N = 1:n(),
                 TotalN = max(N)) %>%
          ## Make label
          mutate(label = map2(.x = Subregion, .y = Measure,
                 .f = ~ {
                   paste0("GP_AIMSJCUNiskin_all_", .x, "_", .y) 
                 })) 



        
        wq.aims.jcu.gams <- wq.aims.jcu.gams %>%
            mutate(Change = map(.x = model,
                                .f = ~ MMP_GAMchange(.x, refYear = 2015)))
        
        save(wq.aims.jcu_gp, file = paste0(DATA_PATH, "/models/wq.aims.jcu_gp.RData"))
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Fit GPs:', msg='Fit AIMS/JCU niskin GPs.', return=TRUE)
    ## ----end
    MMP_checkData(name = "wq.aims.jcu_gp.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "fit AIMS/JCU GPs",
                  PATH = paste0(DATA_PATH, "/models/"))
    MMP_openning_banner()
}


## ---- AIMS and JCU (Open, Mid, Offshore) niskin process
CURRENT_ITEM <- "fitAIMSJCUOMOGAMM"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"wq.aims.jcu.omo_gps.RData"))) &
    file.exists(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')) &
    file.exists(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')) 
    ) {

    ## 1. AIMS and JCU (Open, Mid, Offshore) niskin only 
    ## ---- AIMS and JCU (Open, Mid, Offshore) niskin gamms
    MMP_tryCatch(
    {
        wq.gamdata.all.reef <- get(load(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')))
        wq.guidelines <- get(load(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')))

        wq.aims.jcu.omo_gp <- wq.gamdata.all.reef %>%
          filter(GBRMPA_water_area %in% c('Open Coastal waters','Midshelf waters',
                                          'Offshore waters')) %>%
          droplevels() %>%
          ## filter(!Measure %in% c('SI_NOx.wm','NOx_PO4.wm','SI_PO4.wm','Dtt.num',
          ##                        'CDOM_443.wm','Dt.num.wm','HAND_NH4.wm')) %>%
          filter(Measure %in% c('DRIFTCHL','SECCHI', 'TSS', 'NOx', 'PO4',
                                'PN', 'PP', 'POC', 'DOC')) %>%
          ## As of 2025, PP can now be returned to the analyses
          ## filter(!(Source=='JCU Niskin' & Measure %in% c('PP.wm'))) %>%  #Apparently, as of 2020,  the JCU PN data are good,  so they can go back into the GAMs (but PP should stay out!)
          ## filter(!(Source=='CY Niskin' & Measure %in% c('PP.wm'))) %>%
          ## As of 2025, we should exclude all CY data prior to the reneeYear of 2021
          filter(!(Source == 'CY Niskin' & reneeYear < 2021)) %>% 
          droplevels() %>%
          group_by(Subregion, Measure) %>%
          ## summarise(data = list(cur_data_all()), .groups = "drop") %>%
          nest() |>
          ungroup() |> 
          mutate(N = 1:n(),
                 TotalN = max(N)) %>%
          ## Make label
          mutate(label = map2(.x = Subregion, .y = Measure,
                 .f = ~ {
                   paste0("GP_AIMSJCUNiskin_OMO_", .x, "_", .y) 
                 })) |> 
          ## Prepare data
          mutate(dat = map2(data, Measure,
                            .f = ~ {
                              if (nrow(.x) == 0 | all(is.na(.x$Value))) {
                                dat <- NA
                              }
                              model_data_prep(.x, .y)
                            }
                           )) |>
          ## Prepare formula
          mutate(form = map2(data, Measure, 
                            .f = ~ {
                              model_make_formula(.x, .y)
                            }
                            )) |> 
          ## Prepare priors
          mutate(priors = map(data,
                              .f = ~ {
                                model_make_priors(.x)
                              }
                              )) |>
          ## Fit models
          mutate(mod = pmap(.l = list(form, priors, dat,
                                      label,
                                      N, TotalN),
                            .f = ~ {
                              form <- ..1
                              priors <- ..2
                              dat <- ..3
                              label <- ..4
                              N <- ..5
                              TotalN <- ..6
                              cat(paste0("Fit models: ", N, " out of: ", TotalN, " (", label, ")"), "\n")
                              fname <- paste0(GAM_OUTPUT_PATH, "model_" ,
                                              label, ".rds")
                              if (dat |> filter(!is.na(RESP)) |> nrow() < 10 |
                                  sum(dat$RESP == 0) > 0) {
                                fname <- NA
                                return(fname)
                              }
                              if (!file.exists(fname)) {
                                environment(form) <- environment()
                                mod <- model_fit_model(form, priors, dat)
                                saveRDS(mod, file = fname)
                              }
                              return(fname)
                              }
                            )) 

        save(wq.aims.jcu.omo_gp, file = paste0(DATA_PATH, "/models/wq.aims.jcu.omo_gp.RData"))
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Fit GAMMS:', msg='Fit AIMS/JCU OMO niskin GPs.', return=TRUE)
    ## ----end
    MMP_checkData(name = "wq.aims.jcu.omo_gp.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "fit AIMS/JCU OMO GPs",
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



