
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
## setwd("scripts")
## 4. source the functions
source("MMP_functions.R")
## 5. run fakeArgs
##    MMP_fakeArgs(stage = 4, always_extract = TRUE, reportYear = 2026) 

## If calling this application interactively, you will need to mimic
## the use of command line arguments.  To do so, call the following:
## MMP_fakeArgs(stage = 4, always_extract = TRUE, reportYear = 2026)

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

if (1 == 2) {
  
## ---- AIMS niskin process
CURRENT_ITEM <- "fitGAMM"

if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"wq_gp.RData"))) &
    file.exists(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')) &
    file.exists(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')) 
    ) {

  ## 1. AIMS niskin only 
  ## ---- AIMS niskin gamms
  wq.gamdata.all.reef <- get(load(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')))
  wq.guidelines <- get(load(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')))

  options(future.globals.maxSize = 2 * 1024^3)
  old_plan <- future::plan(future::multisession, workers = 18)
  
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
    )) 
  ## Fit models

  results <- furrr::future_pmap(.l = list(wq_gp$form, wq_gp$priors, wq_gp$dat,
    wq_gp$label, wq_gp$N, wq_gp$TotalN),
    .f = ~ {
      form <- ..1
      priors <- ..2
      dat <- ..3
      label <- ..4
      N <- ..5
      TotalN <- ..6
      cat(paste0("Fit models: ", N, " out of: ", TotalN, " (", label, ")"), "\n")
      fname <- paste0(GAM_OUTPUT_PATH, "model_", label, ".rds")
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
    },
    .progress = TRUE,
    .options = furrr::furrr_options(scheduling = Inf)
  )
  future::plan(old_plan) 
  wq_gp <- wq_gp |> mutate(mod = results)

  save(wq_gp, file = paste0(DATA_PATH, "/models/wq_gp.RData"))
   
}

## ---- AIMS and JCU niskin process
CURRENT_ITEM <- "fitAIMSJCUGAMM"

if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"wq.aims.jcu_gp.RData"))) &
    file.exists(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')) &
    file.exists(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData'))
    ) {

  ## 1. AIMS and JCU niskin only
  ## ---- AIMS and JCU niskin gamms
  wq.gamdata.all.reef <- get(load(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')))
  wq.guidelines <- get(load(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')))

  options(future.globals.maxSize = 2 * 1024^3)
  old_plan <- future::plan(future::multisession, workers = 18)

  wq.aims.jcu_gp <- wq.gamdata.all.reef %>%
    droplevels() |> 
    filter(!(Source == 'CY Niskin' & reneeYear < 2021)) %>% 
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
        paste0("GP_AIMSJCUNiskin_all_", .x, "_", .y) 
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
    )) 
  ## Fit models

  results <- furrr::future_pmap(.l = list(wq.aims.jcu_gp$form, wq.aims.jcu_gp$priors, wq.aims.jcu_gp$dat,
    wq.aims.jcu_gp$label, wq.aims.jcu_gp$N, wq.aims.jcu_gp$TotalN),
    .f = ~ {
      form <- ..1
      priors <- ..2
      dat <- ..3
      label <- ..4
      N <- ..5
      TotalN <- ..6
      cat(paste0("Fit models: ", N, " out of: ", TotalN, " (", label, ")"), "\n")
      fname <- paste0(GAM_OUTPUT_PATH, "model_", label, ".rds")
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
    },
    .progress = FALSE,
    .options = furrr::furrr_options(scheduling = Inf)
  )
  future::plan(old_plan) 
  wq.aims.jcu_gp <- wq.aims.jcu_gp |> mutate(mod = results)

  save(wq.aims.jcu_gp, file = paste0(DATA_PATH, "/models/wq.aims.jcu_gp.RData"))
}

}

## ---- AIMS niskin process
CURRENT_ITEM <- "fitAIMSJCUOMOGAMM"

if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"wq.aims.jcu.omo_gp.RData"))) &
    file.exists(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')) &
    file.exists(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')) 
    ) {

  ## 1. AIMS and JCU (Open, Mid, Offshore) niskin only 
  ## ---- AIMS and JCU (Open, Mid, Offshore) niskin gamms
  wq.gamdata.all.reef <- get(load(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')))
  wq.guidelines <- get(load(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')))

  options(future.globals.maxSize = 2 * 1024^3)
  old_plan <- future::plan(future::multisession, workers = 18)

  wq.aims.jcu.omo_gp <- wq.gamdata.all.reef %>%
    filter(GBRMPA_water_area %in% c('Open Coastal waters','Midshelf waters', 
      'Offshore waters')) |> 
    droplevels() |> 
    filter(!(Source == 'CY Niskin' & reneeYear < 2021)) %>% 
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
    )) 
  ## Fit models

  results <- furrr::future_pmap(.l = list(wq.aims.jcu.omo_gp$form, wq.aims.jcu.omo_gp$priors, wq.aims.jcu.omo_gp$dat,
    wq.aims.jcu.omo_gp$label, wq.aims.jcu.omo_gp$N, wq.aims.jcu.omo_gp$TotalN),
    .f = ~ {
      form <- ..1
      priors <- ..2
      dat <- ..3
      label <- ..4
      N <- ..5
      TotalN <- ..6
      cat(paste0("Fit models: ", N, " out of: ", TotalN, " (", label, ")"), "\n")
      fname <- paste0(GAM_OUTPUT_PATH, "model_", label, ".rds")
      if (dat |> filter(!is.na(RESP)) |> nrow() < 10 |
            sum(dat$RESP == 0) > 0) {
        fname <- NA
        return(fname)
      }
      ## if (!file.exists(fname)) {
        environment(form) <- environment()
        mod <- model_fit_model(form, priors, dat)
        saveRDS(mod, file = fname)
      ## }
      return(fname)
    },
    .progress = FALSE,
    .options = furrr::furrr_options(scheduling = Inf)
  )
  future::plan(old_plan) 
  wq.aims.jcu.omo_gp <- wq.aims.jcu.omo_gp |> mutate(mod = results)

  save(wq.aims.jcu.omo_gp, file = paste0(DATA_PATH, "/models/wq.aims.jcu.omo_gp.RData"))
   
}

CURRENT_ITEM <- "fitFLNTUGAMM"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(GAM_OUTPUT_PATH,"wq.flntu.gams.RData"))) &
      file.exists(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')) &
      file.exists(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')) 
) {

  ## 1. AIMS FLNTU only 
  ## ---- AIMS FLNTU gamms
  wq.gamdata.all.reef <- get(load(paste0(GAM_INPUT_PATH, 'wq.gamdata.all.reef.RData')))
  wq.guidelines <- get(load(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')))

  options(future.globals.maxSize = 2 * 1024^3)
  old_plan <- future::plan(future::multisession, workers = 18)
  

  wq.flntu_gp <- wq.gamdata.all.reef %>%
    filter(Source=='AIMS FLNTU') %>%
    droplevels() |> 
    ## mutate(Measure=dplyr:::recode(Measure,CHL="DRIFTCHL_UGPERL.wm")) %>%
    droplevels() %>%
    mutate(RESP = Value) |> 
    group_by(Subregion, Measure) %>%
    nest() |>
    ungroup() |> 
    mutate(N = 1:n(),
      TotalN = max(N)) %>%
    ## Make label
    mutate(label = map2(.x = Subregion, .y = Measure,
      .f = ~ {
        paste0("GP_AIMSFLNTU_all_", .x, "_", .y) 
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
    )) 
  ## Fit models

  results <- furrr::future_pmap(.l = list(wq.flntu_gp$form, wq.flntu_gp$priors, wq.flntu_gp$dat,
    wq.flntu_gp$label, wq.flntu_gp$N, wq.flntu_gp$TotalN),
    .f = ~ {
      form <- ..1
      priors <- ..2
      dat <- ..3
      label <- ..4
      N <- ..5
      TotalN <- ..6
      cat(paste0("Fit models (FLNTU): ", N, " out of: ", TotalN, " (", label, ")"), "\n")
      fname <- paste0(GAM_OUTPUT_PATH, "model_", label, ".rds")
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
    },
    .progress = FALSE,
    .options = furrr::furrr_options(scheduling = Inf)
  )
  future::plan(old_plan) 
  wq.flntu_gp <- wq.flntu_gp |> mutate(mod = results)
  
  save(wq.flntu_gp, file = paste0(DATA_PATH, "/models/wq.flntu_gp.RData"))
}
