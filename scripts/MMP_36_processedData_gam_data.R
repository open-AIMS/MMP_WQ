source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

## NISKIN_INPUT_PATH <- paste0(DATA_PATH, "/primary/niskin/")
NISKIN_OUTPUT_PATH <- paste0(DATA_PATH, "/processed/niskin/")
FLNTU_OUTPUT_PATH <- paste0(DATA_PATH, "/processed/loggers/")
MAXDATE=as.Date(paste0(reportYear,'-09-30'))
MINDATE=MAXDATE-years(1)+days(1)

## ---- AIMS niskin process
CURRENT_ITEM <- "gamData"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(NISKIN_OUTPUT_PATH,"wq.gam.all.reef.RData"))) &
    file.exists(paste0(NISKIN_OUTPUT_PATH, 'niskin.aims.reef.av.RData')) &
    file.exists(paste0(NISKIN_OUTPUT_PATH, 'niskin.jcu.reef.av1.RData')) &
    file.exists(paste0(NISKIN_OUTPUT_PATH, 'niskin.cy.reef.av.RData')) &
    file.exists(paste0(FLNTU_OUTPUT_PATH, 'flntu.all.RData')) 
    ) {

    ## 1. Read in data
    ## ---- Read in data
    MMP_tryCatch(
    {
        niskin.aims.reef.av <- get(load(paste0(NISKIN_OUTPUT_PATH, 'niskin.aims.reef.av.RData')))
        niskin.jcu.reef.av <- get(load(paste0(NISKIN_OUTPUT_PATH, 'niskin.jcu.reef.av1.RData')))
        niskin.cy.reef.av <- get(load(paste0(NISKIN_OUTPUT_PATH, 'niskin.cy.reef.av.RData')))
        flntu.all <- get(load(paste0(FLNTU_OUTPUT_PATH, 'flntu.all.RData')))

    },
    LOG_FILE, item = CURRENT_NISKIN, Category = 'Data processing:', msg='Reading in data for GAMM preparations.', return=TRUE)
    ## ----end

    ##2. join AIMS and JCU data
    ## ---- Join AIMS and JCU data
    MMP_tryCatch(
    {
        wq.gamdata.all.reef <- niskin.aims.reef.av %>%
            dplyr::select(ends_with('.wm'),            #select only necessary info
                          MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                          GBRMPA_water_area,Region,Reg,Subregion,Subreg,Season,
                          waterYear,reneeYear,cwaterYear,financialYear,cfinancialYear,
                          Date,Mnth,Dt.num,Dtt.num) %>%
            mutate(Source = 'AIMS Niskin') %>%
            full_join(niskin.jcu.reef.av %>%
                      dplyr::select(ends_with('.wm'),            #select only necessary info
                                    MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                                    GBRMPA_water_area,Region,Reg,Subregion,Subreg,Season,
                                    waterYear,reneeYear,cwaterYear,financialYear,
                                    cfinancialYear,Date,Mnth,Dt.num,Dtt.num) %>%
                      mutate(Source = 'JCU Niskin')
                      ) %>%
            full_join(niskin.cy.reef.av %>%
                      dplyr::select(ends_with('.wm'),            #select only necessary info
                                    MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                                    GBRMPA_water_area,Region,Reg,Subregion,Subreg,Season,
                                    waterYear,reneeYear,cwaterYear,financialYear,
                                    cfinancialYear,Date,Mnth,Dt.num,Dtt.num) %>%
                      mutate(Source = 'CY Niskin')
                      ) %>%
            dplyr:::rename(LATITUDE=LATITUDE.wm, LONGITUDE=LONGITUDE.wm) %>%
            dplyr::select(-ACOUSTIC_DEPTH.wm,-oldSamplingYear.wm,-waterYear.wm,
                          -reneeYear.wm,-financialYear.wm,-SWELL_HEIGHT.wm,-WIND_SPEED.wm,
                          -WIND_DIR.wm,-Station.wm,
                          -contains('_UM')) %>%
            gather(key=Measure,value=Value, -LONGITUDE,-LATITUDE,-MMP_SITE_NAME,
                   -GBRMPA_group,-SHORT_NAME,-Water_Samples,
                   -GBRMPA_water_area,-Region,-Reg,-Subregion,-Subreg,-Season,-waterYear,
                   -reneeYear,-cwaterYear,-financialYear,-cfinancialYear,-Date,-Mnth,-Dt.num,
                   -Source) %>%
            suppressMessages() %>%
            suppressWarnings()
    },
    LOG_FILE, item = CURRENT_NISKIN, Category = 'Data processing:', msg='Joining AIMS and JCU niskin data for GAMM preparations.', return=TRUE)
    ## ----end
        
    ##3. Add the FLNTU
    ## ---- Add FLNTU data
    MMP_tryCatch(
    {
        wq.gamdata.all.reef <- wq.gamdata.all.reef %>%
            bind_rows(flntu.all %>%
                      mutate(Source = 'AIMS FLNTU',
                             Mnth=as.integer(format(Date,'%m'))) %>%
                      dplyr::select(LATITUDE,LONGITUDE,MMP_SITE_NAME,GBRMPA_group,
                                    SHORT_NAME,Water_Samples,
                                    GBRMPA_water_area,Region,Reg,Subregion,Subreg,Season,
                                    waterYear,reneeYear,cwaterYear,financialYear,
                                    cfinancialYear,Date,Mnth,Dt.num,Source,CHL_QA_AVG,
                                    NTU_QA_AVG) %>%
                      dplyr:::rename(NTU=NTU_QA_AVG,CHL=CHL_QA_AVG) %>%
                      gather(key=Measure, value=Value, NTU,CHL)
                      ) %>%
            suppressMessages() %>%
            suppressWarnings()
        
        save(wq.gamdata.all.reef,
             file=paste0(NISKIN_OUTPUT_PATH, 'wq.gamdata.all.reef.RData'))

        unlink(paste0(DATA_PATH, "/reports/STAGE",CURRENT_STAGE, "_", CURRENT_ITEM, "_.RData")) 
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SECTION = paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                                    item = CURRENT_ITEM),"\n\n"),
                               TABSET = paste0("::: panel-tabset \n\n"),
                               TABSET_END = paste0("::: \n\n"),
                               )

        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_GLIMPSE = structure(paste0("## Data glimpse\n"),
                                                              parent = 'TABSET'),
                               TAB = structure(mmp__add_table(mmp__glimpse_like(wq.gamdata.all.reef)),
                                               parent = 'SUBSECTION_GLIMPSE'),
                               TAB.CAP = structure(paste0("\n:Extraction of the first five records in each field from the prepared gamm input data. {#tbl-gam}\n\n"),
                                                   parent = 'SUBSECTION_GLIMPSE')
                              )
    },
    LOG_FILE, item = CURRENT_NISKIN, Category = 'Data processing:', msg='Add FLNTU data for GAMM preparations.', return=TRUE)
    ## ----end
}

MMP_checkData(name = "wq.gamdata.all.reef.RData",
              stage = paste0("STAGE", CURRENT_STAGE),
              item = CURRENT_ITEM,
              label.prefix = "Processed",
              PATH = NISKIN_OUTPUT_PATH,
              progressive = FALSE)
MMP_openning_banner()
