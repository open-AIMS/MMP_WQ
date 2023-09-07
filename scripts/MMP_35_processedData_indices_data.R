source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

## NISKIN_INPUT_PATH <- paste0(DATA_PATH, "/primary/niskin/")
NISKIN_INPUT_PATH <- paste0(DATA_PATH, "/processed/niskin/")
FLNTU_INPUT_PATH <- paste0(DATA_PATH, "/processed/loggers/")
INDICES_OUTPUT_PATH <- paste0(DATA_PATH, "/processed/niskin/")

MAXDATE=as.Date(paste0(reportYear,'-09-30'))
MINDATE=MAXDATE-years(1)+days(1)

## ---- Indices data
CURRENT_ITEM <- "indicesData"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if ((alwaysExtract | !file.exists(paste0(INDICES_OUTPUT_PATH,"wq.historic.RData"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'niskin.aims.reef.av.RData')) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'niskin.jcu.reef.av1.RData')) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'niskin.cy.reef.av.RData')) &
    file.exists(paste0(FLNTU_INPUT_PATH, 'flntu.all.RData')) 
    ) {

    ## 1. Historic indices
    ## ---- Historic indices
    MMP_tryCatch(
    {
        niskin.aims.reef.av <- get(load(paste0(NISKIN_INPUT_PATH, 'niskin.aims.reef.av.RData')))
        niskin.jcu.reef.av <- get(load(paste0(NISKIN_INPUT_PATH, 'niskin.jcu.reef.av1.RData')))
        niskin.cy.reef.av <- get(load(paste0(NISKIN_INPUT_PATH, 'niskin.cy.reef.av.RData')))
        flntu.all <- get(load(paste0(FLNTU_INPUT_PATH, 'flntu.all.RData')))
        
        load(file=paste0(NISKIN_INPUT_PATH, '/niskin.aims.reef.av.RData'))
        load(file=paste0(NISKIN_INPUT_PATH, '/niskin.jcu.reef.av1.RData'))
        load(file=paste0(NISKIN_INPUT_PATH, '/niskin.cy.reef.av.RData'))
        load(file=paste0(FLNTU_INPUT_PATH, '/flntu.all.RData'))
        wq.guidelines <- get(load(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')))
        wq.units <- get(load(paste0(DATA_PATH, '/primary/other/wq.units.RData')))
        hierarchy <- get(load(paste0(DATA_PATH, '/primary/other/hierarchy.RData')))

        ## The following is established to enable historic index (including temporal design)
        niskin.historic <- niskin.aims.reef.av %>%
            filter(!is.na(PP.wm)) %>%
            filter(HistoricReef) %>% 
            arrange(MMP_SITE_NAME,Date) %>% 
            dplyr::select(DRIFTCHL_UGPERL.wm,TSS_MGPERL.wm,SECCHI_DEPTH.wm,PP.wm,PN.wm,NOx.wm,
                          SI.wm,POC.wm,DIN.wm,DOC.wm,DON.wm,DOP.wm,PO4.wm,  #extras that are not involved in index, but are needed in summary tables
                          SHORT_NAME,MMP_SITE_NAME,GBRMPA_group,Water_Samples, STATION_CLASS,
                          GBRMPA_water_area,Region,Reg,Subregion,Subreg,Date,Season,Month,
                          oldSamplingYear,waterYear,reneeYear,cwaterYear,financialYear,
                          cfinancialYear) %>%
            mutate(Source = 'AIMS Niskin')

        ## For testing purposes, generate a list of the sampling times (months) for each reef
        if (1==2) {
            dat.wy = niskin.historic %>%
                filter(!is.na(PP.wm),waterYear %in% c(2012:reportYear)) %>%
                group_by(Subregion,MMP_SITE_NAME, waterYear) %>%
                summarize(Months=paste0(Month,'(',Season,')',collapse=','),
                          Dates=paste(Date,collapse=',')) %>%
                suppressMessages() %>%
                suppressWarnings()
            dat.ry = niskin.historic %>%
                filter(!is.na(PP.wm),reneeYear %in% c(2012:reportYear)) %>%
                group_by(Subregion,MMP_SITE_NAME, reneeYear) %>%
                summarize(Months=paste0(Month,'(',Season,')',collapse=','),
                          Dates=paste(Date,collapse=',')) %>%
                suppressMessages() %>%
                suppressWarnings()
            dat.fy = niskin.historic %>%
                filter(!is.na(PP.wm),financialYear %in% c(2012:reportYear)) %>%
                group_by(Subregion,MMP_SITE_NAME, financialYear) %>%
                summarize(Months=paste0(Month,'(',Season,')',collapse=','),
                          Dates=paste(Date,collapse=',')) %>%
                suppressMessages() %>%
                suppressWarnings()
            dat.oy = niskin.historic %>%
                filter(!is.na(PP.wm),oldSamplingYear %in% c(2012:reportYear)) %>%
                group_by(Subregion,MMP_SITE_NAME, oldSamplingYear) %>%
                summarize(Months=paste0(Month,'(',Season,')',collapse=','),
                          Dates=paste(Date,collapse=',')) %>%
                suppressMessages() %>%
                suppressWarnings()

            dat <- cbind(dat.oy,dat.fy, dat.ry)   #this might now add up as the different year definitions might result in differing cutoffs!
        }

        ## The following is to provide a way to limit the number of samples used per year.
        ## Prior to 2015, the historic design collected three samples per year (1 in the wet and 2 in the dry).
        ## From 2015 onwards, the number of samples collected per year increased.
        ## In an attempt to allow an index to be computed as it the original program had continued unchanged,
        ## we need to randomly select 1 we and 2 dry samples per post-2014 year.
        ## For some reason, this is not the case for Barron Daintree?
        ## For 2021, since only Renee can really define which months are relevant for each Region, I have no choice but to treat Fitzroy the same as Barron-Daintree
        ## niskin.historic1 = niskin.historic %>% filter(oldSamplingYear<2015 | Subregion=="Barron Daintree")
        niskin.historic1 <- niskin.historic %>%
            filter(oldSamplingYear<2015 | Subregion %in% c("Barron Daintree", "Fitzroy"))
        temporal.lookup <-
            rbind(
                data.frame(
                    Subregion='Johnstone Russell Mulgrave',
                    Period =c(1,1,2,2,3,3,3,3),
                    Month=c('Sep','Oct', 'Feb','Mar', 'Apr','May','Jun','Jul'),
                    Season=c('Dry','Dry', 'Wet','Wet', 'Dry','Dry','Dry','Dry')
                ),
                data.frame(
                    Subregion='Tully Herbert',
                    Period =c(1,1,2,2,3,3,3,3),
                    Month=c('Sep','Oct', 'Feb','Mar', 'Apr','May','Jun','Jul'),
                    Season=c('Dry','Dry', 'Wet','Wet', 'Dry','Dry','Dry','Dry')
                ),
                data.frame(
                    Subregion='Burdekin',
                    Period =c(1,1,2,2,2,3,3),
                    Month=c('Sep','Oct', 'Jan','Feb','Mar', 'May','Jun'),
                    Season=c('Dry','Dry', 'Wet','Wet','Wet', 'Dry','Dry')
                ),
                data.frame(
                    Subregion='Mackay Whitsunday',
                    Period =c(1,1,2,2,3,3),
                    Month=c('Sep','Oct', 'Feb','Mar', 'May','Jun'),
                    Season=c('Dry','Dry', 'Wet','Wet', 'Dry','Dry')
                )
                
            )

        niskin.historic2 <- niskin.historic %>%
            filter(oldSamplingYear>2014,!is.na(PP.wm)) %>% ## this will remove any records that are still only CR #filter(STATION_CLASS!='CR') %>%
            inner_join(temporal.lookup) %>%
            group_by(Subregion,MMP_SITE_NAME,oldSamplingYear,Period) %>%
            filter(row_number()==1L) %>%
            ungroup() %>%
            dplyr::select(-Period) %>%
            as.data.frame() %>%
                suppressMessages() %>%
                suppressWarnings()

        niskin.historic <- niskin.historic1 %>%
            rbind(niskin.historic2) %>%
            arrange(Subregion,MMP_SITE_NAME, oldSamplingYear) %>%
            gather(key=Measure, value=Value, DRIFTCHL_UGPERL.wm,
                   TSS_MGPERL.wm,SECCHI_DEPTH.wm,PP.wm,PN.wm,NOx.wm,
                   SI.wm,POC.wm,DIN.wm,DOC.wm,DON.wm,DOP.wm,PO4.wm)  %>% #did not used to include these
                suppressMessages() %>%
                suppressWarnings()

        dat.oy <- niskin.historic1 %>%
            rbind(niskin.historic2) %>%
            filter(oldSamplingYear %in% c(2012,2013,2014,2015,2016,2017,2018)) %>%
            group_by(Subregion,MMP_SITE_NAME, oldSamplingYear) %>%
            summarize(Months=paste0(Month,'(',Season,')',collapse=','),
                      Dates=paste(Date,collapse=',')) %>%
                suppressMessages() %>%
                suppressWarnings()
        
        ## Add the FLNTU
        wq.historic <- bind_rows(niskin.historic,
                                 flntu.all %>%
                                 dplyr::select(NTU_QA_AVG, SHORT_NAME, MMP_SITE_NAME,
                                               GBRMPA_group,Water_Samples,
                                               GBRMPA_water_area,Region,Reg,Subregion,
                                               Subreg,Season,oldSamplingYear,waterYear,
                                               reneeYear,cwaterYear,financialYear,
                                               cfinancialYear) %>%
                                 mutate(Source = 'AIMS FLNTU') %>%
                                 dplyr::rename(NTU=NTU_QA_AVG) %>%
                                 gather(key=Measure, value=Value, NTU)
                                 ) %>%
            suppressMessages() %>%
            suppressWarnings()

        save(wq.historic, file=paste0(INDICES_OUTPUT_PATH, '/wq.historic.RData'))
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing:', msg='Preparing historic data for historic index.', return=TRUE)
    ## ----end

    
}

if ((alwaysExtract | !file.exists(paste0(INDICES_OUTPUT_PATH,"wq.all.reef.RData"))) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'niskin.aims.reef.av.RData')) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'niskin.jcu.reef.av1.RData')) &
    file.exists(paste0(NISKIN_INPUT_PATH, 'niskin.cy.reef.av.RData')) &
    file.exists(paste0(FLNTU_INPUT_PATH, 'flntu.all.RData')) 
    ) {

    ## 1. All other regular indices
    ## ---- All other regular indices
    MMP_tryCatch(
    {
        niskin.aims.reef.av <- get(load(paste0(NISKIN_INPUT_PATH, 'niskin.aims.reef.av.RData')))
        niskin.jcu.reef.av <- get(load(paste0(NISKIN_INPUT_PATH, 'niskin.jcu.reef.av1.RData')))
        niskin.cy.reef.av <- get(load(paste0(NISKIN_INPUT_PATH, 'niskin.cy.reef.av.RData')))
        flntu.all <- get(load(paste0(FLNTU_INPUT_PATH, 'flntu.all.RData')))
        
        load(file=paste0(NISKIN_INPUT_PATH, '/niskin.aims.reef.av.RData'))
        load(file=paste0(NISKIN_INPUT_PATH, '/niskin.jcu.reef.av1.RData'))
        load(file=paste0(NISKIN_INPUT_PATH, '/niskin.cy.reef.av.RData'))
        load(file=paste0(FLNTU_INPUT_PATH, '/flntu.all.RData'))
        wq.guidelines <- get(load(paste0(DATA_PATH, '/primary/other/wq.guidelines.RData')))
        wq.units <- get(load(paste0(DATA_PATH, '/primary/other/wq.units.RData')))
        hierarchy <- get(load(paste0(DATA_PATH, '/primary/other/hierarchy.RData')))

        niskin.all.reef <- niskin.aims.reef.av %>%
            dplyr::select(DRIFTCHL_UGPERL.wm,TSS_MGPERL.wm,SECCHI_DEPTH.wm,
                          PP.wm,PN.wm,NOx.wm,            #select only necessary info
                          SI.wm,POC.wm,DIN.wm,DOC.wm,DON.wm,DOP.wm,PO4.wm,  #add others of interest (although they dont have guidelines)
                          MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                          GBRMPA_water_area,Region,Reg,Subregion,Subreg,Season,
                          oldSamplingYear,waterYear,reneeYear,cwaterYear,
                          financialYear,cfinancialYear) %>%
            mutate(Source = 'AIMS Niskin') %>%
            full_join(niskin.jcu.reef.av %>%
                      dplyr::select(DRIFTCHL_UGPERL.wm,TSS_MGPERL.wm,SECCHI_DEPTH.wm,
                                    PP.wm,PN.wm,NOx.wm,            #select only necessary info
                                    SI.wm,POC.wm,DIN.wm,DOC.wm,DON.wm,DOP.wm,PO4.wm,  #add others of interest (although they dont have guidelines)
                                    MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                                    GBRMPA_water_area,Region,Reg,Subregion,Subreg,
                                    Season,oldSamplingYear,waterYear,reneeYear,cwaterYear,
                                    financialYear,cfinancialYear) %>%
                      mutate(Source = 'JCU Niskin')
                      ) %>%
            full_join(niskin.cy.reef.av %>%
                      dplyr::select(DRIFTCHL_UGPERL.wm,TSS_MGPERL.wm,SECCHI_DEPTH.wm,PP.wm,
                                    PN.wm,NOx.wm,            #select only necessary info
                                    SI.wm,POC.wm,DIN.wm,DOC.wm,DON.wm,DOP.wm,PO4.wm,  #add others of interest (although they dont have guidelines)
                                    MMP_SITE_NAME,GBRMPA_group,SHORT_NAME,Water_Samples,
                                    GBRMPA_water_area,Region,Reg,Subregion,Subreg,Season,
                                    oldSamplingYear,waterYear,reneeYear,cwaterYear,
                                    financialYear,cfinancialYear) %>%
                      mutate(Source = 'CY Niskin')
                      ) %>%
            gather(key=Measure, value=Value, DRIFTCHL_UGPERL.wm,TSS_MGPERL.wm,SECCHI_DEPTH.wm,
                   PP.wm,PN.wm,NOx.wm,
                   SI.wm,POC.wm,DIN.wm,DOC.wm,DON.wm,DOP.wm,PO4.wm) %>%
            suppressMessages() %>%
            suppressWarnings()
        
        ## Add the FLNTU
        wq.all.reef <- bind_rows(niskin.all.reef,
                                 flntu.all %>%
                                 dplyr::select(NTU_QA_AVG, MMP_SITE_NAME,
                                               GBRMPA_group,SHORT_NAME,Water_Samples,
                                               GBRMPA_water_area,Region,Reg,Subregion,
                                               Subreg,Season,oldSamplingYear,waterYear,
                                               reneeYear,cwaterYear,financialYear,cfinancialYear) %>%
                                 mutate(Source = 'AIMS FLNTU') %>%
                                 dplyr:::rename(NTU=NTU_QA_AVG) %>%
                                 gather(key=Measure, value=Value, NTU)) %>%
            suppressMessages() %>%
            suppressWarnings()
        
        save(wq.all.reef, file=paste0(INDICES_OUTPUT_PATH, '/wq.all.reef.RData'))
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Data processing:', msg='Preparing data for the non-historic index.', return=TRUE)
    ## ----end
    MMP_checkData(name = "wq.all.reef.RData",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "index data prepared",
                  PATH = paste0(INDICES_OUTPUT_PATH))
    MMP_openning_banner()
    
}
