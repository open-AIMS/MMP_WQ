source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

NISKIN_INPUT_PATH <- paste0(DATA_PATH, "/primary/niskin/")
NISKIN_OUTPUT_PATH <- paste0(DATA_PATH, "/processed/niskin/")

## ---- AIMS niskin process
CURRENT_ITEM <- "aimsNiskin"
mmp__change_status(stage = "STAGE3", item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if (file.exists(paste0(NISKIN_INPUT_PATH, 'niskin.csv'))) {
    MMP_tryCatch(niskin.reef <- read_csv(paste0(NISKIN_INPUT_PATH, 'niskin.csv')) %>%
                     suppressMessages(),
                 LOG_FILE, item = CURRENT_NISKIN, Category = 'Data processing', msg='Reading in Water Quality (Niskin) data', return=TRUE)
    
    ## 1. First level of data processing
    ## ---- AIMS niskin process level 1
    MMP_tryCatch(
    {
        MAXDATE=as.Date(paste0(reportYear,'-08-31'))  #this used to be -06-30
        MINDATE=MAXDATE - lubridate::years(1) + lubridate::days(1)
        niskin.aims.reef = niskin.reef %>%
            mutate(
                COLLECTION_START_DATE = gsub('Sept','Sep', COLLECTION_START_DATE),
                Date = as.Date(COLLECTION_START_DATE, format='%d-%b-%Y'),           # convert to date
                oldSamplingYear=MMP_oldSamplingYear(Date),
                waterYear = MMP_waterYear(Date),
                reneeYear = MMP_reneeYear(Date),
                cwaterYear = MMP_categoricalWaterYear(Date),
                financialYear = MMP_financialYear(Date),                                    # add water year
                cfinancialYear = MMP_categoricalFinancialYear(Date),                        # categorical year
                Dt.num = MMP_decimalDate(Date)                                      # decimal year
            ) %>%
            MMP_correctLocations()  %>%   #correct the LOCATION_NAME of a couple of Reefs at a couple of times 
            ##In order to relate water quality measures to environmental covariates (particularly tidal flow)
            ## it is necessary that the time of sample collection be
            ## preserved in addition to the date.
            mutate(Time=as.POSIXct(COLLECTION_START_DATE, format='%d-%b-%Y %H:%M:%S')) %>%
            mutate(reef.alias=ifelse(MMP_SITE_NAME!='',
                                     as.character(MMP_SITE_NAME),
                                     as.character(LOCATION_NAME))) %>%   #get reef.alias from MMP_SITE_NAME
            filter(Date<MAXDATE) %>%
            MMP_reorderReefs() %>%            #reorder the reef.alias levels according to Latitude and then Longitude 
            MMP_selectReefs(source='niskin')  #select reefs according to the (lookup.csv) 'niskin' design (other options are 'JCU', 'flntu', 'coral', 'WaterTemp')
        save(niskin.aims.reef, file=paste0(NISKIN_OUTPUT_PATH, 'niskin.aims.reef.RData'))
    }, LOG_FILE, Category = 'Data processing', msg='Initial parsing of Water Quality (Niskin) data', return=TRUE)

    MMP_checkData(name = "niskin.aims.reef.RData",
                  stage = "STAGE3",
                  item = "aimsNiskin",
                  label = "AIMS niskin",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end
    ## 2. Correct the PN data for instrument transition
    ## ---- AIMS niskin process level 2
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_PATH, '/pn.lm.RData'))
        wch <- is.na(niskin.aims.reef$PN_UM) & !is.na(niskin.aims.reef$PN_SHIM_UM)
        xs <- niskin.aims.reef$PN_SHIM_UM[wch]
        niskin.aims.reef$PN_UM[wch] <- predict(pn.lm, newdata=data.frame(PN_SHIM_UM.wm=xs))
        save(niskin.aims.reef, file=paste0(NISKIN_OUTPUT_PATH, 'niskin.aims.reef.RData'))
    }, LOG_FILE, Category = 'Data processing', msg='Correct the PN data for instrument transition for Water Quality (Niskin) data', return=TRUE)

    MMP_checkData(name = "niskin.aims.reef.RData",
                  stage = "STAGE3",
                  item = "aimsNiskin",
                  label = "AIMS niskin",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()
    ## ----end
    ## 3. Further processing
    ## ---- AIMS niskin process level 3
    MMP_tryCatch(
    {
        ## msg <- file("log/junk.msg", open = "wt"); sink(msg); sink(msg, type = "message")
        load(paste0(NISKIN_OUTPUT_PATH, 'niskin.aims.reef.RData'))
        niskin.aims.reef <- niskin.aims.reef %>%
            MMP_limitDetection() %>%                                                           #correct for limit detection for HAND_NH4
            MMP_convertUnits() %>%                                                         #convert units from those used to store data (micro mol) to those  for reporting (micro gram)
             MMP_derivedChem()                                                          #derive a bunch of chemical combinations and ratios
        niskin.aims.reef.av <-
            niskin.aims.reef %>%
            filter(STATION_CLASS != 'V') %>%                        #2020: remove this station class
            MMP_aggregateWQDuplicates() %>%                   #aggregate the duplicates
            MMP_consecutiveDays() %>%  #Note, this has changed (2018)... It is now based on LOCATION and Date and it also removes CR samples that are not aligned with R samples.
             ungroup %>% #select(-Col) %>%                                                           #I THINK THERE MIGHT HAVE BEEN A BUG IN THE PREVIOUS COE FOR WHEN DIFFERENT STATIONS ARE COLLECTED ON THE SAME DAY.generate a field that specifies a Collection of observations (perhaps collected over multiple days)
             MMP_depthWeightedAverages() %>%                                                #generate a few different depth weighted averages
             MMP_designLatest(WQ=TRUE) %>%
             MMP_GBRMPA_specs(WQ=TRUE) %>%
             MMP_region_subregion(Source='Niskin')# %>%
             ## MMP_reorderReefs() %>% arrange(MMP_SITE_NAME) %>%
             ## mutate(Label = MMP_locationLabels(reef.alias),         #create names for consistency with forams data
             ##        Dtt.num = as.integer(Time),
             ##        Mnth = as.integer(format(Date, format='%m')),
             ##        Subregion=factor(Subregion, levels=unique(Subregion))) %>% droplevels %>%
             ## mutate(HistoricReef=MMP_HistoricReef(MMP_SITE_NAME))
        save(niskin.aims.reef.av, file=paste0(NISKIN_OUTPUT_PATH, 'niskin.aims.reef.av.RData'))
        ## rm(niskin.aims.reef.agg, niskin.aims.reef, niskin.aims.reef.av)
        ## gc()
    }, LOG_FILE, Category = "Data processing", msg='Process Water Quality (Niskin) data', return=TRUE)

    MMP_checkData(name = "niskin.aims.reef.av.RData",
                  stage = "STAGE3",
                  item = "aimsNiskin",
                  label = "AIMS niskin processed averages",
                  PATH = NISKIN_OUTPUT_PATH,
                  progressive = TRUE)
    MMP_openning_banner()

    ## ----end

} else {
    MMP_checkData(name = "niskin.aims.reef.RData",
                  stage = "STAGE3",
                  item = "aimsNiskin",
                  label = "AIMS niskin",
                  PATH = NISKIN_OUTPUT_PATH)
}

## ----end
