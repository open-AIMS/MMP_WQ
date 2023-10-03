source("MMP_functions.R")
source("MMP_functions_models.R")
source("MMP_functions_excel.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

NISKIN_INPUT_PATH <- paste0(DATA_PATH, "/processed/niskin/")
PARAMS_INPUT_PATH <- paste0(DATA_PATH, "/primary/other/")
XLS_OUTPUT_FILE <- paste0(OUTPUT_PATH, "/mmp.xlsx")

assign("CURRENT_STAGE", 8, env = globalenv())

## ---- AIMS niskin process
CURRENT_ITEM <- "excel"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                       SECTION = paste0("# ", str_to_title(CURRENT_ITEM), "\n\n"),
                       TABSET = paste0("::: panel-tabset \n\n"),
                       TABSET_END = paste0("::: \n\n")
                       )

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
    ## 1. Create the excel workbook
    ## ---- create the workbook
    MMP_tryCatch(
    {
        MMP_excel_styles()
        wb <- createWorkbook()
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Set up workbook', return=TRUE)
    ## ----end

    ## 2. Add stations
    ## ---- Stations
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        
        MMP_prepare_table_stations() %>% list2env(envir = globalenv()) %>%
            suppressWarnings() %>%
            suppressMessages()

        sheet <- "Stations"
        addWorksheet(wb, sheet)
        headers <- typology %>% group_by(what) %>%
        mutate(rep = 1:n(),
               Header1 = ifelse(max(rep) == 1 | (max(rep) > 1 & rep > 1), "", what),
               Header2 = measure) %>%
        ungroup() %>% 
        dplyr::select(Header1, Header2)  %>%
        t()
        nCols <- ncol(headers)
        nRows <- nrow(headers)
        writeData(wb, sheet, headers, colNames = FALSE, withFilter = FALSE)
        writeData(wb, sheet, wq.sites, colNames = FALSE, withFilter = FALSE,
                  startRow = 3)
        setColWidths(wb, sheet, cols = 4:8, widths = "auto")
        mergeCells(wb, sheet, cols = 4:5, rows = 1)
        mergeCells(wb, sheet, cols = 6:8, rows = 1)
        addStyle(wb, sheet, cols = 1:nCols, rows = 1:nRows,
                 style = createStyle(border = "TopBottomLeftRight",
                                     fgFill = "lightblue",
                                     halign = "center",
                                     borderColour = openxlsx_getOp("borderColour", "black"),
                                     borderStyle = openxlsx_getOp("borderStyle", "thick")),
                 gridExpand = TRUE)
        freezePane(wb, sheet, firstActiveRow = nRows + 1, firstActiveCol = 4)
        addFilter(wb, sheet, rows = nRows, cols = 1:nCols)
        

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Export stations', return=TRUE)
    ## ----end

    ## 3. Add guidelines
    ## ---- guidelines
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))

        MMP_prepare_table_guidelines() %>% list2env(env = globalenv()) %>%
            suppressWarnings() %>%
            suppressMessages()
        sheet <- "Guidelines"
        addWorksheet(wb, sheet)
        headers <- typology %>% dplyr::select(measure) %>% t()
        nCols <- ncol(headers)
        nRows <- nrow(headers)
        writeData(wb, sheet, headers, colNames = FALSE, withFilter = FALSE)
        writeData(wb, sheet, wq.g, colNames = FALSE, withFilter = FALSE,
                  startRow = nRows+1)
        ## mergeCells(wb, sheet, cols = 4:5, rows = 1)
        ## mergeCells(wb, sheet, cols = 6:8, rows = 1)
        addStyle(wb, sheet, cols = 1:nCols, rows = 1:nRows,
                 style = createStyle(border = "TopBottomLeftRight",
                                     fgFill = "lightblue",
                                     halign = "center",
                                     borderColour = openxlsx_getOp("borderColour", "black"),
                                     borderStyle = openxlsx_getOp("borderStyle", "thick")),
                 gridExpand = TRUE)
        addFilter(wb, sheet, rows = nRows, cols = 1:nCols)
        setColWidths(wb, sheet, cols = 4:8, widths = "auto")
        freezePane(wb, sheet, firstActiveRow = nRows + 1, firstActiveCol = 1)

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Export guidelines', return=TRUE)
    ## ----end

    ## 4. Niskin summaries
    ## ---- niskin summaries
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))

        MMP_prepare_table_niskin_summaries() %>% list2env(env = globalenv()) %>%
            suppressWarnings() %>%
            suppressMessages()
        data.summary1 <- data.summary1 %>%
            mutate(across(everything(), ~replace(.x, is.nan(.x), NA)))  ## Remove any NaN values (replace with NA)
        sheet <- "Niskin summaries"
        addWorksheet(wb, sheet)
        redStyle <- createStyle(fgFill = "red")
        yellowStyle <- createStyle(fgFill = "yellow")
        for (av in c('Mean', 'Median')) {
            wchCols <- which(colnames(data.summary1) %in% av)
            wchRows <- which(cols[,wchCols] == 'red') + 1
            addStyle(wb, sheet, style = redStyle, rows = wchRows, cols = wchCols)
            wchRows <- which(cols[,wchCols] == 'yellow') + 1
            addStyle(wb, sheet, style = yellowStyle, rows = wchRows, cols = wchCols, gridExpand = TRUE)
        }
        
        addStyle(wb, sheet, cols = 1:15, rows = 1,
                 style = createStyle(border = "TopBottomLeftRight",
                                     fgFill = "lightblue",
                                     halign = "center",
                                     valign = "center",
                                     borderColour = openxlsx_getOp("borderColour", "black"),
                                     borderStyle = openxlsx_getOp("borderStyle", "thick")),
                 gridExpand = TRUE)
        setColWidths(wb, sheet, cols = 1:nCols, widths = "auto")
        writeData(wb, sheet, data.summary1) 
        freezePane(wb, sheet, firstActiveRow = 1, firstActiveCol = 4)


    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Export niskin summaries', return=TRUE)
    ## ----end


    ## 4. excel - indicators (historic)
    ## ---- excel - indicators (historic)
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))

        MMP_prepare_table_indicators() %>% list2env(env = globalenv()) %>%
            suppressWarnings() %>%
            suppressMessages()

        sheet <- "Indices - historical"
        addWorksheet(wb, sheet)
        headers <- typology %>% group_by(what) %>%
            mutate(rep = 1:n(),
                   Header1 = ifelse(max(rep) == 1 | (max(rep) > 1 & rep > 1), "", what),
                   Header2 = measure) %>%
            ungroup() %>% 
            dplyr::select(Header1, Header2)  %>%
            t()
        nCols <- ncol(headers)
        nRows <- nrow(headers)
        writeData(wb, sheet, headers, colNames = FALSE, withFilter = FALSE)
        writeData(wb, sheet, wq.idx[,2:20], colNames = FALSE, withFilter = FALSE,
                  startRow = 3)
        setColWidths(wb, sheet, cols = 2:19, widths = "auto")
        mergeCells(wb, sheet, cols = 3:9, rows = 1)
        mergeCells(wb, sheet, cols = 10:16, rows = 1)
        addStyle(wb, sheet, cols = 1:nCols, rows = 1:nRows,
                 style = createStyle(border = "TopBottomLeftRight",
                                     fgFill = "lightblue",
                                     halign = "center",
                                     valign = "center",
                                     borderColour = openxlsx_getOp("borderColour", "black"),
                                     borderStyle = openxlsx_getOp("borderStyle", "thick")),
                 gridExpand = TRUE)
        wchCols <- which(colnames(wq.idx[,2:20]) %in% 'Scaled')
        wchRows <- which(wq.idx[,wchCols+1] <= (-2/3)) + nRows
        addStyle(wb, sheet, style = Style_E, rows = wchRows, cols = wchCols)
        wchRows <- which(wq.idx[,wchCols+1] > (-2/3) & wq.idx[,wchCols+1] <= (-1/3)) + nRows
        addStyle(wb, sheet, style = Style_D, rows = wchRows, cols = wchCols)
        wchRows <- which(wq.idx[,wchCols+1] > (-1/3) & wq.idx[,wchCols+1] <= 0) + nRows
        addStyle(wb, sheet, style = Style_C, rows = wchRows, cols = wchCols)
        wchRows <- which(wq.idx[,wchCols+1] > 0 & wq.idx[,wchCols+1] <= (1/2)) + nRows
        addStyle(wb, sheet, style = Style_B, rows = wchRows, cols = wchCols)
        wchRows <- which(wq.idx[,wchCols+1] > (1/2)) + nRows
        addStyle(wb, sheet, style = Style_A, rows = wchRows, cols = wchCols)
        
        addFilter(wb, sheet, rows = nRows, cols = 1:nCols)
        freezePane(wb, sheet, firstActiveRow = nRows + 1, firstActiveCol = 3)
        ## saveWorkbook(wb, "../output/report/tables/mmp.xlsx", overwrite = TRUE)
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Export historical indices', return=TRUE)
    ## ----end

    ## 5. excel - indicators (version 0.1)
    ## ---- excel - indicators (version 0.1)
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))
        load(file = paste0(DATA_PATH, '/final/measure.site.year.RData'))
        wb <- MMP_historical_index_spreadsheet(wb, idx.df = measure.site.year, level = 1)
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Export version 0.1 indices', return=TRUE)
    ## ----end

    ## 6. excel - indicators (version 0.2)
    ## ---- excel - indicators (version 0.2)
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))
        load(file = paste0(DATA_PATH, '/final/measure.site.year.RData'))
        wb <- MMP_historical_index_spreadsheet(wb, idx.df = measure.site.year, level = 2)
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Export version 0.2 indices', return=TRUE)
    ## ----end

    ## 7. excel - indicators (version 0.3)
    ## ---- excel - indicators (version 0.3)
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))
        load(file = paste0(DATA_PATH, '/final/indicator.subregion.year.RData'))
        wb <- MMP_historical_index_spreadsheet(wb, idx.df = indicator.subregion.year, level = 3)
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Export version 0.3 indices', return=TRUE)
    ## ----end

    ## 8. excel - indicators (version 0.4)
    ## ---- excel - indicators (version 0.4)
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))
        load(file = paste0(DATA_PATH, '/final/indicator.region.year.RData'))
        wb <- MMP_historical_index_spreadsheet(wb, idx.df = indicator.region.year, level = 4)
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Export version 0.4 indices', return=TRUE)
    ## ----end

    ## 9. excel - indicators (version 6.1)
    ## ---- excel - indicators (version 0.5)
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))
        load(file = paste0(DATA_PATH, '/final/reneeYear.site.measure.RData'))
        wb <- MMP_version6_index_spreadsheet(wb, idx.df = reneeYear.site.measure, level = 1)
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Export version 6.1 indices', return=TRUE)
    ## ----end
    
    ## 10. excel - indicators (version 6.2)
    ## ---- excel - indicators (version 6.2)
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))
        load(file = paste0(DATA_PATH, '/final/reneeYear.subregion.measure.RData'))
        wb <- MMP_version6_index_spreadsheet(wb, idx.df = reneeYear.subregion.measure, level = 2)
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Export version 6.2 indices', return=TRUE)
    ## ----end

    ## 11. excel - indicators (version 6.3)
    ## ---- excel - indicators (version 6.3)
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))
        load(file = paste0(DATA_PATH, '/final/reneeYear.region.measure.RData'))
        wb <- MMP_version6_index_spreadsheet(wb, idx.df = reneeYear.region.measure, level = 3)
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Export version 6.3 indices', return=TRUE)
    ## ----end

    ## 12. excel - indicators (version 6.4)
    ## ---- excel - indicators (version 6.4)
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))
        load(file = paste0(DATA_PATH, '/final/reneeYear.subregion.subindicator.RData'))
        wb <- MMP_version6_index_spreadsheet(wb, idx.df = reneeYear.subregion.subindicator, level = 4)
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Export version 6.4 indices', return=TRUE)
    ## ----end

    ## 13. excel - indicators (version 6.5)
    ## ---- excel - indicators (version 6.5)
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))
        load(file = paste0(DATA_PATH, '/final/reneeYear.subregion.indicator.RData'))
        wb <- MMP_version6_index_spreadsheet(wb, idx.df = reneeYear.subregion.indicator, level = 5)
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Export version 6.5 indices', return=TRUE)
    ## ----end

    ## 14. excel - indicators (version 6.6)
    ## ---- excel - indicators (version 6.6)
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))
        load(file = paste0(DATA_PATH, '/final/reneeYear.region.indicator.RData'))
        wb <- MMP_version6_index_spreadsheet(wb, idx.df = reneeYear.region.indicator, level = 6)
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Export version 6.6 indices', return=TRUE)
    ## ----end


    ## 16. excel - FLNTU
    ## ---- excel - FLNTU 
    MMP_tryCatch(
    {
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))

        load(file=paste0(DATA_PATH, "/processed/loggers/flntu.all.daily.RData"))
        flntu.all.sum <- flntu.all.daily %>%
            rename(NTU=NTU_QA_AVG, DRIFTCHL_UGPERL.wm=CHL_QA_AVG) %>%
            gather(key=Measure, value=Value, NTU,DRIFTCHL_UGPERL.wm) %>%
            left_join(wq.guidelines %>%
                      dplyr:::filter(GL.Season=='Annual') %>%
                      left_join(names_lookup) %>%
                      dplyr:::select(MMP_SITE_NAME,Measure,GL,Latitude,Location,DirectionOfFailure) %>%
                      distinct %>%
                      spread(Location,GL)
                      ) %>%
            mutate(GL=rowSums(cbind(Mean,Median), na.rm=TRUE),
                   cwaterYear=MMP_categoricalWaterYear(Date)) %>%
            group_by(Region,Subregion,MMP_SITE_NAME,Latitude,cwaterYear,Measure) %>%
            summarize(
                N=n(),
                mean=mean(Value, na.rm=TRUE),
                se=SE(Value),
                median=median(Value, na.rm=TRUE),
                GL_mean=mean(Mean,na.rm=TRUE),
                GL_median=mean(Median,na.rm=TRUE),
                GT=ifelse(!is.na(GL_mean),countExceeds(Value,Mean,DirectionOfFailure),
                          countExceeds(Value,Median,DirectionOfFailure)),
                GT5=countExceeds(Value,rep(5,length(Mean)),DirectionOfFailure)
            ) %>% ungroup %>%
            gather(key=Temp, value=Value, N,mean,se,median,GL_mean, GL_median,GT,GT5) %>%
            unite(Temp,Measure,Temp) %>%
            spread(key=Temp, value=Value) %>%
            arrange(desc(Latitude),cwaterYear) %>%
            dplyr:::select(#Region,
                        Subregion,Site=MMP_SITE_NAME,Year=cwaterYear,
                        NTU_N, NTU_mean, NTU_se,NTU_median, NTU_GL_mean,NTU_GL_median,NTU_GT,NTU_GT5,
                        DRIFTCHL_UGPERL.wm_N,
                        DRIFTCHL_UGPERL.wm_mean, DRIFTCHL_UGPERL.wm_median, DRIFTCHL_UGPERL.wm_GL_mean,DRIFTCHL_UGPERL.wm_GL_median,DRIFTCHL_UGPERL.wm_GT
                    ) %>%
            mutate(NTU_N=sprintf('%1.0f',NTU_N))

        headers <- tribble(
            ~Header1, ~Header2,
            "Subregion", "Subregion",
            "Site",      "Site",
            "Year",      "Year",
            "NTU",       "N",
            "NTU",       "Annual Mean",
            "NTU",       "SE",
            "NTU",       "Annual Median",
            "NTU",       "%d > Trigger",
            "NTU",       "%d >5 Trigger",
            "Chl-a",       "N",
            "Chl-a",       "Annual Mean",
            ## "Chl-a",       "SE",
            "Chl-a",       "Annual Median",
            "Chl-a",       "%d > Trigger"
            ## "Chl-a",       "%d >5 Trigger"
        ) %>%
            dplyr::select(Header1, Header2)  %>%
            t()
        nCols <- ncol(headers)
        nRows <- nrow(headers)
        sheet <- "FLNTU"
        addWorksheet(wb, sheet)
        writeData(wb, sheet, headers, colNames = FALSE, withFilter = FALSE)
        dat <- flntu.all.sum %>%
            dplyr::select(!contains("_GL_")) 
        writeData(wb, sheet, dat, colNames = FALSE, withFilter = FALSE,
                  startRow = 3)
        setColWidths(wb, sheet, cols = 1:nCols, widths = "auto")
        mergeCells(wb, sheet, cols = 4:9, rows = 1)
        mergeCells(wb, sheet, cols = 10:13, rows = 1)
        for (cc in 1:3) mergeCells(wb, sheet, cols = cc, rows = 1:2)
        addStyle(wb, sheet, cols = 1:nCols, rows = 1:nRows,
                 style = createStyle(border = "TopBottomLeftRight",
                                     fgFill = "lightblue",
                                     halign = "center",
                                     valign = "center",
                                     borderColour = openxlsx_getOp("borderColour", "black"),
                                     borderStyle = openxlsx_getOp("borderStyle", "thick")),
                 gridExpand = TRUE)
        addStyle(wb, sheet, cols = 3:nCols, rows = 1:nrow(dat) + nRows,
                 gridExpand = TRUE,
                 style = createStyle(valign = "center"),
                 stack = TRUE
                 )
        for (wch in dat %>% colnames %>% str_extract('.*mean.*|.*median.*') %>% na.omit()) {
            wch.chem <- str_replace(wch, '(.*)_(mean|median)', '\\1')
            wch.av <- str_replace(wch, '.*_(mean|median)', '\\1')
            nm.gl <- paste0(wch.chem, "_", "GL_", wch.av)
            
            for (i in 1:nrow(dat)) {
                if (!is.na(flntu.all.sum[i,wch]) & !is.na(flntu.all.sum[i,nm.gl])) {
                    if (flntu.all.sum[i,wch] > flntu.all.sum[i,nm.gl]) {
                        j = which(colnames(dat) == wch)
                        addStyle(wb, sheet, style = Style_E, rows = i + nRows, cols = j, stack = TRUE)
                    }
                }

            }
            
        }
        addFilter(wb, sheet, rows = nRows, cols = 1:nCols)
        freezePane(wb, sheet, firstActiveRow = nRows + 1, firstActiveCol = 4)
 
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='Export FLNTU', return=TRUE)
    ## ----end

    saveWorkbook(wb, XLS_OUTPUT_FILE, overwrite = TRUE)

    ## 17. quarto - excel
    ## ---- quarto - excel 
    MMP_tryCatch(
    {
        h <- paste0("data:", mime::guess_type(XLS_OUTPUT_FILE), ";base64,",
                    base64_encode(XLS_OUTPUT_FILE))
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_1 = structure(paste0("## Excel\n"),
                                                        parent = 'TABSET'),
                            TEXT_1 = structure(paste0("\n<p>An excel workbook containing the following sheets can be directly downloaded via the clicking the button below. \n
<ul>\n
<li>sampling station info</li>\n
<li>guideline values</li>\n
<li>niskin summaries</li>\n
<li>indices - historical</li>\n
<li>indices - version 0.1</li>\n
<li>indices - version 0.2</li>\n
<li>indices - version 0.3</li>\n
<li>indices - version 0.4</li>\n
<li>indices - version 6.1</li>\n
<li>indices - version 6.2</li>\n
<li>indices - version 6.3</li>\n
<li>indices - version 6.4</li>\n
<li>indices - version 6.5</li>\n
<li>FLNTU</li>\n
</ul>\n
</p>\n"), 
                                                  parent = 'SUBSECTION_1'),
                               HTML_1 = structure(paste0("\n\n<a href = '",h,"' class = 'btn'>Download excel workbook</a>\n\n"), 
                                                  parent = 'SUBSECTION_1')
                               )
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Excel:', msg='embed in quarto', return=TRUE)
    ## ----end

    ## 18. quarto - zip
    ## ---- quarto - zip 
    MMP_tryCatch(
    {
##         ZIP_FILE <- paste0(OUTPUT_PATH, "/figures/Plots4Renee.zip")
##         h <- paste0("data:", mime::guess_type(ZIP_FILE), ";base64,",
##                     base64_encode(ZIP_FILE))
##         MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
##                                SUBSECTION_2 = structure(paste0("## Figures\n"),
##                                                         parent = 'TABSET'),
##                             TEXT_2 = structure(paste0("\n<p>An zip file containing the following figures can be directly downloaded via the clicking the button below. \n
## <ul>\n
## <li>sampling station info</li>\n
## <li>guideline values</li>\n
## <li>niskin summaries</li>\n
## <li>indices - historical</li>\n
## </ul>\n
## </p>\n"), 
##                                                   parent = 'SUBSECTION_2'),
##                                HTML_2 = structure(paste0("\n\n<a href = '",h,"' class = 'btn'>Download zip file</a>\n\n"), 
##                                                   parent = 'SUBSECTION_2')
##                                )
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Zip:', msg='embed zip in quarto', return=TRUE)
    ## ----end
    
    source("MMP_35_processedData_report.R")

    MMP_checkData(name = "mmp.xlsx",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "excel",
                  PATH = paste0(OUTPUT_PATH, "/"))
    MMP_openning_banner()
}
