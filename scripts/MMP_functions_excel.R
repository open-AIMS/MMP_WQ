## Excel spreadsheet functions ===========================================================

MMP_excel_styles <- function() {
    redStyle <<- createStyle(fgFill = "#ff0000", valign = "center")
    orangeStyle <<- createStyle(fgFill = "#ffc000", valign = "center")
    yellowStyle <<- createStyle(fgFill = "#ffff00", valign = "center")
    greenStyle <<- createStyle(fgFill = "#92d050", valign = "center")
    darkgreenStyle <<- createStyle(fgFill = "#00b050", valign = "center")
    Style_E <<- createStyle(fgFill = "#ff0000", valign = "center")
    Style_D <<- createStyle(fgFill = "#ffc000", valign = "center")
    Style_C <<- createStyle(fgFill = "#ffff00", valign = "center")
    Style_B <<- createStyle(fgFill = "#92d050", valign = "center")
    Style_A <<- createStyle(fgFill = "#00b050", valign = "center")
}

MMP_prepare_table_stations <- function() {
    PARAMS_INPUT_PATH <- paste0(DATA_PATH, "/primary/other/")
    wq.sites <- get(load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))) %>%
        arrange(-Latitude)

    lookup <- get(load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData')))
    names_lookup <- get(load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData')))

    lookup <- lookup %>% left_join(names_lookup)
    wq.sites <- wq.sites %>%
        left_join(lookup %>%
                  dplyr:::select(reef.alias,Region,Subregion)) %>%
        left_join(names_lookup)
    wq.sites <- wq.sites %>% dplyr:::select(Region,MMP_SITE_NAME,SHORT_NAME,
                                            `Logger FLNTU`,
                                            `Logger Salinity`, `Water (AIMS and JCU)`,
                                            `Water (AIMS MMP)`, `Water (JCU)`) %>% 
        mutate(across(c(`Logger FLNTU`, `Logger Salinity`, `Water (AIMS and JCU)`,
                        `Water (AIMS MMP)`, `Water (JCU)`),
                      .fns = function(x) ifelse(is.na(x), '', x)))
    
    mtch <- match(unique(wq.sites$Region),wq.sites$Region)[-1]-1
    typology <- data.frame(
        col_keys = c('Region','MMP_SITE_NAME','SHORT_NAME',
                   'Logger FLNTU', 'Logger Salinity',
                   'Water (AIMS and JCU)','Water (AIMS MMP)', 'Water (JCU)'),
        what = c('NRM Region','Site', 'GBRMPA name',
               rep('Logger',2),
               rep('Routine Niskin Sampling',3)),
        measure=c('NRM Region','Site', 'GBRMPA name',
                  'Turbidity / Chlorophyll', 'Salinity', 'AIMS and JCU', 'AIMS only', 'JCU only'),
        stringsAsFactors = FALSE )
    return(list(wq.sites = wq.sites,
                mtch = mtch,
                typology = typology))
}



MMP_prepare_table_guidelines <- function() {
    
    PARAMS_INPUT_PATH <- paste0(DATA_PATH, "/primary/other/")
    wq.sites <- get(load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))) %>%
        arrange(-Latitude)

    lookup <- get(load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData')))
    names_lookup <- get(load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData')))

    lookup <- lookup %>% left_join(names_lookup)
    load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))
    load(file=paste0(PARAMS_INPUT_PATH, 'wq.units.RData'))

   wq.g <- wq.guidelines %>%
        mutate(SHORT_NAME=gsub(',',', ',SHORT_NAME)) %>%
        dplyr:::left_join(wq.units %>%
                          dplyr:::select(Measure,Name.tab.abbr)) %>%
        unite(V,GL.Season,Location) %>%
        spread(key=V, value=GL) %>%
        mutate(Location=ifelse(!is.na(Dry_Median) | !is.na(Annual_Median),'median','mean')) %>%
        dplyr:::select(GBRMPA_group, SHORT_NAME,Name.tab.abbr,GBRMPA_water_area,
                       DirectionOfFailure,Annual_Mean,Annual_Median,Dry_Median,Wet_Median) %>%
        mutate(GBRMPA_group = factor(GBRMPA_group,
                                     levels=unique(wq.guidelines$GBRMPA_group))) %>%
        arrange(GBRMPA_group, Name.tab.abbr)
    
    wch <- is.na(wq.g$Annual_Mean)
    wq.g <- wq.g %>%
        mutate(across(c(Annual_Mean, Annual_Median, Dry_Median, Wet_Median),
                      .fns=function(x) ifelse(is.na(x),'',sprintf('% 5.2f',x))))
    mtch.group <- with(wq.g, match(unique(interaction(GBRMPA_group, SHORT_NAME)),
                                   interaction(GBRMPA_group, SHORT_NAME)))[-1]-1
    mtch <- match(unique(wq.g$SHORT_NAME),wq.g$SHORT_NAME)[-1]-1
    typology <- data.frame(
        col_keys = c('GBRMPA_group', 'SHORT_NAME','GBRMPA_water_area', 
                     'Name.tab.abbr', 'DirectionOfFailure', 'Annual_Mean','Annual_Median',
                     'Dry_Median', 'Wet_Median'),
        what = c('GBRMPA group', 'GBRMPA sites', 'Water body', 
                 'Measure', 'DOF', 'Annual Mean','Annual Median',
                 'Dry Median', 'Wet Median'),
        measure = c('GBRMPA group', 'GBRMPA sites', 'Water body', 
                    'Measure', 'DOF', 'Annual Mean','Annual Median',
                    'Dry Median', 'Wet Median'),
        stringsAsFactors = FALSE )
    return(list(wq.g = wq.g,
                mtch = mtch,
                mtch.group = mtch.group,
                typology = typology))
}

MMP_prepare_table_niskin_summaries <- function() {
    
    NISKIN_INPUT_PATH <- paste0(DATA_PATH, "/processed/niskin/")
    PARAMS_INPUT_PATH <- paste0(DATA_PATH, "/primary/other/")
    load(file=paste0(NISKIN_INPUT_PATH, 'wq.all.reef.RData'))
    load(file=paste0(PARAMS_INPUT_PATH, 'hierarchy.RData'))
    load(file=paste0(PARAMS_INPUT_PATH, 'wq.units.RData'))
    load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))
    load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData')) 

    niskin.tableData <- wq.all.reef
    niskin.tableData <- niskin.tableData %>%
        filter(reneeYear == reportYear) 
    niskin.tableData <- niskin.tableData %>% 
        left_join(hierarchy) %>%
        left_join(wq.units) 
    niskin.tableData.sum.reportYear <- niskin.tableData %>%
        group_by(Region, SHORT_NAME, MMP_SITE_NAME,
                 Measure, Name.tab.abbr, Name.latex.abbr) %>%
        summarize(N = n(),
                  Mean = mean(Value, na.rm=TRUE),
                  Median = median(Value,na.rm=TRUE),
                  ## DryMedian=median(Value[.$Season == 'Dry'],na.rm=TRUE),
                  ## WetMedian=median(Value[.$Season == 'Wet'],na.rm=TRUE),
                  'Q5' = quantile(Value, p=0.05, na.rm = TRUE),
                  'Q20' = quantile(Value, p=0.20, na.rm = TRUE),
                  'Q80' = quantile(Value, p=0.80, na.rm = TRUE),
                  'Q95' = quantile(Value, p=0.95, na.rm = TRUE)
                  #'Guideline'=mean(GL,na.rm=TRUE)
                  ) %>%
        left_join(wq.guidelines %>%
                  dplyr::select(MMP_SITE_NAME, Measure, GL,
                                DirectionOfFailure, Location,
                                GL.Season) %>%
                  distinct() %>%
                  spread(GL.Season,GL)) %>%
        ungroup() %>%
        left_join(wq.sites %>%
                  dplyr::select(SHORT_NAME, Latitude) %>%
                  distinct) %>%
        dplyr::mutate(Site = ifelse(is.na(SHORT_NAME),
                                    MMP_SITE_NAME,
                                    paste0(MMP_SITE_NAME,'(',SHORT_NAME,')'))) %>%
        dplyr::select(-MMP_SITE_NAME, -SHORT_NAME) %>%
        dplyr::select(Region, Site, everything()) %>%
        arrange(desc(Latitude), Site, Measure) %>%
        mutate(Region = factor(Region, levels = unique(Region))) %>%
        dplyr::select(-Latitude)

    niskin.tableData.sum <- niskin.tableData.sum.reportYear %>%
        ungroup %>%
        dplyr:::select(-Name.latex.abbr,-Measure) %>%
        rename(Measure = Name.tab.abbr)%>%
        as.data.frame()

    data.summary1 <- niskin.tableData.sum 
    data.summary1 = data.summary1 %>%
        mutate(Site=gsub('\\(',' (', Site))
    data.summary1 <- data.summary1[,is.na(colnames(data.summary1))==FALSE]
    data.summary1 <- data.summary1 %>%
        dplyr::select(-`<NA>`)

    is_triggered <- function(x,DoF,Annual, color = 'red') {
        ifelse(is.na(DoF), 'white',
        ifelse((DoF=='H' & x>Annual) |
               (DoF=='L' & x<Annual),
               color,'white'))
    }

    cols <- data.summary1 %>%
        ungroup() %>%
        mutate(Mean = ifelse(Location == 'Mean' &
                             !is.na(Annual),
                             is_triggered(as.numeric(Mean),
                                          DoF=.$DirectionOfFailure,
                                          Annual=as.numeric(.$Annual)),
                             'white')
              ,Median = ifelse(Location == 'Median' & !is.na(Annual),
                               is_triggered(as.numeric(Median), DoF = .$DirectionOfFailure,
                                            Annual = as.numeric(.$Annual)),
                               'white'),
               ## Now Wet and Dry only
              ,Median = ifelse(Location == 'Median' &  (Wet!='') &
                               (is.na(Annual) | Annual ==""),
                               is_triggered(as.numeric(.$Median), DoF=.$DirectionOfFailure,
                                            Annual=as.numeric(.$Wet), color='yellow'),
                               Median),
              ,Mean = ifelse(Location == 'Mean' &  (Wet!='') &
                             (is.na(Annual) | Annual ==""),
                             is_triggered(as.numeric(.$Mean), DoF=.$DirectionOfFailure,
                                          Annual=as.numeric(.$Wet), color='yellow'),
                             Mean),
              ,Median = ifelse(Location == 'Median' &  (Dry!='') &
                               (is.na(Annual) | Annual ==""),
                               is_triggered(as.numeric(.$Median), DoF=.$DirectionOfFailure,
                                            Annual=as.numeric(.$Dry), color='yellow'),
                               Median),
              ,Mean = ifelse(Location == 'Mean' &  (Dry!='') & (is.na(Annual) | Annual ==""),
                             is_triggered(as.numeric(.$Mean), DoF=.$DirectionOfFailure,
                                          Annual=as.numeric(.$Dry), color='yellow'),
                             Mean)
               )
    typology <- data.frame(
        col_keys=c('Region','Site','Measure','N','Mean','Median',
                   'Q5','Q20','Q80','Q95','DirectionOfFailure',
                   'Location','Annual','Dry','Wet'),
        what=c('Region','Site','Measure','N','Mean','Median',
               rep('Quantiles',4), rep('Guidelines',5)),
        measure=c('Region','Site','Measure','N','Mean','Median',
                  'Q05','Q20','Q80','Q95','DOF','Location','Annual','Dry','Wet'),
        stringsAsFactors = FALSE )
    mtch=with(data.summary1, match(unique(Site),Site))[-1]-1
    return(list(data.summary1 = data.summary1,
                cols = cols,
                mtch = mtch,
                typology = typology))
}

MMP_prepare_table_indicators <- function() {
    load(file = paste0(DATA_PATH, '/indices/wq.old.idx.RData'))
    load(file = paste0(DATA_PATH, '/indices/wq.old.qaqc1.RData'))
    wq.old.idx_calc <- wq.old.idx %>%
        mutate(Total = rowSums(cbind(DRIFTCHL_UGPERL.wm, CombinedTurb, PN.wm, PP.wm, NOx.wm))) %>%
        dplyr::rename(DRIFTCHL_UGPERL.idx = DRIFTCHL_UGPERL.wm,
                      PN.idx = PN.wm, PP.idx = PP.wm, NOx.idx = NOx.wm,
                      NTU.idx = NTU,SECCHI_DEPTH.idx = SECCHI_DEPTH.wm,
                      TSS_MGPERL.idx = TSS_MGPERL.wm) %>%
        left_join(wq.old.qaqc1 %>%
                  ungroup() %>% dplyr::select(MMP_SITE_NAME, Year, Measure, Value) %>%
                  spread(key = Measure, value = Value) %>%
                  dplyr::select(MMP_SITE_NAME, Year, DRIFTCHL_UGPERL.wm, PN.wm,
                                PP.wm, NOx.wm, NTU, SECCHI_DEPTH.wm, TSS_MGPERL.wm)) %>%
        mutate(DateRange = paste0(Year-3,'-',Year)) %>%
        dplyr:::select(Region, Reef = MMP_SITE_NAME, DateRange,
                       NOx.wm, PN.wm, PP.wm, DRIFTCHL_UGPERL.wm,
                       TSS_MGPERL.wm, SECCHI_DEPTH.wm, NTU,
                       NOx.idx, PN.idx, PP.idx, DRIFTCHL_UGPERL.idx,
                       TSS_MGPERL.idx, SECCHI_DEPTH.idx, NTU.idx,
                       CombinedTurb, Total, Scaled = Index)

    wq.idx <- wq.old.idx_calc
    score <- wq.idx$Scaled
    wq.idx <- wq.idx %>% mutate_if(is.numeric, funs(round(., 2)))
    mtch <- match(unique(wq.idx$Reef),wq.idx$Reef)[-1]-1
    typology <- data.frame(
        col_keys=c('Reef','DateRange',
                   'NOx.wm','PN.wm','PP.wm','DRIFTCHL_UGPERL.wm','TSS_MGPERL.wm','SECCHI_DEPTH.wm','NTU',
                   'NOx.idx','PN.idx','PP.idx','DRIFTCHL_UGPERL.idx','TSS_MGPERL.idx','SECCHI_DEPTH.idx','NTU.idx',
                   'CombinedTurb','Total','Scaled'),
        what=c('Site','Date Range',
               rep('Depth-weighted means',7),
               rep('Indicator scores',7),
               'Combined Turbidity','Total score','Scaled score'),
        measure=c('Site','Date Range',
                  'NOx','PN','PP','Chl a','SS','Secchi','Turbidity',
                  'NOx','PN','PP','Chl a','SS','Secchi','Turbidity',
                  'Combined Turbidity','Total score','Scaled score'),
        stringsAsFactors = FALSE )
    return(list(wq.idx = wq.idx,
                score = score,
                mtch = mtch,
                typology = typology))
}

MMP_historical_index_spreadsheet <- function(wb, idx.df, level) {
    switch(level,
    { # Level 1 (measure/site/year)
        idx <- idx.df %>%
            dplyr::select(Region, Subregion, Site = MMP_SITE_NAME, Short_name = SHORT_NAME, Year,
                          NOx.wm, PN.wm, PP.wm, DRIFTCHL_UGPERL.wm, TSS_MGPERL.wm, SECCHI_DEPTH.wm,
                          NTU, CombinedTurb) %>%
            pivot_longer(cols = c(NOx.wm, PN.wm, PP.wm, DRIFTCHL_UGPERL.wm, TSS_MGPERL.wm, SECCHI_DEPTH.wm,
                                  NTU, CombinedTurb),
                         names_to = 'Measure',
                         values_to = 'Index') %>%
            group_by(Site, Year) %>%
            mutate(Measure_order = 1:n()) %>%
            ungroup() %>% 
            left_join(wq.units %>% dplyr::select(Measure, Name.tab.abbr)) %>%
            mutate(Measure = ifelse(is.na(Name.tab.abbr), Measure, Name.tab.abbr),
                   Measure = factor(Measure, levels = unique(Measure))) %>%
            dplyr::select(-Name.tab.abbr) %>% 
            arrange(Region, Subregion, Year, Site, Measure) %>%
            mutate(Year = paste0(Year - 3, "-", Year)) %>%
            ## mutate(Index = scales::rescale(Index, from=c(-1,1), to = c(0,1))) %>%
            dplyr::select(-Measure_order)
        headers <- rbind(c('Region','Subregion','Site','Short_name','Date Range', 'Measure', 'Index'))
    },
    { # Level 2 (indicator/site/year)
        idx <- idx.df %>%
            dplyr::select(Region, Subregion, Site = MMP_SITE_NAME, Short_name = SHORT_NAME, Year,
                          Index = Index) %>% 
            arrange(Region, Subregion, Year, Site) %>%
            mutate(Year = paste0(Year - 3, "-", Year)) ## %>% 
        ## mutate(Index = scales::rescale(Index, from=c(-1,1), to = c(0,1)))
        headers <- rbind(c('Region','Subregion','Site','Short_name','Date Range', 'Index'))
    },
    { # Level 3 (indicator/subregion/year)
        idx <- idx.df %>%
            ungroup() %>% 
            left_join(lookup %>% dplyr::select(Region,Subregion) %>% distinct()) %>%
            dplyr::select(Region, Subregion, Year, Index = Index) %>% 
            arrange(Region, Subregion, Year) %>%
            mutate(Year = paste0(Year - 3, "-", Year)) ## %>% 
        ## mutate(Index = scales::rescale(Index, from=c(-1,1), to = c(0,1)))
        headers <- rbind(c('Region','Subregion','Date Range', 'Index'))
    },
    { # Level 4 (indicator/region/year)
        idx <- idx.df %>%
            ungroup() %>% 
            dplyr::select(Region, Year, Index = Index) %>% 
            arrange(Region, Year) %>%
            mutate(Year = paste0(Year - 3, "-", Year)) ## %>% 
        ## mutate(Index = scales::rescale(Index, from=c(-1,1), to = c(0,1)))
        headers <- rbind(c('Region','Date Range', 'Index'))
    }
    )
    nRows <- nrow(headers)
    nCols <- ncol(headers)
    ## wb <- createWorkbook()
    sheet <- paste0("Indices - historic.",level)
    addWorksheet(wb, sheet)
    writeData(wb, sheet, headers, startRow = 1, colNames = FALSE, borderColour = 'black') 
    writeData(wb, sheet, idx, startRow = nRows + 1, colNames = FALSE, borderColour = 'black') 
    ## setColWidths(wb, sheet, cols = 1:nCols, widths = "auto")
    addStyle(wb, sheet, cols = 1:nCols, rows = 1:nRows,
             style = createStyle(border = "TopBottomLeftRight",
                                 fgFill = "lightblue",
                                 halign = "center",
                                 valign = "center",
                                 borderColour = openxlsx_getOp("borderColour", "black"),
                                 borderStyle = openxlsx_getOp("borderStyle", "thick")),
             gridExpand = TRUE)
    addFilter(wb, sheet, rows = nRows, cols = 1:nCols)
    wchCols <- which(colnames(idx) == 'Index')
    for (grade in c('A','B','C','D','E')) {
        ## wchRows <- which(idx[,wchCols] == grade ) + nRows
        ## wchRows <- which(MMP_generateGrades(idx[,wchCols]) ==grade) + nRows
        wchRows <- which(MMP_generateOldGrades(idx[,wchCols]) ==grade) + nRows
        addStyle(wb, sheet,
                 style = eval(sym(paste0("Style_", grade))),
                 rows = wchRows, cols = wchCols, stack = TRUE)
    }
    freezePane(wb, sheet, firstActiveRow = nRows + 1, firstActiveCol = 1)
    ## saveWorkbook(wb, "../output/report/tables/historical_index1.xlsx", overwrite = TRUE)
    wb
}

MMP_version6_index_spreadsheet <- function(wb, idx.df, level = 1) {
    switch(level,
           { # Level 1 (measure/site/year)
               idx <- idx.df %>%
                   dplyr::select(Region, Subregion, Site = MMP_SITE_NAME, Year = reneeYear,
                                 Measure, Index = Score, Grade) %>%
                   group_by(Site, Year) %>%
                   mutate(Measure_order = 1:n()) %>%
                   ungroup() %>% 
                   left_join(wq.units %>% dplyr::select(Measure, Name.tab.abbr)) %>%
                   mutate(Measure = ifelse(is.na(Name.tab.abbr), Measure, Name.tab.abbr),
                          Measure = factor(Measure, levels = unique(Measure))) %>%
                   dplyr::select(-Name.tab.abbr) %>% 
                   arrange(Region, Subregion, Year, Site, Measure) %>%
                   mutate(Index = scales::rescale(Index, to=c(-1,1), from = c(0,1))) %>%
                   mutate(Grade=MMP_generateOldGrades(Index)) %>%
                   dplyr::select(-Measure_order)
               headers <- rbind(c('Region','Subregion','Site','Date Range', 'Measure',
                                  'Index','Grade'))
           },
           { # Level 2 (measure/subregion/year)
               idx <- idx.df %>%
                   dplyr::select(Region, Subregion, Year = reneeYear,
                                 Measure, Index = Score, Grade, Lower, Upper) %>%
                   group_by(Subregion, Year) %>%
                   mutate(Measure_order = 1:n()) %>%
                   ungroup() %>% 
                   left_join(wq.units %>% dplyr::select(Measure, Name.tab.abbr)) %>%
                   mutate(Measure = ifelse(is.na(Name.tab.abbr), Measure, Name.tab.abbr),
                          Measure = factor(Measure, levels = unique(Measure))) %>%
                   dplyr::select(-Name.tab.abbr) %>% 
                   arrange(Region, Subregion, Year, Measure) %>%
                   mutate(across(c(Index,Lower,Upper),
                                 function(x) scales::rescale(x, to=c(-1,1), from = c(0,1)))) %>%
                   mutate(Grade=MMP_generateOldGrades(Index)) %>%
                   dplyr::select(-Measure_order)
               headers <- rbind(c('Region','Subregion','Date Range', 'Measure',
                                  'Index','Grade','Lower','Upper'))
           },
           { # Level 3 (measure/region/year)
               idx <- idx.df %>%
                   dplyr::select(Region, Year = reneeYear,
                                 Measure, Index = Score, Grade, Lower, Upper) %>%
                   group_by(Region, Year) %>%
                   mutate(Measure_order = 1:n()) %>%
                   ungroup() %>% 
                   left_join(wq.units %>% dplyr::select(Measure, Name.tab.abbr)) %>%
                   mutate(Measure = ifelse(is.na(Name.tab.abbr), Measure, Name.tab.abbr),
                          Measure = factor(Measure, levels = unique(Measure))) %>%
                   dplyr::select(-Name.tab.abbr) %>% 
                   arrange(Region, Year, Measure) %>%
                   mutate(across(c(Index,Lower,Upper),
                                 function(x) scales::rescale(x, to=c(-1,1), from = c(0,1)))) %>%
                   mutate(Grade=MMP_generateOldGrades(Index)) %>%
                   dplyr::select(-Measure_order)
               headers <- rbind(c('Region','Date Range', 'Measure',
                                  'Index','Grade','Lower','Upper'))
           },
           { # Level 4 (subindicator/subregion/year)
               idx <- idx.df %>%
                   dplyr::select(Region, Subregion, Year = reneeYear,
                                 Subindicator, Index = Score, Grade, Lower, Upper) %>%
                   group_by(Subregion, Year) %>%
                   mutate(Measure_order = 1:n()) %>%
                   ungroup() %>% 
                   arrange(Region, Year, Subindicator) %>%
                   mutate(across(c(Index,Lower,Upper),
                                 function(x) scales::rescale(x, to=c(-1,1), from = c(0,1)))) %>%
                   mutate(Grade=MMP_generateOldGrades(Index)) %>%
                   dplyr::select(-Measure_order)
               headers <- rbind(c('Region','Subregion','Date Range', 'Subindicator',
                                  'Index','Grade','Lower','Upper'))
           },
           { # Level 5 (indicator/subregion/year)
               idx <- idx.df %>%
                   dplyr::select(Region, Subregion, Year = reneeYear,
                                 Indicator, Index = Score, Grade, Lower, Upper) %>%
                   group_by(Subregion, Year) %>%
                   mutate(Measure_order = 1:n()) %>%
                   ungroup() %>% 
                   arrange(Region, Year, Indicator) %>%
                   mutate(across(c(Index,Lower,Upper),
                                 function(x) scales::rescale(x, to=c(-1,1), from = c(0,1)))) %>%
                   mutate(Grade=MMP_generateOldGrades(Index)) %>%
                   dplyr::select(-Measure_order)
               headers <- rbind(c('Region','Subregion','Date Range', 'Indicator',
                                  'Index','Grade','Lower','Upper'))
           },
           { # Level 6 (indicator/region/year)
               idx <- idx.df %>%
                   dplyr::select(Region, Year = reneeYear,
                                 Indicator, Index = Score, Grade, Lower, Upper) %>%
                   group_by(Region, Year) %>%
                   mutate(Measure_order = 1:n()) %>%
                   ungroup() %>% 
                   arrange(Region, Year, Indicator) %>%
                   ## mutate(Index = scales::rescale(Index, to=c(-1,1), from = c(0,1))) %>%
                   mutate(across(c(Index,Lower,Upper),
                                 function(x) scales::rescale(x, to=c(-1,1), from = c(0,1)))) %>%
                   mutate(Grade=MMP_generateOldGrades(Index)) %>%
                   dplyr::select(-Measure_order)
               headers <- rbind(c('Region','Date Range', 'Indicator',
                                  'Index','Grade','Lower','Upper'))
           }
           )

    nRows <- nrow(headers)
    nCols <- ncol(headers)
    ## wb <- createWorkbook()
    sheet <- paste0("Indices - Version 6.",level)
    addWorksheet(wb, sheet)
    writeData(wb, sheet, headers, startRow = 1, colNames = FALSE, borderColour = 'black') 
    writeData(wb, sheet, idx, startRow = nRows + 1, colNames = FALSE, borderColour = 'black') 
    ## setColWidths(wb, sheet, cols = 1:nCols, widths = "auto")
    addStyle(wb, sheet, cols = 1:nCols, rows = 1:nRows,
             style = createStyle(border = "TopBottomLeftRight",
                                 fgFill = "lightblue",
                                 halign = "center",
                                 valign = "center",
                                 borderColour = openxlsx_getOp("borderColour", "black"),
                                 borderStyle = openxlsx_getOp("borderStyle", "thick")),
             gridExpand = TRUE)
    addFilter(wb, sheet, rows = nRows, cols = 1:nCols)
    wchCols <- which(colnames(idx) %in% c('Grade'))
    for (grade in c('A','B','C','D','E')) {
        ## wchRows <- which(idx[,wchCols] == grade ) + nRows
        wchRows <- which(idx[,wchCols] ==grade) + nRows
        addStyle(wb, sheet,
                 style = eval(sym(paste0("Style_", grade))),
                 rows = wchRows, cols = wchCols:(wchCols-1), stack = TRUE, gridExpand = TRUE)
    }
    freezePane(wb, sheet, firstActiveRow = nRows + 1, firstActiveCol = 1)
    ## saveWorkbook(wb, "../output/report/tables/historical_index1.xlsx", overwrite = TRUE)
    wb
}

############################################################
## The following function generates FLNTU summary tables. ##
## This only summarises NTU (not CHLA)                    ##
## Calculates (for each Reef, year):                      ##
##  - N (number of samples)                               ##
##  - Mean                                                ##
##  - SE (standard error of the mean)                     ##
##  - Median                                              ##
##  - gt (number of samples above the guidelines)         ##
##  - gt5 (number of samples above an NTU of 5)           ##
############################################################
countExceeds <- function(value,GL,DOF) {
    v=value
    value = v[!is.na(v) & !is.na(GL)]
    GL=GL[!is.na(v) & !is.na(GL)]
    if (length(value)==0 | all(is.nan(v)) | any(is.na(DOF))) {
        return(NA)
    } else {
        if (unique(DOF)=='H') cnt=100*length(value[value>GL])/length(value)
        else cnt=100*length(value[value<GL])/length(value)
        cnt
    }
}


SE <- function(x) {
    sd(x,na.rm=TRUE)/sqrt(length(x))
}
