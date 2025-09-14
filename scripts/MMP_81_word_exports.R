source("MMP_functions.R")
source("MMP_functions_models.R")
source("MMP_functions_word.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

FLNTU_INPUT_PATH <- paste0(DATA_PATH, "/processed/loggers/")
DOCX_OUTPUT_FILE <- paste0(OUTPUT_PATH, "/mmp.docx")

assign("CURRENT_STAGE", 9, env = globalenv())

## ---- Summary FLNTU tables
CURRENT_ITEM <- "word"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                       SECTION = paste0("# ", str_to_title(CURRENT_ITEM), "\n\n"),
                       TABSET = paste0("::: panel-tabset \n\n"),
                       TABSET_END = paste0("::: \n\n")
                       )


if ((alwaysExtract | !file.exists(paste0(DOCX_OUTPUT_FILE))) &
    file.exists(paste0(FLNTU_INPUT_PATH, 'flntu.all.daily.RData')) 
    ) {


    unlink(paste0(DATA_PATH, "/reports/STAGE",CURRENT_STAGE, "_", CURRENT_ITEM, "_.RData")) 
    MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                           SECTION = paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                                item = CURRENT_ITEM),"\n\n"),
                           TABSET = paste0("::: panel-tabset \n\n"),
                           TABSET_END = paste0("::: \n\n")
                           )
    ## 1. Create docx tables
    ## ---- create the docx tables
    MMP_tryCatch(
    {
        library(flextable)
        library(officer)

        load(file=paste0(FLNTU_INPUT_PATH, 'flntu.all.daily.RData'))

        
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
            mutate(GL=rowSums(cbind(Mean,Median), na.rm=TRUE),cwaterYear=MMP_categoricalWaterYear(Date)) %>%
            group_by(Region,Subregion,MMP_SITE_NAME,Latitude,cwaterYear,Measure) %>%
            summarize(
                N=n(),
                mean=mean(Value, na.rm=TRUE),
                se=SE(Value),
                median=median(Value, na.rm=TRUE),
                GL_mean=mean(Mean,na.rm=TRUE),
                GL_median=mean(Median,na.rm=TRUE),
                GT=ifelse(!is.na(GL_mean),countExceeds(Value,Mean,DirectionOfFailure),countExceeds(Value,Median,DirectionOfFailure)),
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
        

        ## Get the list of years
        Years <- flntu.all.sum %>% pull(Year) %>% unique()
        ## Get the indices of the years in groups of three from maximum
        wch <- seq(from = length(Years), to = 1, by = -3)
        flntu.tbl <- data.frame(Idx = wch) %>%
            mutate(To = Idx - 2) %>%
            filter(To > 0) %>%
            mutate(Cnt = 1:n()) %>%
            group_by(Idx, Cnt) %>%
            nest() %>%
            mutate(data = map(.x = Idx,
                              .f = ~flntu.all.sum %>%
                                  dplyr::select(-starts_with("DRIFTCHL")) %>%
                                  filter(Year %in% Years[.x:max(c(1,(.x-2)))]) %>%
                                  pivot_wider(id_cols = everything(),
                                              names_from =  Year,
                                              names_vary = "slowest",
                                              values_from = starts_with("NTU"))
                              ),
                   tab = map2(.x = data,
                              .y = Cnt,
                              .f = ~ MMP__docx_table(dat = .x, docx.tab.count = .y)
                              )
                   )
        ## flntu.tbl[1,'data'][[1]][[1]]
        ## dat <- flntu.tbl[1,'data'][[1]][[1]]
        ## tt <- MMP__docx_table(dat = dat, docx.tab.count = 1)
        ## dat <- flntu.tbl[2,'data'][[1]][[1]]
        ## tt2 <- MMP__docx_table(dat = dat, docx.tab.count = 2)
        ## dat <- flntu.tbl[3,'data'][[1]][[1]]
        ## tt3 <- MMP__docx_table(dat = dat, docx.tab.count = 3)

        sect_properties <- prop_section(
            page_size = page_size(
                orient = "landscape",
                width = 8.3, height = 11.7
            ),
            type = "continuous",
            page_margins = page_mar()
        )
        save_as_docx(values = flntu.tbl$tab, path = DOCX_OUTPUT_FILE, pr_section = sect_properties)
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Word:', msg='Create the docx tables', return=TRUE)
    ## ----end

    ## 2. Create docx niskin tables
    ## ---- create the niskin docx tables
    MMP_tryCatch(
    {
        library(flextable)
        library(officer)
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.sites.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'names_lookup.RData'))
        load(file=paste0(PARAMS_INPUT_PATH, 'wq.guidelines.RData'))

        MMP_prepare_table_niskin_summaries() %>% list2env(env = globalenv()) %>%
            suppressWarnings() %>%
            suppressMessages()
        data.summary1 <- data.summary1 %>%
            mutate(across(everything(), ~replace(.x, is.nan(.x), NA))) %>% ## Remove any NaN values (replace with NA)
          rename(DOF = DirectionOfFailure)

        data.summary1 <-
          data.summary1 %>%
          left_join(lookup %>%
                    mutate(Site = paste0(reef.alias, " (", SHORT_NAME, ")")) %>%
                    dplyr::select(Site, Subregion) %>%
                    distinct(),
                    by = "Site") |>
          dplyr::select(Region, Subregion, everything())

        sect_properties <- prop_section(
            page_size = page_size(
                orient = "landscape",
                width = 8.3, height = 11.7
            ),
            type = "continuous",
            page_margins = page_mar()
        )

        tab <- MMP__docx_table_niskin(dat =  data.summary1, cols,
                                      docx.tab.count = length(flntu.tbl$tab) + 1) 

        save_as_docx(values = append(flntu.tbl$tab, list("niskin" = tab)),
                     path = DOCX_OUTPUT_FILE,
                     pr_section = sect_properties)
        ## key_cols <- colnames(data.summary1)
        ## typology1 <- data.frame(
        ##   col_keys = key_cols,
        ##   what = c("Region", "Subregion", "Site", "Measure", "N", "Mean", "Median",
        ##            rep("Quantiles", 4), rep("Guideline Values", 5)
        ##            ),
        ##   measure = key_cols
        ## )
        
        ## ii <- 1:100
        ## a <- flextable(data = data.summary1[ii,]) %>%
        ##   set_header_df(mapping=typology1, key='col_keys') %>%
        ##   colformat_double(j =  1:11,
        ##                 digits=2, big.mark='') %>% 
        ##   merge_h(part = "header") %>%
        ##   merge_v(part = "header") %>%
        ##   merge_v(j=1:3, part = "body") %>%
        ##   theme_booktabs() %>%
        ##   flextable::align(i=1:2, align='center', part='header') %>%
        ##   flextable::align(j = -1:-4, align='center', part='body') %>%
        ##   flextable::align(j = 1:4, align='left', part='body') %>%
        ##   valign(j=1:3, valign='top') %>%
        ##   border_inner_v( border=fp_border(color="black")) %>%
        ##   border_outer( border=fp_border(color="black", width=2)) %>%
        ##   hline(part='body', j=-1:-3, border=fp_border(color='black')) %>%
        ##   hline(part='body', i=with(data.summary1[ii,], match(unique(Subregion),Subregion))[-1]-1,
        ##         border=fp_border(color='black')) %>%
        ##   hline(part='body', i=with(data.summary1[ii,], match(unique(Site),Site))[-1]-1,
        ##         border=fp_border(color='black')) %>%
        ##   fontsize(size = 8, part = "all") %>%
        ##   padding(padding=0, part='all') %>% 
        ##   width(j=1:16,
        ##         width=c(1,1,1,1,
        ##                 0.3,
        ##                 rep(0.6, 2),
        ##                 rep(0.6, 4),
        ##                 rep(0.4, 1),
        ##                 rep(0.6, 1),
        ##                 rep(0.6, 3))) %>%
        ##   fix_border_issues() %>% 
        ##   bg(j=6, i = ~ cols[ii,]$Mean == "yellow", bg = "yellow") %>% 
        ##   bg(j=6, i = ~ cols[ii,]$Mean == "red", bg = "red") %>% 
        ##   bg(j=7, i = ~ cols[ii,]$Median == "yellow", bg = "yellow") %>% 
        ##   bg(j=7, i = ~ cols[ii,]$Median == "red", bg = "red") %>% 
        ##   set_caption(paste("Table ",1,". Summary statistics for water quality parameters at individual monitoring sites from 1 October ", as.numeric(reportYear) - 1, " to 30 September ", reportYear, ". N = number of sampling occasions. See Section 2 for descriptions of each analyte and its abbreviation. Mean and median values that exceed available Water Quality Guidelines (DERM, 2009; Great Barrier Reef Marine Park Authority, 2010; State of Queensland, 2020) are shaded in red. Averages that exceed dry season guidelines are shaded in yellow. DOF is direction of failure ('H' = high values fail, while 'L' = low values fail)."))

        ## save_as_docx(values = list("a" = a),
        ##              path = DOCX_OUTPUT_FILE,
        ##              pr_section = sect_properties
        ##              )

    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Word:', msg='Create the niskin docx tables', return=TRUE)
    ## ----end

    ## ---- quarto - word 
    MMP_tryCatch(
    {
        h <- paste0("data:", mime::guess_type(DOCX_OUTPUT_FILE), ";base64,",
                    base64_encode(DOCX_OUTPUT_FILE))
        MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                               SUBSECTION_1 = structure(paste0("## Word\n"),
                                                        parent = 'TABSET'),
                            TEXT_1 = structure(paste0("\n<p>An word document containing the following tables. \n
<ul>\n
<li>FLNTU</li>\n
<li>Niskin</li>\n
</ul>\n
</p>\n"), 
                                                  parent = 'SUBSECTION_1'),
                               HTML_1 = structure(paste0("\n\n<a href = '",h,"' class = 'btn'>Download word document</a>\n\n"), 
                                                  parent = 'SUBSECTION_1')
                               )
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Word:', msg='embed in quarto', return=TRUE)
    ## ----end

    source("MMP_35_processedData_report.R")
    
    MMP_checkData(name = "mmp.docx",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "word",
                  PATH = paste0(OUTPUT_PATH, "/"))
    MMP_openning_banner()
}
