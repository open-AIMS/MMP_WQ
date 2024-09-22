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
