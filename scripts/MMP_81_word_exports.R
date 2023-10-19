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
    file.exists(paste0(FLNTU_INPUT_PATH, 'flntu.all.sum.RData')) 
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
        load(file = paste0(FLNTU_INPUT_PATH, "flntu.all.sum.RData"))

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
        save_as_docx(values = flntu.tbl$tab, path = DOCx_OUTPUT_FILE, pr_section = sect_properties)
        
    },
    LOG_FILE, item = CURRENT_ITEM, Category = 'Word:', msg='Create the docx tables', return=TRUE)
    ## ----end


    
    MMP_checkData(name = "mmp.docx",
                  stage = paste0("STAGE", CURRENT_STAGE),
                  item = CURRENT_ITEM,
                  label = "word",
                  PATH = paste0(OUTPUT_PATH, "/"))
    MMP_openning_banner()
}
