source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

## ---- PARAMS
CURRENT_ITEM <<- 'Parameter files'
unlink(paste0(DATA_PATH, "/reports/STAGE",CURRENT_STAGE, "_", "ParamFiles", "_.RData")) 
MMP_add_to_report_list(CURRENT_STAGE, 'ParamFiles',
                       SECTION = paste0("# ", CURRENT_ITEM, "\n\n"),
                               TABSET = paste0("::: panel-tabset \n\n"),
                               TABSET_END = paste0("::: \n\n")
                              )
## ----end

## ---- PARAMS wq.sites
CURRENT_ITEM <<- 'wq.sites'
wq.sites <- read_csv(paste0(PARAMS_PATH, '/wq.sites.csv'), trim_ws = TRUE) %>% suppressMessages()
MMP_add_to_report_list(CURRENT_STAGE, 'ParamFiles',
                       SUBSECTION_WQSITES = structure(paste0("# ", CURRENT_ITEM, "\n\n"),
                                              parent = 'TABSET'),
                               TAB_wq.sites = structure(mmp__add_table(wq.sites),
                                               parent = 'SUBSECTION_WQSITES'),
                               TAB_CAP.wq.sites = structure(paste0("\n:Water Quality Sites design lookup.  In particular, this parameter file descibes the mapping between GBRMPA groups and short names and AIMS reef.alias as well as which sites should have what type of samples. {#tbl-wqsites}\n\n"),
                                                   parent = 'SUBSECTION_WQSITES')
                              )
## MMP_get_report_list(CURRENT_STAGE, 'ParamFiles')
save(wq.sites, file=paste0(DATA_PATH, '/primary/other/wq.sites.RData'))
## ----end


## ---- PARAMS lookup
CURRENT_ITEM <<- 'lookup'
lookup <- read.csv(paste0(PARAMS_PATH, '/lookup.csv'), strip.white = TRUE) %>% suppressMessages()
MMP_add_to_report_list(CURRENT_STAGE, 'ParamFiles',
                       SUBSECTION_LOOKUP = structure(paste0("# ", CURRENT_ITEM, "\n\n"),
                                              parent = 'TABSET'),
                               TAB_lookup = structure(mmp__add_table(lookup),
                                               parent = 'SUBSECTION_LOOKUP'),
                               TAB_CAP.lookup = structure(paste0("\n:Water Quality Sites design lookup.  In particular, this parameter file descibes the mapping between GBRMPA groups and short names and AIMS reef.alias as well as which sites should have what type of samples. {#tbl-wqsites}\n\n"),
                                                   parent = 'SUBSECTION_LOOKUP')
                              )
## MMP_get_report_list(CURRENT_STAGE, 'ParamFiles')
save(lookup, file=paste0(DATA_PATH, '/primary/other/lookup.RData'))
## ----end

## ---- PARAMS names.lookup
CURRENT_ITEM <<- 'names.lookup'
names_lookup <- read.csv(paste0(PARAMS_PATH, '/names_lookup.csv'), strip.white = TRUE) %>% suppressMessages()
MMP_add_to_report_list(CURRENT_STAGE, 'ParamFiles',
                       SUBSECTION_NAMESLOOKUP = structure(paste0("# ", CURRENT_ITEM, "\n\n"),
                                              parent = 'TABSET'),
                               TAB_lookup = structure(mmp__add_table(names_lookup),
                                               parent = 'SUBSECTION_NAMESLOOKUP'),
                               TAB_CAP.lookup = structure(paste0("\n:Provides a mapping between `SHORT_NAME` and `MMP_SITE_NAME`. {#tbl-nameslookup}\n\n"),
                                                   parent = 'SUBSECTION_NAMESLOOKUP')
                              )
save(names_lookup, file=paste0(DATA_PATH, '/primary/other/names_lookup.RData'))
## ----end

## ---- PARAMS wq.guidelines
CURRENT_ITEM <<- 'wq.guidelines'
wq.guidelines <- read.table(paste0(PARAMS_PATH, '/wq.guidelines.txt'), header=TRUE, sep=';', strip.white = TRUE)
wq.guidelines <- wq.guidelines %>%
    mutate(SHORT_NAME = str_replace_all(SHORT_NAME, ',', ', '))
MMP_add_to_report_list(CURRENT_STAGE, 'ParamFiles',
                       SUBSECTION_WQGUIDELINES = structure(paste0("# ", CURRENT_ITEM, "\n\n"),
                                              parent = 'TABSET'),
                               TAB_wq.guidelines = structure(mmp__add_table(wq.guidelines),
                                               parent = 'SUBSECTION_WQGUIDELINES'),
                               TAB_CAP.wq.guidelines = structure(paste0("\n:Water Quality Sites design water quality guidelines.  In particular, this parameter file descibes the mapping between GBRMPA groups and short names and AIMS reef.alias as well as which sites should have what type of samples. {#tbl-wqsites}\n\n"),
                                                   parent = 'SUBSECTION_WQGUIDELINES')
                              )
## MMP_get_report_list(CURRENT_STAGE, 'ParamFiles')
wq.guidelines <- wq.guidelines %>%
    separate_rows(SHORT_NAME, sep=', ') %>%
    full_join(wq.sites) %>%
    left_join(names_lookup %>%
              dplyr::select(SHORT_NAME, MMP_SITE_NAME) %>%
              distinct()) %>%
    suppressMessages() %>%
    suppressWarnings()
save(wq.guidelines, file=paste0(DATA_PATH, '/primary/other/wq.guidelines.RData'))
## ----end

## ---- PARAMS old.wq.guidelines
CURRENT_ITEM <<- 'old.wq.guidelines'
old.wq.guidelines <- read.csv(paste0(PARAMS_PATH, '/old.wq.guidelines.csv'), strip.white=TRUE)
old.wq.guidelines <- old.wq.guidelines %>%
    mutate(SHORT_NAME = str_replace_all(MMP_SITE_NAME, ',', ', '))
MMP_add_to_report_list(CURRENT_STAGE, 'ParamFiles',
                       SUBSECTION_OLDWQGUIDELINES = structure(paste0("# ", CURRENT_ITEM, "\n\n"),
                                              parent = 'TABSET'),
                               TAB_old.wq.guidelines = structure(mmp__add_table(old.wq.guidelines),
                                               parent = 'SUBSECTION_OLDWQGUIDELINES'),
                               TAB_CAP.old.wq.guidelines = structure(paste0("\n:Water Quality Sites design water quality guidelines.  In particular, this parameter file descibes the mapping between GBRMPA groups and short names and AIMS reef.alias as well as which sites should have what type of samples. {#tbl-oldwqguidelines}\n\n"),
                                                   parent = 'SUBSECTION_OLDWQGUIDELINES')
                              )
## MMP_get_report_list(CURRENT_STAGE, 'ParamFiles')
old.wq.guidelines <- old.wq.guidelines %>%
    separate_rows(SHORT_NAME, sep=', ') %>%
    full_join(wq.sites) %>%
    left_join(names_lookup %>%
              dplyr::select(SHORT_NAME, MMP_SITE_NAME) %>%
              distinct()) %>%
    suppressMessages() %>%
    suppressWarnings()
save(old.wq.guidelines, file=paste0(DATA_PATH, '/primary/other/old.wq.guidelines.RData'))
## ----end

## ---- PARAMS river.lookup
CURRENT_ITEM <<- 'river.lookup'
river.lookup<-read.csv(paste0(PARAMS_PATH, "/river.gauge.correction.factors.csv"),
                       strip.white = TRUE)
MMP_add_to_report_list(CURRENT_STAGE, 'ParamFiles',
                       SUBSECTION_RIVERLOOKUP = structure(paste0("# ", CURRENT_ITEM, "\n\n"),
                                              parent = 'TABSET'),
                               TAB_river.lookup = structure(mmp__add_table(river.lookup),
                                               parent = 'SUBSECTION_RIVERLOOKUP'),
                               TAB_CAP.river.lookup = structure(paste0("\n:River discharge lookup table. In particular, this parameter file descibes the mapping between river discharge stations, river names, correction.factors and subregions. {#tbl-riverlookup}\n\n"),
                                                   parent = 'SUBSECTION_RIVERLOOKUP')
                              )
## MMP_get_report_list(CURRENT_STAGE, 'ParamFiles')

river.lookup <- river.lookup %>%
    mutate(Subregion=
               ifelse(subregion=='Cape York', 'Cape York',
               ifelse(subregion=='Daintree', 'Barron Daintree',
               ifelse(subregion=='Johnstone', 'Johnstone Russell Mulgrave',
               ifelse(subregion=='Tully', 'Tully Herbert',
               ifelse(subregion=='Burdekin','Burdekin',
               ifelse(subregion=='Proserpine','Mackay Whitsunday','Fitzroy')))))),
           Region = ifelse(Subregion %in% c('Barron Daintree',
                                            'Johnstone Russell Mulgrave',
                                            'Tully Herbert'),
                           'Wet Tropics', as.character(Subregion)),
           Subregion = factor(Subregion, levels = unique(Subregion)),
           Region = factor(Region, levels = unique(Region))
           )
save(river.lookup, file=paste0(DATA_PATH, '/primary/other/river.lookup.RData'))
## ----end

## ---- PARAMS LTmedian.discharge.river
CURRENT_ITEM <<- 'LTmedian.discharge.river'
discharge.baseline <- read.csv(paste0(PARAMS_PATH, "/LTmedian.discharge.river.csv"),
                               strip.white = TRUE)
MMP_add_to_report_list(CURRENT_STAGE, 'ParamFiles',
                       SUBSECTION_LTmedian = structure(paste0("# ", CURRENT_ITEM, "\n\n"),
                                              parent = 'TABSET'),
                               TAB_LTmedian = structure(mmp__add_table(discharge.baseline),
                                               parent = 'SUBSECTION_LTmedian'),
                               TAB_CAP.LTmedian = structure(paste0("\n:Long-term median river discharge data from each of the major rivers. {#tbl-ltmedian}\n\n"),
                                                   parent = 'SUBSECTION_LTmedian')
                              )
discharge.baseline <- discharge.baseline %>%
    mutate(River = ifelse(River=="O'Connell River", 'OConnell River',as.character(River)))
save(discharge.baseline, file=paste0(DATA_PATH, '/primary/other/discharge.baseline.RData'))

## ----end

## ---- PARAMS wq.units
CURRENT_ITEM <<- 'wq.units'
wq.units <- read.table(file=paste0(PARAMS_PATH, '/wq.units.txt'), header=TRUE, sep=';', strip.white=TRUE)
MMP_add_to_report_list(CURRENT_STAGE, 'ParamFiles',
                       SUBSECTION_wqunits = structure(paste0("# ", CURRENT_ITEM, "\n\n"),
                                              parent = 'TABSET'),
                               TAB_units.lookup = structure(mmp__add_table(wq.units),
                                               parent = 'SUBSECTION_wqunits'),
                               TAB_CAP.units.lookup = structure(paste0("\n:Units associated with each of the analytes. {#tbl-wqunits}\n\n"),
                                                   parent = 'SUBSECTION_wqunits')
                              )

save(wq.units, file=paste0(DATA_PATH, '/primary/other/wq.units.RData'))
## ----end

## ---- PARAMS hierarchy
CURRENT_ITEM <<- 'hierarchy'
hierarchy <- read.csv(file=paste0(PARAMS_PATH, '/hierarchy.csv'), strip.white=TRUE)
MMP_add_to_report_list(CURRENT_STAGE, 'ParamFiles',
                       SUBSECTION_hier = structure(paste0("# ", CURRENT_ITEM, "\n\n"),
                                              parent = 'TABSET'),
                               TAB_hier.lookup = structure(mmp__add_table(hierarchy),
                                               parent = 'SUBSECTION_hier'),
                               TAB_CAP.hier.lookup = structure(paste0("\n:Index aggregation hierarchy. {#tbl-hier}\n\n"),
                                                   parent = 'SUBSECTION_hier')
                              )

save(hierarchy, file=paste0(DATA_PATH, '/primary/other/hierarchy.RData'))
## ----end
