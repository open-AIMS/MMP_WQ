source("MMP_functions_boxes.R")
source("MMP_functions_processing.R")

#########################################################################
## The following function determines whether the current script is the ##
## parent (directly called from command line etc) or child (sourced    ##
## from another R script.                                              ##
## NOTE.  if we were to run sys.nframe() directly from a parent        ##
## script, it would return a value of 0.  However, since we are        ##
## calling sys.nframe() from a sourced script, then it will be 1       ##
#########################################################################
MMP_isParent <- function() {
   ifelse(sys.nframe()==1, TRUE, FALSE) 
}

MMP_fakeArgs <- function(stage = 1, always_extract = FALSE, reportYear = NULL) {
    MMP_startMatter(args = c('','','','','',
                             paste0('--reportYear=', reportYear),
                             paste0('--runStage=', paste0(stage)),
                             paste0('--alwaysExtract=', always_extract)))
}

#########################################################################
## The following function is a wrapper to a series of functions that:  ##
## - parse the command line arguments                                  ##
#########################################################################
MMP_startMatter <- function(args = commandArgs()) {
    MMP_initialise_status()    ## create the status list
    MMP_initialise_log()       ## create the log 
    MMP_loadPackages()         ## load required packages
    MMP_define_paths()         ## define the location of paths/files
    MMP_parseCLA(args)         ## parse command line arguments
    if (CURRENT_STAGE == 1) {
        ## clear data and outputs from previous runs
        MMP_clear_paths(paths = c('DATA_PATH', 'OUTPUT_PATH'))      
        ## clear DOCS_PATH files
        unlink(paste0(DOCS_PATH, paste0("/MMP_processData_report", c(".qmd", ".html"))))
        MMP_prepare_paths()    ## prepare file strjucture
    }
    MMP_openning_banner()
}


#########################################################################
## The following function provides a list of used to report run status ##
## and progress.  The list itself comprises:                           ##
##  - top level items represent the major status items that form       ##
##    status categories                                                ##
##  - within each list there are three items:                          ##
##     - title: a title to use in the status                           ##
##     - items: a vector or object names                               ##
##     - status: the status of the item (determines symbol)            ##
#########################################################################
MMP_initialise_status <- function() {
    STATUS <- list(
        SETTINGS = list(title = 'Create paths',
                      items = c('DATA_PATH', 'OUTPUT_PATH', 'PARAMS_PATH', 'DOCS_PATH'),
                      names = c('Data path', 'Output path', 'Parameters path', 'Documents path'),
                      status = c('pending', 'pending', 'pending', 'pending')
                      ),
        STAGE1 = list(title = "Stage 1 - prepare environment",
                      names = c("Load packages", "Parse command line args", "Prepare file system"),
                      items = c("Load packages", "Parse command line args", "Prepare file system"),
                      status = c("pending", "pending", "pending")
                      ),
        STAGE2 = list(title = "Stage 2 - extract data from DBs",
                      items = c("aimsNiskin","cairnsTransect","jcuNiskin","jcuCYNiskin",
                                "jcuEventNiskin","jcuCYEventNiskin",
                                "flntu", 
                                "waterTemp","salinity","dhd","disturbances", "tides","BOM","discharge",
                                "DataReport"),
                      names = c("AIMS niskin data","Cairns transect data","JCU niskin data","JCY CY niskin data",
                                "JCU Event niskin data","JCU CY Event niskin data",
                                "AIMS FLNTU loggers",
                                "Water temperaturej loggers","Salinity loggers",
                                "Degree heating weeks","Disturbance table", "Harmonic tides","BOM weather", "River discharge",
                                "Data report"),
                      status = c("pending","pending","pending","pending","pending",
                                 "pending","pending","pending","pending","pending","pending","pending","pending","pending","pending")
                      ),
        STAGE3 = list(title = "Stage 3 - process data",
                      items = c("aimsNiskin", "cairnsTransect", "jcuNiskin",
                                "jcuCYNiskin","jcuEventNiskin","jcuCYEventNiskin",
                                "flntu", "waterTemp", "salinity",
                                "dhw","disturbances","tides",
                                "BOM","discharge",
                                "timeseries","indicesData",
                                "gamData", "package"),
                      names = c("AIMS niskin data", "Cairns transect data","JCU niskin data",
                                "JCY CY niskin data","JCU Event niskin data","JCU CY Event niskin data",
                                "AIMS FLNTU loggers","Water temperature loggers","Salinity loggers",
                                "Degree heating weeks","Disturbance tables", "Harmonic tides",
                                "BOM weather", "River discharge",
                                "Compilation timeseries", "Prepare Indices data",
                                "Prepare GAMM data", "Package data"),
                      status = c("pending","pending","pending",
                                 "pending","pending","pending",
                                 "pending","pending","pending",
                                 "pending","pending","pending",
                                 "pending","pending",
                                 "pending", "pending",
                                 "pending", "pending"
                                 )
                      ),
        STAGE4 = list(title = "Stage 4 - fit GAMMs",
                      items = c("fitGAMM", "fitFLNTUGAMM",
                                "fitAIMSJCUGAMM", "fitAIMSJCUOMOGAMM"),
                      names = c("fit GAMMs", "fit FLNTU GAMMs",
                                "fit AIMS/JCU GAMMs", "fit AIMS/JCU OMO GAMMs"),
                      status = c("pending", "pending", "pending", "pending")
                      ),
        STAGE5 = list(title = "Stage 5 - calculate indices",
                      items = c("Type0", "Type1", "Type2", "Type3",
                                "Type4", "Type5", "Type6", "Comp"),
                      names = c("Type 0", "Type 1", "Type 2", "Type 3",
                                "Type 4", "Type 5", "Type 6", "Comparisons"),
                      status = c("pending", "pending", "pending", "pending",
                                 "pending", "pending", "pending", "pending")
                      ),
        STAGE6 = list(title = "Stage 6 - compilations",
                      items = c("GAMS", "GAMPages"),
                      names = c("GAMS", "GAM Pages"),
                      status = c("pending", "pending")),
        STAGE7 = list(title = "Stage 7 - transect plots",
                      items = c("Transects"),
                      names = c("Transects"),
                      status = c("pending")),
        STAGE8 = list(title = "Stage 8 - excel exports",
                      items = c("excel", "embed"),
                      names = c("excel exports", "embed in quarto"),
                      status = c("pending", "pending")),
        STAGE9 = list(title = "Stage 9 - word exports",
                      items = c("word"),
                      names = c("word exports"),
                      status = c("pending"))
        
    )
    assign("STATUS", STATUS, env = globalenv())
}

#########################################################################
## The following function parses the command line arguments            ##
## --finalYear            [a four digit number]                        ##
##    : the maximum reneeYear (beginning 1st September each year).     ##
##      This essentially defines the upper limit of data used in the   ##
##      analyses.                                                      ##
## --runStage             [an integer or vector of integers]           ##
##    : which stage is the analysis intending to run.                  ##
##      1. preparation stage, also performs a complete clearout        ##
## --alwaysExtract=TRUE   [boolean]                                    ##
##    : TRUE (default) - extracts all data from database and           ##
##                       overwrite any data existing in \data folder   ##
##      FALSE - extracts only data which is missing                    ##
#########################################################################
MMP_parseCLA <- function(args) {
    runStage <<- 1   ## this is a temp incase it is not specified on the command line - it is required for the openning banner
    CURRENT_STAGE <<- 1

    # Check all neccessary CLAs are present and no extra CLAs entered
    if(length(args) < 7) {
        MMP_log(status = "FAILURE", logFile = LOG_FILE, Category = "Parsing the command line arguments", msg=NULL) 
        mmp__change_status(stage = "STAGE1", item = "Parse command line args", status = "failure")
        MMP_openning_banner()
        stop(paste('This project must be run with command line arguments\nUsage: Rscript MMP_00_main.R --reportYear=<YEAR> --runStage=<vector of stage numbers> --alwaysExtract=<TRUE>'),
             call. = FALSE)
    }
    ## args <- commandArgs()

    ## report_year ==========================================================
    # Check report year is present, is numeric integer, has exactly four digits, ?<= current year + 1?
    report_year <- grep('--reportYear=.*', args) # index of reportYear in args
    if(length(report_year) == 0) {  # if index wasn't returned, reportYear is missing
        MMP_log(status = "FAILURE", logFile = LOG_FILE, Category = "Parsing the command line arguments", msg=NULL) 
        mmp__change_status(stage = "STAGE1", item = "Parse command line args", status = "failure")
        MMP_openning_banner()
        stop('A final report year must be supplied as a command line argument, such as: Rscript <script.R> --reportYear=2022', call. = FALSE)
    }
    reportYear <- args[report_year]  # get reportYear arg
    reportYear <- gsub('--reportYear=(.*)','\\1', reportYear) # subtract '--reportYear='
    if(!grepl("^\\d{4}$", reportYear)) {   # check if report year is exactly 4 digits'
        MMP_log(status = "FAILURE", logFile = LOG_FILE, Category = "Parsing the command line arguments", msg=NULL) 
        mmp__change_status(stage = "STAGE1", item = "Parse command line args", status = "failure")
        MMP_openning_banner()
        stop('A final report year must be a number with 4 digits, such as: Rscript <script.R> --reportYear=2022', call. = FALSE)
    }
    assign("reportYear", reportYear, env = globalenv()) # assign to global variable
    mmp__add_status(stage = "SETTINGS", item = "reportYear", name = "Report year", status = "success") # update status

    ## runStage ============================================================
    # Check if run stage is present, is numeric vector, ?all elements unique and in [1, maxStage]?
    runStage <- grep('--runStage=.*', args)   # return index of runStage in args
    if(length(runStage) == 0) { # if index wasn't returned, runStage is missing
        MMP_log(status = "FAILURE", logFile = LOG_FILE, Category = "Parsing the command line arguments", msg=NULL) 
        mmp__change_status(stage = "STAGE1", item = "Parse command line args", status = "failure")
        MMP_openning_banner()
        stop('A run stage must be supplied as a command line argument, such as: Rscript <script.R> --runStage=1', call. = FALSE)
    }
    # POSSIBLE ADDITION: change d to [1, max run stage], add '...must be an integer or vector of integers between 1 - max run stage
    runStage <- args[runStage] # get index of run stage from args
    runStage <- tryCatch( # parse and evaluate run stage to R code e.g. "1:2" --> c(1, 2)
        eval(parse(text = gsub('--runStage=(.*)','\\1', runStage))), 
        error = function(e) { # catch non-numeric run stages e.g. "A" --> Error: object A doesnt exist
            MMP_log(status = "FAILURE", logFile = LOG_FILE, Category = "Parsing the command line arguments", msg=NULL) 
            mmp__change_status(stage = "STAGE1", item = "Parse command line args", status = "failure")
            MMP_openning_banner()
            stop('Run stage must be an integer or vector of integers, such as: Rscript <script.R> --runStage=1:2', call. = FALSE)
        }
    )
    if(!all(grepl("^\\d{1}$", runStage))) { # check if all run stage vector entries are digits 
        MMP_log(status = "FAILURE", logFile = LOG_FILE, Category = "Parsing the command line arguments", msg=NULL) 
        mmp__change_status(stage = "STAGE1", item = "Parse command line args", status = "failure")
        MMP_openning_banner()
        stop('Run stage must be an integer or a vector of integers, such as: Rscript <script.R> --runStage=2:4', call. = FALSE)
    }
    assign("runStage", runStage, env = globalenv()) # create global variable for run stage
    assign("CURRENT_STAGE", runStage[1], env = globalenv()) # ... and current stage
    mmp__add_status(stage = "SETTINGS", item = "runStage", name = "Run stages", status = "success") # update status
    mmp__add_status(stage = "SETTINGS", item = "CURRENT_STAGE", name = "Current stage", status = "success")

    ## alwaysExtract =======================================================
    alwaysExtract <- grep('--alwaysExtract.*', args) # see if always extract was specified
    if (length(alwaysExtract) == 0) {# ... if not, default to TRUE
        alwaysExtract <- TRUE
    } else { # if so, set to TRUE or FALSE as appropriate, otherwise error
        alwaysExtract <- args[alwaysExtract]
        alwaysExtract <- gsub('--alwaysExtract=(.*)','\\1', alwaysExtract)
        if(alwaysExtract %in% c("FALSE", "false", "F", "f")) {
            alwaysExtract <- FALSE
        } 
        else if(alwaysExtract %in% c("TRUE", "true", "T", "t")) {
            alwaysExtract <- TRUE
        }
        else {
            MMP_log(status = "FAILURE", logFile = LOG_FILE, Category = "Parsing the command line arguments", msg=NULL) 
            mmp__change_status(stage = "STAGE1", item = "Parse command line args", status = "failure")
            MMP_openning_banner()
            stop('Always extract must be either TRUE or FALSE (default = TRUE), such as: Rscript <script.R> --alwaysExtract=FALSE', call. = FALSE)
        }
    }
    assign("alwaysExtract", alwaysExtract, env = globalenv()) # create global always extract variable
    mmp__add_status(stage = "SETTINGS", item = "alwaysExtract", name = "Always extract", status = "success") # update status
    
    mmp__change_status(stage = "STAGE1", item = "Parse command line args", status = "success")
    MMP_log(status = "SUCCESS", logFile = LOG_FILE, Category = "Parsing the command line arguments", msg=NULL) 

}

####################################################################
## The following function checks to ensure that all the required  ##
## packages are available on the system.                          ##
##                                                                ##
## NOTE - we could run code to check whether a package is present ##
## or not and tif it is not, then install the package.  However,  ##
## this will install the package in the user .libPath rather than ##
## system .libPath and this could lead to multiple                ##
## packages/versions in multiple locations.                       ##
####################################################################
MMP_loadPackages <- function(log = TRUE) {           
    missing <- ''
    options(tidyverse.quiet = TRUE)
    pkgs <- c('tidyverse', 'mgcv', 'testthat','cli','rlang','crayon',
              'assertthat', 'lubridate', 'rmarkdown','bookdown', 'ggh4x',
              'furrr','reportcards', 'emmeans', 'openxlsx', 'xfun', 'ncdf4',
              'kableExtra'
              )

    for (p in pkgs) {
        ## unforunately we must do this the base r way until rlang is
        ## loaded
        eval(parse(text=paste0("suppressPackageStartupMessages(if(!require(",
                               p,",quietly = TRUE, warn.conflicts = FALSE)) missing <- c(missing, ",
                               p,"))"))) 
    }


    if(missing!="") { 
        MMP_log(status = "FAILURE",
                logFile = LOG_FILE,
                Category = "Loading the necessary R packages",
                msg=NULL) 
        mmp__change_status(stage = "STAGE1", item = "Load packages", status = "failure")
        MMP_openning_banner()
        stop(paste('The following required package(s) are missing: ',paste(missing, collapse=', ')))
    } else {
        mmp__change_status(stage = "STAGE1", item = "Load packages", status = "success")
        MMP_log(status = "SUCCESS",
                logFile = LOG_FILE,
                Category = "Loading the necessary R packages",
                msg=NULL) 
    }
}

#########################################################################
## The following function defines the location of major folders,       ##
##  subfolders and files created in the running of this codebase.      ##
#########################################################################
MMP_define_paths <- function() {
    ## location of folder containing R data objects
    DATA_PATH <<- '../data'
    mmp__change_status(stage = "SETTINGS", item = "DATA_PATH", status = "success")
    ## location of folder containing perpetual data used in this project
    PARAMS_PATH <<- '../parameters'
    mmp__change_status(stage = "SETTINGS", item = "PARAMS_PATH", status = "success")
    ## location of folder containing outputs (individual figures and tables)
    OUTPUT_PATH <<- '../outputs'
    mmp__change_status(stage = "SETTINGS", item = "OUTPUT_PATH", status = "success")
    ## location of folder containing generated documents 
    DOCS_PATH <<- '../docs'
    mmp__change_status(stage = "SETTINGS", item = "DOCS_PATH", status = "success")

}

#########################################################################
## The following function initialises a log file.  This log file is    ##
## placed in the root of the project as it needs to be in a location   ##
## that is guarenteed to exist from a freshly cloned instance of this  ##
## codebase.                                                           ##
#########################################################################
MMP_initialise_log <- function() {
    ##Log file
    LOG_FILE <<- paste0("../.mmp.log")
    if (file.exists(LOG_FILE)) unlink(LOG_FILE)
    mmp__add_status(stage = "SETTINGS", item = "LOG_FILE", name = "Log file", status = "success")
}

eval_parse <- function(x) {
    eval(parse(text = paste0(x)))
}

#########################################################################
## The following function deletes all dynamically generated folders    ##
## and files that are produced in the running of this codebase         ##
## effectively returning the filesystem to a pre-run state in          ##
## preparation for a clean run start.                                  ##
#########################################################################
MMP_clear_paths <- function(paths = c('DATA_PATH', 'OUTPUT_PATH')) {
    for (d in paths) {
        if (dir.exists(eval_parse(d))) 
            unlink(eval_parse(d), recursive = TRUE)
    }
}


#########################################################################
## The following function deletes all dynamically generated folders    ##
## and files that are produced in the running of this codebase         ##
## effectively returning the filesystem to a pre-run state in          ##
## preparation for a clean run start.                                  ##
#########################################################################
MMP_prepare_paths <- function() {
    if (!dir.exists(DATA_PATH)) dir.create(DATA_PATH)
    if (!dir.exists(paste0(DATA_PATH, '/primary')))
        dir.create(paste0(DATA_PATH, '/primary'))
    if (!dir.exists(paste0(DATA_PATH, '/primary/niskin')))
        dir.create(paste0(DATA_PATH, '/primary/niskin'))
    if (!dir.exists(paste0(DATA_PATH, '/primary/loggers')))
        dir.create(paste0(DATA_PATH, '/primary/loggers'))
    if (!dir.exists(paste0(DATA_PATH, '/primary/other')))
        dir.create(paste0(DATA_PATH, '/primary/other'))

    if (!dir.exists(paste0(DATA_PATH, '/processed')))
        dir.create(paste0(DATA_PATH, '/processed'))
    if (!dir.exists(paste0(DATA_PATH, '/processed/niskin')))
        dir.create(paste0(DATA_PATH, '/processed/niskin'))
    if (!dir.exists(paste0(DATA_PATH, '/processed/loggers')))
        dir.create(paste0(DATA_PATH, '/processed/loggers'))
    if (!dir.exists(paste0(DATA_PATH, '/processed/other')))
        dir.create(paste0(DATA_PATH, '/processed/other'))
    if (!dir.exists(paste0(DATA_PATH, '/models')))
        dir.create(paste0(DATA_PATH, '/models'))
    if (!dir.exists(paste0(DATA_PATH, '/indices')))
        dir.create(paste0(DATA_PATH, '/indices'))
    if (!dir.exists(paste0(DATA_PATH, '/reports')))
        dir.create(paste0(DATA_PATH, '/reports'))
    if (!dir.exists(paste0(DATA_PATH, '/final')))
        dir.create(paste0(DATA_PATH, '/final'))

    if (!dir.exists(OUTPUT_PATH)) dir.create(OUTPUT_PATH)
    if (!dir.exists(paste0(OUTPUT_PATH, '/tables')))
        dir.create(paste0(OUTPUT_PATH, '/tables'))
    if (!dir.exists(paste0(OUTPUT_PATH, '/figures')))
        dir.create(paste0(OUTPUT_PATH, '/figures'))
    if (!dir.exists(paste0(OUTPUT_PATH, '/figures/processed')))
        dir.create(paste0(OUTPUT_PATH, '/figures/processed'))
    if (!dir.exists(paste0(OUTPUT_PATH, '/figures/indices')))
        dir.create(paste0(OUTPUT_PATH, '/figures/indices'))
    if (!dir.exists(paste0(OUTPUT_PATH, '/figures/models')))
        dir.create(paste0(OUTPUT_PATH, '/figures/models'))

    if (!dir.exists(DOCS_PATH)) dir.create(DOCS_PATH)

    mmp__change_status(stage = "STAGE1", item = "Prepare file system", status = "success")

    MMP_log(status = "SUCCESS", logFile = LOG_FILE, Category = "Preparing file system", msg=NULL) 
}


#########################################################################
## The following function generates an informative banner that         ##
## displays the a range of settings associated with the analysis       ##
#########################################################################
MMP_openning_banner <- function(){
    system('clear')
    cat(" ")
    currentTime <- format(Sys.time(),'%d/%m/%Y %H:%M:%S')
    ## maxStringLength <- max(nchar(c(
    ##     DATA_PATH,
    ##     PARAMS_PATH,
    ##     OUTPUT_PATH,
    ##     DOCS_PATH,
    ##     reportYear,
    ##     currentTime)))
    ## cat(paste0(
    ##     paste0(rep('*', 19 + maxStringLength + 1), collapse = ''), '*\n',
    ##     '* Data path:       ', DATA_PATH, paste0(rep(' ', maxStringLength-nchar(DATA_PATH)), collapse=''), ' *\n',
    ##     '* Parameters path: ', PARAMS_PATH, paste0(rep(' ', maxStringLength-nchar(PARAMS_PATH)), collapse=''), ' *\n',
    ##     '* Output path:     ', OUTPUT_PATH, paste0(rep(' ', maxStringLength-nchar(OUTPUT_PATH)), collapse=''), ' *\n',
    ##     '* Docs path:       ', DOCS_PATH, paste0(rep(' ', maxStringLength-nchar(DOCS_PATH)), collapse=''), ' *\n',
    ##     '* Report Year:     ', reportYear, paste0(rep(' ', maxStringLength-nchar(reportYear)), collapse=''), ' *\n',
    ##     '* Date:            ', currentTime, paste0(rep(' ', maxStringLength-nchar(currentTime)), collapse=''), ' *\n',
    ##   paste0(rep('*',19 + maxStringLength + 2),collapse=''),'\n',
    ##   collapse=''
    ## ))

    ## Leave this as is
    STATUS$SETTINGS$items <- c(STATUS$SETTINGS$items, 'currentTime')
    STATUS$SETTINGS$names <- c(STATUS$SETTINGS$names, 'Date/Time')
    STATUS$SETTINGS$status <- c(STATUS$SETTINGS$status, 'success')
    
    box.style <- cli:::box_styles()
    box.width <- 80
    box.margins <- 1
  
    ## get the width of the path box
    settings.box.nchar <-nchar(
        paste0(STATUS$SETTINGS$names, ': ',sapply(STATUS$SETTINGS$items, function(x) eval(parse(text = x))))
    )
    settings.box.width <- max(settings.box.nchar) +
        2 +              # add one for the status character
        box.margins*2    # add the left and right margin
    
    ## Outer box (top)
    top <- mmp__outerBox.top(box.width, settings.box.width)
    ## cat(top)

    ## Settings box
    settings.box.text <- mmp__settingsBox(settings = STATUS$SETTINGS,
                                     box.width = settings.box.width,
                                     box.nchar = settings.box.nchar,
                                     box.margins = box.margins,
                                     currentTime)
    
    ## Main box
    main.box.text <- mmp__mainBox(settings.box.text,
                                  box.width,
                                  settings.box.width,
                                  box.margins)

    ## Outer box (bottom)
    bottom <- mmp__outerBox.bottom(box.width, settings.box.width)
    
    ## bottom <- paste0(box.style["double", "bottom_left"],
    ##                  strrep(box.style["double", "horizontal"], settings.box.width),
    ##                  '\u2567',
    ##                  strrep(box.style["double", "horizontal"], box.width - settings.box.width),
    ##                  box.style["double", "bottom_right"],
    ##                  "\n"
    ##                  )

    ## Combine boxes
    combined.boxes.text <- mmp__combinedBoxes(
        top,
        settings.box.text,
        main.box.text,
        bottom,
        box.width,
        settings.box.width,
        box.margins)

    cat(combined.boxes.text)
    ## for (i in 1:max(length(settings.box.text), length(main.box.text))) {
    ##     cat(paste0(
    ##         ifelse(i>length(settings.box.text),
    ##                cli::ansi_align("", width = box.width - settings.box.width - 1, align = 'center'),
    ##                settings.box.text[i]),
    ##         ifelse(i>length(main.box.text),
    ##                cli::ansi_align("", width = box.width - settings.box.width - 1, align = 'center'),
    ##                main.box.text[i]),
    ##         "\u2551",
    ##         "\n"))
    ## }
    
    ## log box
    log.box <- mmp__logBox(box.width, box.margins)
    cat(log.box) 
}

mmp__add_status <- function(stage, item, name, status) {
    STATUS[[stage]]$items <- c(STATUS[[stage]]$items, item)
    STATUS[[stage]]$names <- c(STATUS[[stage]]$names, name)
    STATUS[[stage]]$status <- c(STATUS[[stage]]$status, status)
    assign("STATUS", STATUS, env = globalenv())
}

mmp__change_status <- function(stage, item, status) {
    STATUS[[stage]]$status[which(STATUS[[stage]]$item == item)] <- status
    assign("STATUS", STATUS, env = globalenv())
}

mmp__change_name <- function(stage, item, name) {
    STATUS[[stage]]$names[which(STATUS[[stage]]$item == item)] <- name
    assign("STATUS", STATUS, env = globalenv())
}
mmp__append_filesize <- function(stage, item, label, filepath) {
    filesize <- R.utils::hsize(file.size(paste0(filepath)))
    mmp__change_name(stage = stage, item = item, name = paste0(label, "  [",filesize, "]"))
}

mmp__get_name <- function(stage, item) {
    str_replace(STATUS[[stage]]$names[which(STATUS[[stage]]$item == item)],
                '(.*) \\[.*\\]',
                '\\1')
}

MMP_test <- function() {
    for (i in 1:10) {
        for (j in 1:100000000) {
        }
        MMP_openning_banner()
        cat(paste0(i,'\n'))
        if (i > 5) STATUS$SETTINGS$status[4] <<- 'failure'
        }
    }

####################################################################################
## The following function writes out log information to a file named by the       ##
## LOG_FILE global variable.                                                      ##
## Arguments:                                                                     ##
## - log_status:     a string indicating either 'FAILURE',  'SUCCESS',            ##
##              'WARNING' or 'INFO                                                ##
## - logFile:    a character string representation of the log file name           ##
##               (including path relative to the current working director)        ##
## - Category:   a character string with a category to appear verbatim in the log ##
## - success:    boolean or string. One of TRUE (for success), 'WARNING'          ##
##               (for warnings) or anything else for a failure                    ##
## - msg:        the message (as a string) to appear verbatim in the log          ##
####################################################################################
MMP_log <- function(status, logFile = LOG_FILE, Category, msg=NULL) {
    d <- dirname(logFile)
    files <- list.files(d)
    ## print(files)
    if(!any(grepl(paste0('^',logFile,'$'),files))) system(paste0('touch "',logFile,'"'))
    now <- Sys.time()
    options(digits.secs=2)              ## switch to subsecond display
    msg = paste0(now, '|', status, ': ', Category, ' ', msg)
    ## cat(paste0(msg,'\n'))
    if (!is.null(msg)) {
        write(msg,  file=paste0(logFile), append=TRUE)
    }
}


##########################################################################                                               
## The following function provides a more useful error handling         ##                                               
## routine.                                                             ##                                               
##    expr:      an R expression to be evaluated                        ##                                               
##    logFile:   a character string represetnation of the log file name ##                                               
##               (including path relative to the current working        ##                                               
##               directory)                                             ##                                               
##    Category:  a character string representation of error category    ##                                               
##    msg:       a character string with a message to appear verbatim   ##                                               
##               in the log                                             ##                                               
##    return:    boolean, whether to return a TRUE or FALSE             ##
##    progressive: denotes whether we have a finished extracting a      ## 
##               dataset (FALSE) or further steps remain (TRUE) -       ##
##               if FALSE, appends filesize to console status           ##                                           
##########################################################################  
MMP_tryCatch <- function(expr, logFile, item, Category, expectedClass=NULL, msg=NULL, return=NULL, showWarnings=FALSE) {
    if (!exists('PROGRESS')) PROGRESS=NULL
    max.warnings<-4
    warnings<-0
    W <- NULL
    w.handler <- function(w) { # warning handler                                                                          
        m<-w$message
        if ((warnings < max.warnings) && (grepl ('MMP_WARNING', m)>0)) {
            MMP_log('WARNING', logFile, Category, paste(warnings, msg, m))
            warnings<<-warnings+1
        }
        invokeRestart("muffleWarning")
    }
    ## ret <- list(value = withCallingHandlers(tryCatch(expr,
    ##                                                  error = function(e) e,
    ##                                                  warning = function(w) w,
    ##                                                  message = function(m) m),
    ##                                         warning = w.handler),warning = W)
    ret <- list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                            warning = w.handler),warning = W)
    ## if(!is.atomic(ret$value) && !is.null(ret$value$message)){
    ## print(!is.atomic(ret$value))
    ## print(any(class(ret$value) %in% c("simpleError", "error", "rlang_error")))
    if(!is.atomic(ret$value) && any(class(ret$value) %in% c("simpleError", "error", "rlang_error"))){
        ## An error occurred
        PROGRESS <<- c(PROGRESS,'Fail')
        class(ret) <- "try-error"
        MMP_log('ERROR', logFile, Category, paste(msg, ret$value$message))
        ## mmp__change_status(stage = CURRENT_STAGE, item = item, status = "failure")
        if(!is.null(return)) {
            FALSE
        }else {
            if (DEBUG_MODE) {
                "An error occured, please refer to the status line above..."
            } else {
                quit(status=-1,save="no")
            }
        }
    } else {    #no error check for warning
        PROGRESS <<- c(PROGRESS,'Pass')
        MMP_log('SUCCESS', logFile, Category, msg)
        if(!is.null(return)) {
            TRUE
        }
    }
    MMP_openning_banner()
}




############################################################################
## MMP_tryCatch_db logic:                                                 ##
##                                                                        ##
##  If alwaysExtract OR file ! exist:                                     ##
##      --> Get data                                                      ##
##          --> Error: update log/status *END*                            ##
##          --> Success: update log/status                                ##
##              --> Check file exists:                                    ##
##                  --> file ! exist: update log/status *END*             ##
##                  --> file exists: update log/status                    ##
##                      --> if ! progressive: append file size to console ##
##      *END*                                                             ##
##  Else !alwaysExtract AND file exists                                   ##
############################################################################
MMP_tryCatch_db <- function(name = 'niskin',
                            stage = "STAGE2",
                            item = "aimsNiskin",
                            label = "AIMS niskin",
                            PATH = NISKIN_PATH,
                            db_user = "wq_nut2", 
                            progressive=TRUE) {
    extract_msg <- paste0("Extracting ", label, " data from database")
    sql_file <- paste0(PATH, name, ".sql") # sql instructions stored here
    data_file <- paste0(PATH, name, ".csv") # save data here
    tryCatch(
        {   ## Try extract data using sql instructions and  and save to data file
            status <- system2(
                "java", stdout = TRUE, stderr = TRUE,
                args = paste("-jar dbExport.jar", sql_file, data_file, db_user) 
            )
            ## Check for errors returned by the database and update log & status accordingly
            if (stringr::str_detect(status[6], 'Error')) {
                extract_error_msg <- paste0(
                    MSG, ": There is a problem with the SQL: ", 
                    stringr::str_replace(status[8], "java.sql.SQLSyntaxErrorException: (.*)", "\\1")
                )
                MMP_log("FAILURE", LOG_FILE, Category = extract_error_msg) 
                mmp__change_status(stage, item, status = "failure")
            } 
            else {
                MMP_log("SUCCESS", LOG_FILE, Category = extract_msg) 
                mmp__change_status(stage, item, status = "success")
                
                ## Check if data was successfully saved to file and update log/status accordingly
                if (file.exists(data_file)) {
                    MMP_log("SUCCESS", LOG_FILE, Category = paste0(label, " data saved in ", data_file))
                    mmp__change_status(stage, item, status = "success")
                    ## If last step in data extraction stage, append filesize in console
                    if (!progressive) mmp__append_filesize(stage, item, label, data_file)
                }
                else {
                    save_error_msg <- paste(label, "data could not be saved to", data_file)
                    MMP_log("FAILURE", LOG_FILE, Category = save_error_msg) 
                    mmp__change_status(stage = stage, item = item, status = "failure")
                }
            }
        },
        error = function(e) {
            MMP_log("FAILURE", LOG_FILE, Category = paste0(extract_msg, ": ", e["message"])) 
            mmp__change_status(stage, item, status = "failure")
        },
        warning = function(w) {
            MMP_log("FAILURE", LOG_FILE, Category = paste0(extract_msg, ": ", w["message"])) 
            mmp__change_status(stage, item, status = "failure")
        }
    )
}

## Progressive = TRUE indicates that the status should only change if there is a failure.
## Essentially, this allows a fail flag to be raised midway through a sequence of steps.
## If a step fails, then the whole sequence is a failure.  However, if a step does not
## fail, the sequence is still in progress
MMP_checkData <- function(name = "niskin.csv",
                    stage = "STAGE2",
                    item = "aimsNiskin",
                    label = "",
                    label.prefix = "",
                    label.suffix = "",
                    PATH = NISKIN_PATH,
                    progressive = FALSE)
{
    label <- ifelse(label == "", mmp__get_name(stage, item), label)
    if (file.exists(paste0(PATH, name))) {
        if (!progressive) {
            MMP_log(status = "SUCCESS",
                    logFile = LOG_FILE,
                    Category = str_squish(paste(label.prefix,label, label.suffix, " data exists")),
                    msg=NULL)
            mmp__change_status(stage = stage, item = item, status = "success")
            mmp__append_filesize(stage = stage, item, label, paste0(PATH, name))
            ## filesize <- R.utils::hsize(file.size(paste0(PATH, name)))
            ## mmp__change_name(stage = stage, item = item, name = paste0(label, "  [",filesize, "]"))
        }
    } else {
        MMP_log(status = "FAILURE",
                logFile = LOG_FILE,
                Category = str_squish(paste(label.prefix, label, label.suffix, " data does not exist")),
                msg=NULL) 
        mmp__change_status(stage = stage, item = item, status = "failure")
    }
}

MMP_getTides <- function(ref, loc,path,file, t.start, t.end) {
    if(any(grepl(file,list.files(path)))) system(paste0("rm ",path,file))
    system(paste("export HFILE_PATH=../parameters/harmonics-2004-06-14.tcd; tide -l '",ref,"' -b '",t.start,"' -e '",t.end,"' -m m -f c -em smSMp -s '00:10' -z y -u m -o ",path,file, sep=""), intern = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
    tmp <-read.csv(paste0(path,file), header=F)
    colnames(tmp)<- c('TidesLocation','Date','Time','Height')
    tmp$DateTime <- as.POSIXct(paste(tmp$Date, tmp$Time), format='%Y-%m-%d %I:%M %p UTC')
    tmp<- data.frame(tmp,reef.alias=loc)
    tmp
}


MMP_add_to_report <- function(report_list, content) {
    report_list <- report_list %>% append(content)
    assign("DOC_REPORT_LIST", report_list, env = globalenv())
}

mmp__sql <- function(file) {
    paste0("```{sql}\n",
           "#| filename: ", file,"\n",
           "#| eval: false\n",
           "#| code-fold: false\n\n",
           readr::read_file(file),
           "```\n\n")
}

mmp__add_table <- function(tab) {
    knitr::kable(tab, format = "markdown")
}

mmp__glimpse_like <- function(tab) {
    data.frame(Variable = names(tab),
               Class = sapply(tab, typeof),
               `First values` = sapply(tab, function(x) paste0(head(x, 5),  collapse = ", ")),
               row.names = NULL)  
}

my_html_document <- function(template = "", ...) {
  base_format <- rmarkdown::html_document(...)

  template_arg <- which(base_format$pandoc$args == "--template") + 1L
  base_format$pandoc$args[template_arg] <- template

  base_format
}


MMP_add_to_report_list <- function(stage, item, ...) {
    values <- list2(...)
    if (length(values) ==1) values <- values[[1]]  # if ... is already wrapped in a list
    ## values <- c(..., use.names = TRUE)
    stage <- paste0("STAGE", stage)
    list.filename <- paste(stage, item, ".RData", sep = "_")
    if (!file.exists(paste0(DATA_PATH, "/reports/", list.filename))) {
        doc_list <- list()
        save(doc_list, file = paste0(DATA_PATH, "/reports/", list.filename))
    }
    load(paste0(DATA_PATH, "/reports/", list.filename))
    nms <- names(unlist(doc_list))
    ## find_and_replace <- function(x, find, replace){
    ##         if(is.list(x)){
    ##             n <- names(x) == find
    ##             if (any(n)) {
    ##                 wch <- which(n)
    ##                 ## print(wch)
    ##                 ## print(x[wch])
    ##                 ## print(replace)
    ##                 ## xx <- x[[wch]]
    ##                 ## xx <- append(xx, replace)
    ##                 ## x[[wch]] <- xx
    ##                 x[[wch]] <- append(x[[wch]], replace)
    ##                 ## print(x[wch])
    ##             }
    ##             lapply(x, find_and_replace, find=find, replace=replace)
    ##         }else{
    ##             x
    ##         }
    ##     }
    find_and_append <- function(x, find, replace){
        if(is.list(x)){
            n <- names(x) == find
            if (any(n)) {
                wch <- which(n)
                x[[wch]] <- append(x[[wch]], replace)
            }
            lapply(x, find_and_append, find=find, replace=replace)
        }else{
            x
        }
    }
    ## find_name <- function(haystack, needle) {
    ##     if (hasName(haystack, needle)) {
    ##         haystack[[needle]]
    ##     } else if (is.list(haystack)) {
    ##         for (obj in haystack) {
    ##             ret <- Recall(obj, needle)
    ##             if (!is.null(ret)) return(ret)
    ##         }
    ##     } else {
    ##         NULL
    ##     }
    ## }

## find_ref <- function(haystack, needle) {
##  if (hasName(haystack, needle)) {
##    haystack[needle]
##  } else if (is.list(haystack)) {
##    for (obj in haystack) {
##      ret <- Recall(obj, needle)
##      if (!is.null(ret)) return(ret)
##    }
##  } else {
##    NULL
##  }
## }

## do.call(`<-`, list(parse(text = e)[[1]], append(parse(text = e)[[1]], list('Big' = structure(list('An item'), parent = 'hat')), after = 1)))
    for (i in 1:length(values)) {
        nms <- names(unlist(doc_list))
        parent <- attr(values[[i]], 'parent')
        value <- values[i]
        if(!is.null(parent)) {
            if (any(str_which(nms, parent))) {
                doc_list <- find_and_append(doc_list, parent, value)
            }else {
                doc_list <- append(doc_list, value)
            }
            ## if (!is.null(parent)) {
            ##     doc_list[[parent]] <- append(doc_list[[parent]], value)
        } else {
            doc_list <- append(doc_list, value)
        }
        assign('doc_list', doc_list, envir = globalenv())
    }
    save(doc_list, file = paste0(DATA_PATH, "/reports/", list.filename))
}





## MMP_add_to_report_list <- function(stage, item, ...) {
##     values <- list2(...)
##     ## values <- c(..., use.names = TRUE)
##     stage <- paste0("STAGE", stage)
##     list.filename <- paste(stage, item, ".RData", sep = "_")
##     if (!file.exists(paste0(DATA_PATH, "/reports/", list.filename))) {
##         doc_list <- list()
##         save(doc_list, file = paste0(DATA_PATH, "/reports/", list.filename))
##     }
##     load(paste0(DATA_PATH, "/reports/", list.filename))

##     for (i in 1:length(values)) {
##          parent <- attr(values[[i]], 'parent')
##          value <- values[i]
##          if (!is.null(parent)) {
##              doc_list[[parent]] <- append(doc_list[[parent]], value)
##          } else {
##              doc_list <- append(doc_list, value)
##          }
##          assign('doc_list', doc_list, envir = globalenv())
##     }
##     save(doc_list, file = paste0(DATA_PATH, "/reports/", list.filename))
## }
MMP_get_report_list <- function(stage, item) {
    stage <- paste0("STAGE", stage)
    list.filename <- paste(stage, item, ".RData", sep = "_")
    load(paste0(DATA_PATH, "/reports/", list.filename))
    doc_list
}

mmp__make_table_chunk <- function(tab, caption) {
    tab <- deparse(substitute(tab))
    name <- str_replace_all(tab, "\\.", "-")
    label <- paste0('tbl-',name)
    
    ## - dump and capture the raw kable object
    ## - create an R chunk as a string to put into the qmd
    a <- paste(capture.output(dump(tab,"")), collapse='') %>%
        str_replace_all("NA","")

    paste0('\n```{r ',name,', results = "asis", echo=FALSE}\n',
                       '#| label: ',label,'\n',
                       '#| tbl-cap: "',caption,'"\n\n',
                       "options(knitr.kable.NA = '')\n",
                       a,
                      '\n',
                      '\n',tab,'\n',
                      '```\n\n'
                      ) %>%
                str_replace_all("\"", "'")
}

theme_mmp <- ggplot2:::theme_classic(10) + ggplot2:::theme(axis.line.x=ggplot2:::element_line(),
                                                           axis.line.y=ggplot2:::element_line())

####################################################################
## The following function defines nicer tick marks for log scales ##
####################################################################
base_breaks <- function(n = 10){
    function(x) {
        axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }
}


MMP__figure_export_dev <- function(FIGURE_OUTPUT_PATH, fig_name_suffix,
                                        Plot,
                                        fig.width, fig.height, units, pt.size = 10) {
    ## Output the figures 
    pdf(file = paste0(FIGURE_OUTPUT_PATH, fig_name_suffix, '.pdf'),
        width = fig.width, height = fig.height, pointsize = pt.size)
    print(Plot)
    dev.off()
    png(file = paste0(FIGURE_OUTPUT_PATH, fig_name_suffix, '.png'),
        width = fig.width, height = fig.height, units = units, pointsize = pt.size, res = 100)
    print(Plot)
    dev.off()
    png(file = paste0(FIGURE_OUTPUT_PATH, fig_name_suffix, '_large.png'),
        width = fig.width, height = fig.height, units = units, pointsize = pt.size, res = 600)
    print(Plot)
    dev.off()
    
}

MMP__comp_figure_export <- function(FIGURE_OUTPUT_PATH, Subregion, fig_name_suffix,
                                    Plot,
                                    fig.width, fig.height, pt.size = 10) {
    ## Output the figures 
    ggsave(filename = paste0(FIGURE_OUTPUT_PATH, "gamm_", gsub(" ","_", Subregion),
                             fig_name_suffix, ".pdf"),
           Plot, 
           width = fig.width, height = fig.height, pointsize = pt.size
           ) 
    ggsave(filename = paste0(FIGURE_OUTPUT_PATH, "gamm_", gsub(" ","_", Subregion),
                             fig_name_suffix, ".png"),
           Plot, 
           width = fig.width, height = fig.height, units = "in", dpi = 100, pointsize = pt.size
           ) 
    ggsave(filename = paste0(FIGURE_OUTPUT_PATH, "gamm_", gsub(" ","_", Subregion),
                             fig_name_suffix, "_large.png"),
           Plot, 
           width = fig.width, height = fig.height, units = "in", dpi = 600, pointsize = pt.size 
           ) 
}

MMP__figure_quarto <- function(CURRENT_STAGE, CURRENT_ITEM, FIGURE_OUTPUT_PATH,
                               Section, fig_name_suffix,
                               label_suffix, tabset_parent,
                               fig.caption) {
    ## Make the quarto list
    SUFFIX <- label_suffix 
    suffix <- gsub("_", "-", SUFFIX) 

    a <- list()
    a[[paste0("SUBSECTION",SUFFIX)]] <-
        structure(paste0("### ", Section, "\n"), parent = tabset_parent)
    a[[paste0("FIG_REF",SUFFIX)]] <-
        structure(paste0("\n::::: {#fig",suffix,"}\n"),
                  parent = paste0('SUBSECTION',SUFFIX)) 
    a[[paste0("FIG",SUFFIX)]] <-
        structure(paste0("![](",FIGURE_OUTPUT_PATH,
                         fig_name_suffix, ".png)\n"),
                  parent = paste0("FIG_REF",SUFFIX))
    a[[paste0("FIG_CAP", SUFFIX)]] <-
        structure(fig.caption,
                  parent = paste0('FIG_REF', SUFFIX))
    a[[paste0("FIG_REF",SUFFIX,"END")]] <-
        structure(paste0("\n::::: \n"), parent = paste0('SUBSECTION',SUFFIX)) 
    a[[paste0("FIG_NAME",SUFFIX)]] <-
        structure(paste0("\n**[", FIGURE_OUTPUT_PATH, fig_name_suffix, ".png]**\n"),
                  parent = paste0("SUBSECTION", SUFFIX))
    MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM, a)
}
MMP__comp_figure_quarto <- function(FIGURE_OUTPUT_PATH, Subregion, fig_name_suffix,
                                    label_suffix, Cnt, tabset_parent,
                                    fig.caption) {
    ## Make the quarto list
    SUFFIX <- paste0(label_suffix, Cnt) 
    suffix <- gsub("_", "-", SUFFIX) 

    a <- list()
    a[[paste0("SUBSECTION",SUFFIX)]] <-
        structure(paste0("### ", Subregion, "\n"), parent = tabset_parent)
    a[[paste0("FIG_REF",SUFFIX)]] <-
        structure(paste0("\n::::: {#fig",suffix,"}\n"),
                  parent = paste0('SUBSECTION',SUFFIX)) 
    a[[paste0("FIG",SUFFIX)]] <-
        structure(paste0("![](",FIGURE_OUTPUT_PATH,"gamm_",
                         gsub(" ", "_", Subregion), fig_name_suffix, ".png)\n"),
                  parent = paste0("FIG_REF",SUFFIX))
    a[[paste0("FIG_CAP", SUFFIX)]] <-
        structure(fig.caption,
                  parent = paste0('FIG_REF', SUFFIX))
    a[[paste0("FIG_REF",SUFFIX,"END")]] <-
        structure(paste0("\n::::: \n"), parent = paste0('SUBSECTION',SUFFIX)) 
    a[[paste0("FIG_NAME",SUFFIX)]] <-
        structure(paste0("\n**[", FIGURE_OUTPUT_PATH, "gamm_",
                         gsub(" ", "_", Subregion), fig_name_suffix, ".png]**\n"),
                  parent = paste0("SUBSECTION", SUFFIX))
    
    MMP_add_to_report_list(CURRENT_STAGE, "compilations", a)
}

MMP__transect_figure_quarto <- function(CURRENT_STAGE, CURRENT_ITEM, FIGURE_OUTPUT_PATH,
                               Section, fig_name_suffix,
                               label_suffix, tabset_parent,
                               fig.caption) {
    ## Make the quarto list
    SUFFIX <- gsub(" ", "", label_suffix) 
    suffix <- gsub("_", "-", SUFFIX) 

    a <- list()
    a[[paste0("SUBSECTION",SUFFIX)]] <-
        structure(paste0("## ", Section, "\n"), parent = tabset_parent)
    a[[paste0("FIG_REF",SUFFIX)]] <-
        structure(paste0("\n:::: {#fig",suffix,"}\n"),
                  parent = paste0('SUBSECTION',SUFFIX)) 
    a[[paste0("FIG",SUFFIX)]] <-
        structure(paste0("![](",FIGURE_OUTPUT_PATH,
                         fig_name_suffix, ".png)\n"),
                  parent = paste0("FIG_REF",SUFFIX))
    a[[paste0("FIG_CAP", SUFFIX)]] <-
        structure(fig.caption,
                  parent = paste0('FIG_REF', SUFFIX))
    a[[paste0("FIG_REF",SUFFIX,"END")]] <-
        structure(paste0("\n:::: \n"), parent = paste0('SUBSECTION',SUFFIX)) 
    a[[paste0("FIG_NAME",SUFFIX)]] <-
        structure(paste0("\n**[", FIGURE_OUTPUT_PATH, fig_name_suffix, ".png]**\n"),
                  parent = paste0("SUBSECTION", SUFFIX))
    MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM, a)
}

MMP__gam_table_quarto <- function(CURRENT_STAGE, Subregion,label_suffix,
                                  gam.tbl, Cnt, tabset_parent,
                                  tbl.caption) {
    ## Make the quarto list
    SUFFIX <- paste0(label_suffix, Cnt) 
    suffix <- gsub("_", "-", SUFFIX) 

    a <- list()
    a[[paste0("SUBSECTION",SUFFIX)]] <-
        structure(paste0("### ", Subregion, "\n"), parent = tabset_parent)
    ## a[[paste0("TBL_REF",SUFFIX)]] <-
    ##     structure(paste0("\n::::: {#tbl",suffix,"}\n"),
    ##               parent = paste0('SUBSECTION',SUFFIX)) 
    a[[paste0("TBL",SUFFIX)]] <-
        structure(mmp__add_table(gam.tbl),
                  ## parent = paste0("TBL_REF",SUFFIX))
                  parent = paste0('SUBSECTION',SUFFIX)) 
    a[[paste0("TBL_CAP", SUFFIX)]] <-
        structure(paste0(tbl.caption," {#tbl",suffix,"}\n\n"),
                  parent = paste0('TBL', SUFFIX))
    ## a[[paste0("TBL_REF",SUFFIX,"END")]] <-
    ##     structure(paste0("\n::::: \n"), parent = paste0('SUBSECTION',SUFFIX)) 
    
    MMP_add_to_report_list(CURRENT_STAGE, "compilations", a)
}


#########################################################################
## The following function reads in a netcdf file and standardises the  ##
## fields so that they are compatible with the previous database       ##
## structures.                                                         ##
## Parameters:                                                         ##
##    nc_file: a string representation of the full path of the file    ##
## Return:                                                             ##
##    tibble: a tibble representing the data.                          ##
#########################################################################
mmp__read_flntu_nc <- function(nc_file) {
  lookup <- read_csv("../parameters/lookup.csv") %>% suppressMessages()
  names_lookup <- read_csv("../parameters/names_lookup.csv") %>% suppressMessages()

  dat <- ncdf4::nc_open(nc_file)
  ## print(dat) 
  ## names(dat$dim)

  ## get the site_code
  site_code <- ncdf4::ncatt_get(dat, 0, "site_code")$value
  region <- lookup %>% filter(SHORT_NAME == site_code) %>% pull(Region)
  ## extract the time dimension
  time <- ncdf4::ncvar_get(dat, "TIME")
  ## determine the units of time
  tunits <- ncdf4::ncatt_get(dat, "TIME", "units")
  ## convert Gregorian time into Date 
  datetime <- MMP_convert_fractional_days_to_datetime(time)
  sample_day <- as.Date(format(datetime, "%Y-%m-%d"))

  ## get the names of the variables
  avail_vars <- names(dat$var)
  
  ## get longitude
  lon <- ncdf4::ncvar_get(dat, "LONGITUDE")
  lat <- ncdf4::ncvar_get(dat, "LATITUDE")
  depth <- ncdf4::ncvar_get(dat, "NOMINAL_DEPTH")

  ## construct a tibble with the above variables
  df <- tibble(
    STATION_ID = site_code,
    NRM_REGION = region,
    DEPLOY_DATE = format(datetime, "%d-%b-%Y %H:%M:%S"),
    SAMPLE_DAY = sample_day,
    lon = lon,
    lat = lat,
    depth = depth,
    )
  ## Handle each of the different measurements.
  ## Should probably just write a single function that takes a name
  ## but the following is easier (although more verbose)

  df_1 <- NULL
  if ("CPHL" %in% avail_vars) {
    chla <- ncdf4::ncvar_get(dat, "CPHL")
    chla_quality_control <- ncdf4::ncvar_get(dat, "CPHL_quality_control")
    df_tmp <- df %>%
      mutate(
        chla = chla,
        chla_quality_control = chla_quality_control) %>% 
      filter(chla_quality_control == 1) %>% 
      pivot_longer(cols = c(chla, chla_quality_control),
                   names_to = "PARAMETER", values_to = "Values")
    df_1 <- df_1 %>% rbind(df_tmp)
  }
  if ("TURB" %in% avail_vars) {
    turb <- ncdf4::ncvar_get(dat, "TURB")
    turb_quality_control <- ncdf4::ncvar_get(dat, "TURB_quality_control")
    df_tmp <- df %>%
      mutate(
        turb = turb,
        turb_quality_control = turb_quality_control) %>% 
      filter(turb_quality_control == 1) %>% 
      pivot_longer(cols = c(turb, turb_quality_control),
                   names_to = "PARAMETER", values_to = "Values")
    df_1 <- df_1 %>% rbind(df_tmp)
  }
  df_1 |>
    mutate(SHORT_NAME = STATION_ID) %>%
    left_join(names_lookup %>%
              dplyr::select(SHORT_NAME, MMP_SITE_NAME), by = "SHORT_NAME") |>
    pivot_wider(names_from = "PARAMETER", values_from = "Values")
}

MMP_read_flntu_nc <- function(flntu_files) {
  df <- do.call('rbind', lapply(flntu_files, mmp__read_flntu_nc))

  df <- df %>%
    dplyr::filter(!is.na(chla)) %>% 
    group_by(STATION_ID, SHORT_NAME, MMP_SITE_NAME, SAMPLE_DAY) %>%
    summarise(
      CHL_QA_AVG = mean(chla),
      NTU_QA_AVG = mean(turb)) %>% 
    suppressWarnings() %>%
    suppressMessages()
  df
}

MMP_read_flntu_nc_old <- function(nc_file) {
  lookup <- read_csv("../parameters/lookup.csv") %>% suppressMessages()
  names_lookup <- read_csv("../parameters/names_lookup.csv") %>% suppressMessages()

  dat <- ncdf4::nc_open(nc_file)
  ## print(dat) 
  ## names(dat$dim)

  ## get the site_code
  site_code <- ncdf4::ncatt_get(dat, 0, "site_code")$value
  region <- lookup %>% filter(SHORT_NAME == site_code) %>% pull(Region)
  ## extract the time dimension
  time <- ncdf4::ncvar_get(dat, "TIME")
  ## determine the units of time
  tunits <- ncdf4::ncatt_get(dat, "TIME", "units")
  ## convert Gregorian time into Date 
  datetime <- MMP_convert_fractional_days_to_datetime(time)
  sample_day <- as.Date(format(datetime, "%Y-%m-%d"))

  ## get the names of the variables
  ## names(dat$var)
  
  ## get longitude
  lon <- ncdf4::ncvar_get(dat, "LONGITUDE")
  lat <- ncdf4::ncvar_get(dat, "LATITUDE")
  depth <- ncdf4::ncvar_get(dat, "NOMINAL_DEPTH")
  chla <- ncdf4::ncvar_get(dat, "CPHL")
  chla_quality_control <- ncdf4::ncvar_get(dat, "CPHL_quality_control")
  turb <- ncdf4::ncvar_get(dat, "TURB")
  turb_quality_control <- ncdf4::ncvar_get(dat, "TURB_quality_control")
  
  ## construct a tibble with the above variables
  df <- tibble(
    STATION_ID = site_code,
    NRM_REGION = region,
    DEPLOY_DATE = format(datetime, "%d-%b-%Y %H:%M:%S"),
    SAMPLE_DAY = sample_day,
    lon = lon,
    lat = lat,
    depth = depth,
    chla = chla,
    chla_quality_control = chla_quality_control,
    turb = turb,
    turb_quality_control = turb_quality_control
  ) 
  ## daily aggregate
  df <- df %>%
    group_by(STATION_ID, NRM_REGION, SAMPLE_DAY) %>%
    summarise(CHL_QA_AVG = mean(chla[chla_quality_control == 1]),
              NTU_QA_AVG = mean(turb[turb_quality_control == 1])
              ) %>%
    mutate(SHORT_NAME = site_code) %>%
    left_join(names_lookup %>%
              dplyr::select(SHORT_NAME, MMP_SITE_NAME), by = "SHORT_NAME") %>% 
    suppressWarnings() %>%
    suppressMessages()
  df
}

#########################################################################
## The following function reads in a netcdf file and standardises the  ##
## fields so that they are compatible with the previous database       ##
## structures.                                                         ##
## Parameters:                                                         ##
##    nc_file: a string representation of the full path of the file    ##
## Return:                                                             ##
##    tibble: a tibble representing the data.                          ##
#########################################################################
mmp__read_salinity_nc <- function(nc_file) {
  lookup <- read_csv("../parameters/lookup.csv") %>% suppressMessages()
  names_lookup <- read_csv("../parameters/names_lookup.csv") %>% suppressMessages()

  ## print(nc_file)
  dat <- ncdf4::nc_open(nc_file)
  ## print(dat) 
  ## names(dat$dim)

  ## get the site_code
  site_code <- ncdf4::ncatt_get(dat, 0, "site_code")$value
  region <- lookup %>% filter(SHORT_NAME == site_code) %>% pull(Region)
  ## extract the time dimension
  time <- ncdf4::ncvar_get(dat, "TIME")
  ## determine the units of time
  tunits <- ncdf4::ncatt_get(dat, "TIME", "units")
  ## convert Gregorian time into Date 
  datetime <- MMP_convert_fractional_days_to_datetime(time)
  sample_day <- as.Date(format(datetime, "%Y-%m-%d"))

  ## get the names of the variables
  avail_vars <- names(dat$var)
  
  ## get longitude
  lon <- ncdf4::ncvar_get(dat, "LONGITUDE")
  lat <- ncdf4::ncvar_get(dat, "LATITUDE")
  depth <- ncdf4::ncvar_get(dat, "NOMINAL_DEPTH")
  ## construct a tibble with the above variables
  df <- tibble(STATION_NAME = paste0(site_code,"_REC_", format(datetime, "%Y%m%d")),
    NRM_REGION = region,
    DEPLOY_DATE = format(datetime, "%d-%b-%Y %H:%M:%S"),
    SAMPLE_DAY = sample_day,
    lon = lon,
    lat = lat,
    depth = depth
    )
  ## Handle each of the different measurements.
  ## Should probably just write a single function that takes a name
  ## but the following is easier (although more verbose)
  df_1 <- NULL
  if ("TEMP" %in% avail_vars) {
    temp <- ncdf4::ncvar_get(dat, "TEMP") * 1.00024
    temp_quality_control <- ncdf4::ncvar_get(dat, "TEMP_quality_control")
    df_tmp <- df %>%
      mutate(
        temp = temp,
        temp_quality_control = temp_quality_control) %>% 
      dplyr::rename("temperature" = temp) %>%
      filter(temp_quality_control == 1) %>% 
      pivot_longer(cols = c(temperature, temp_quality_control),
                   names_to = "PARAMETER", values_to = "Values")
    df_1 <- df_1 %>% rbind(df_tmp)
  }
  if ("PSAL" %in% avail_vars) {
    psal <- ncdf4::ncvar_get(dat, "PSAL")
    psal_quality_control <- ncdf4::ncvar_get(dat, "PSAL_quality_control")
    df_tmp <- df %>%
      mutate(
        psal = psal,
        psal_quality_control = psal_quality_control) %>% 
      dplyr::rename("salinity" = psal) %>%
      filter(psal_quality_control == 1) %>% 
      pivot_longer(cols = c(salinity, psal_quality_control),
                   names_to = "PARAMETER", values_to = "Values")
    df_1 <- df_1 %>% rbind(df_tmp)
  }
  if ("CNDC" %in% avail_vars) {
    cndc <- ncdf4::ncvar_get(dat, "CNDC")
    cndc_quality_control <- ncdf4::ncvar_get(dat, "CNDC_quality_control")
    df_tmp <- df %>%
      mutate(
        cndc = cndc,
        cndc_quality_control = cndc_quality_control) %>% 
      dplyr::rename("conductivity" = cndc) %>%
      filter(cndc_quality_control == 1) %>% 
      pivot_longer(cols = c(conductivity, cndc_quality_control),
                   names_to = "PARAMETER", values_to = "Values")
    df_1 <- df_1 %>% rbind(df_tmp)
  }
  if ("PRES_REL" %in% avail_vars) {
    pres <- ncdf4::ncvar_get(dat, "PRES_REL")
    pres_quality_control <- ncdf4::ncvar_get(dat, "PRES_REL_quality_control")
    df_tmp <- df %>%
      mutate(
        pres = pres,
        pres_quality_control = pres_quality_control) %>% 
      dplyr::rename("pres_rel" = pres) %>%
      filter(pres_quality_control == 1) %>% 
      pivot_longer(cols = c(pres_rel, pres_quality_control),
                   names_to = "PARAMETER", values_to = "Values")
    df_1 <- df_1 %>% rbind(df_tmp)
  }
  df_1
}

MMP_read_salinity_nc <- function(salinity_files) {
  df <- do.call('rbind', lapply(salinity_files, mmp__read_salinity_nc))

  df <- df %>%
    dplyr::filter(!str_detect(PARAMETER, "quality_control")) %>% 
    group_by(STATION_NAME, SAMPLE_DAY, PARAMETER) %>%
    summarise(
      AVG_VALUE_QAQC = mean(Values)) %>% 
    suppressWarnings() %>%
    suppressMessages()
  df
}


MMP_read_salinity_nc_old <- function(nc_file) {
  lookup <- read_csv("../parameters/lookup.csv") %>% suppressMessages()
  names_lookup <- read_csv("../parameters/names_lookup.csv") %>% suppressMessages()

  ## print(nc_file)
  dat <- ncdf4::nc_open(nc_file)
  ## print(dat) 
  ## names(dat$dim)

  ## get the site_code
  site_code <- ncdf4::ncatt_get(dat, 0, "site_code")$value
  region <- lookup %>% filter(SHORT_NAME == site_code) %>% pull(Region)
  ## extract the time dimension
  time <- ncdf4::ncvar_get(dat, "TIME")
  ## determine the units of time
  tunits <- ncdf4::ncatt_get(dat, "TIME", "units")
  ## convert Gregorian time into Date 
  datetime <- MMP_convert_fractional_days_to_datetime(time)
  sample_day <- as.Date(format(datetime, "%Y-%m-%d"))

  ## get the names of the variables
  avail_vars <- names(dat$var)
  
  ## get longitude
  lon <- ncdf4::ncvar_get(dat, "LONGITUDE")
  lat <- ncdf4::ncvar_get(dat, "LATITUDE")
  depth <- ncdf4::ncvar_get(dat, "NOMINAL_DEPTH")
  ## construct a tibble with the above variables
  df <- tibble(STATION_NAME = paste0(site_code,"_REC_", format(datetime, "%Y%m%d")),
    NRM_REGION = region,
    DEPLOY_DATE = format(datetime, "%d-%b-%Y %H:%M:%S"),
    SAMPLE_DAY = sample_day,
    lon = lon,
    lat = lat,
    depth = depth
    )
  if ("TEMP" %in% avail_vars) {
    temp <- ncdf4::ncvar_get(dat, "TEMP") * 1.00024
    temp_quality_control <- ncdf4::ncvar_get(dat, "TEMP_quality_control")
    df <- df %>% mutate(
                   temp = temp,
                   temp_quality_control = temp_quality_control,
                   temp = ifelse(temp_quality_control == 1, temp, NA)) %>%
      dplyr::rename("temperature" = temp)
  }
  if ("PSAL" %in% avail_vars) {
    psal <- ncdf4::ncvar_get(dat, "PSAL")
    psal_quality_control <- ncdf4::ncvar_get(dat, "PSAL_quality_control")
    df <- df %>% mutate(
                   psal = psal,
                   psal_quality_control = psal_quality_control,
                   psal = ifelse(psal_quality_control == 1, psal, NA)
                 )  %>%
      ## dplyr::rename("condOS/m: Conductivity [S/m]" = psal)
      dplyr::rename("salinity" = psal)
  }
  if ("CNDC" %in% avail_vars) {
    cndc <- ncdf4::ncvar_get(dat, "CNDC")
    cndc_quality_control <- ncdf4::ncvar_get(dat, "CNDC_quality_control")
    df <- df %>% mutate(
                   cndc = cndc,
                   cndc_quality_control = cndc_quality_control,
                   cndc = ifelse(cndc_quality_control == 1, cndc, NA)
                 )  %>%
      dplyr::rename("conductivity" = cndc)
  }
  if ("PRES_REL" %in% avail_vars) {
    pres <- ncdf4::ncvar_get(dat, "PRES_REL")
    pres_quality_control <- ncdf4::ncvar_get(dat, "PRES_REL_quality_control")
    df <- df %>% mutate(
                   pres = pres,
                   pres_quality_control = pres_quality_control,
                   pres = ifelse(pres_quality_control == 1, pres, NA)
                   )  %>%
      dplyr::rename("pres_rel" = pres)
  }
  
  ## daily aggregate
  df <- df %>%
    ## remove any missing values - this may address issues caused by nc files that
    ## have dates that appear in other nc files, yet have non-one qaqc values thereby
    ## resulting in NA values
    filter(across(any_of(c("pres_rel", "conductivity", "salinity", "temperature")),
                  function(x) !is.na(x))) %>% 
    dplyr::select(-NRM_REGION, -lon, -lat, -depth, -DEPLOY_DATE, -ends_with("quality_control")) %>% 
    group_by(STATION_NAME, SAMPLE_DAY) %>%
    summarise(
      ## across(any_of(c("temp", "psal", "cndc", "pres")), ~mean(.x, na.rm = TRUE)
      across(-any_of(c("STATION_NAME", "SAMPLE_DAY")), ~mean(.x, na.rm = TRUE))) %>% 
    pivot_longer(cols = c(-"STATION_NAME", -"SAMPLE_DAY"), names_to = "PARAMETER",
                 values_to = "AVG_VALUE_QAQC") %>% 
    suppressWarnings() %>%
    suppressMessages()
  df
}

#########################################################################
## The following function converts fractional days (relative to        ##
## 1950-01-01 10:00) as provided in the netcdf files into an actual    ##
## datetime vector.                                                    ##
## Parameters:                                                         ##
##    time:    a fractional number of days since 1950-01-01 10:00      ##
## Return:                                                             ##
##    POSIXct: a date time representation of the sample date           ##
#########################################################################
MMP_convert_fractional_days_to_datetime <- function(time) {
  origin <- as.POSIXct("1950-01-01 10:00")
  int_days <- as.integer(time)
  fractional_days <- time - int_days 
  dt <- origin + lubridate::ddays(int_days)
  hours <- as.integer(fractional_days * 24)
  minutes <- as.integer((fractional_days * 24 - hours) * 60)
  seconds <- as.integer((((fractional_days * 24 - hours) * 60 - minutes) * 60)) 
  dt + dhours(hours) + dminutes(minutes) + dseconds(seconds)
}
