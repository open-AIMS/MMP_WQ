source("MMP_functions_boxes.R")

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
        MMP_clear_paths(paths = c('DATA_PATH', 'OUTPUT_PATH',
                                  'DOCS_PATH'
                                  ))      
        MMP_prepare_paths()    ## prepare file structure
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
                                "waterTemp","salinity","dhd","disturbances", "tides","BOM",
                                "DataReport"),
                      names = c("AIMS niskin data","Cairns transect data","JCU niskin data","JCY CY niskin data",
                                "JCU Event niskin data","JCU CY Event niskin data",
                                "AIMS FLNTU loggers",
                                "Water temperature loggers","Salinity loggers",
                                "Degree heating weeks","Disturbance table", "Harmonic tides","BOM weather",
                                "Data report"),
                      status = c("pending","pending","pending","pending","pending",
                                 "pending","pending","pending","pending","pending","pending","pending","pending","pending")
                      )
    )
    assign("STATUS", STATUS, env = globalenv())
}

#########################################################################
## The following function parses the command line arguments            ##
## --finalYear                                                         ##
##    : the maximum reneeYear (beginning 1st September each year).     ##
##      This essentially defines the upper limit of data used in the   ##
##      analyses.                                                      ##
## --runStage                                                          ##
##    : which stage is the analysis intending to run.                  ##
##      1. preparation stage, also performs a complete clearout        ##
#########################################################################
MMP_parseCLA <- function(args) {
    runStage <<- 1   ## this is a temp incase it is not specified on the command line - it is required for the openning banner
    CURRENT_STAGE <<- 1
    if(length(args)<7) {
        MMP_log(status = "FAILURE", logFile = LOG_FILE, Category = "Parsing the command line arguments", msg=NULL) 
        mmp__change_status(stage = "STAGE1", item = "Parse command line args", status = "failure")
        MMP_openning_banner()
        stop(paste('This project must be run with command line arguments\nUsage: Rscript MMP_00_main.R --reportYear=<YEAR> --runStage=<vector of stage numbers> --alwaysExtract=<TRUE>'),
             call. = FALSE)
    }
    ## args <- commandArgs()
    report_year <- grep('--reportYear=.*', args)
    if(length(report_year) == 0) {
        MMP_log(status = "FAILURE", logFile = LOG_FILE, Category = "Parsing the command line arguments", msg=NULL) 
        mmp__change_status(stage = "STAGE1", item = "Parse command line args", status = "failure")
        MMP_openning_banner()
        stop('A final report year must be supplied as a command line argument, such as: Rscript <script.R> --reportYear=2022', call. = FALSE)
    }
    reportYear <- args[report_year]
    reportYear <- gsub('--reportYear=(.*)','\\1', reportYear)
    assign("reportYear", reportYear, env = globalenv())
    mmp__add_status(stage = "SETTINGS", item = "reportYear", name = "Report year", status = "success")
    
    runStage <- grep('--runStage=.*', args)
    if(length(runStage) == 0) {
        MMP_log(status = "FAILURE", logFile = LOG_FILE, Category = "Parsing the command line arguments", msg=NULL) 
        mmp__change_status(stage = "STAGE1", item = "Parse command line args", status = "failure")
        MMP_openning_banner()
        stop('A run stage must be supplied as a command line argument, such as: Rscript <script.R> --runStage=1', call. = FALSE)
    }
    runStage <- args[runStage]
    runStage <- eval(parse(text=gsub('--runStage=(.*)','\\1', runStage)))
    assign("runStage", runStage, env = globalenv())
    assign("CURRENT_STAGE", runStage[1], env = globalenv())
    mmp__add_status(stage = "SETTINGS", item = "runStage", name = "Run stages", status = "success")
    mmp__add_status(stage = "SETTINGS", item = "CURRENT_STAGE", name = "Current stage", status = "success")

    alwaysExtract <- grep('--alwaysExtract.*', args)
    if (length(alwaysExtract) == 0) {
        alwaysExtract <- TRUE
    } else {
        alwaysExtract <- args[alwaysExtract]
        alwaysExtract <- eval(parse(text=gsub('--alwaysExtract=(.*)','\\1', alwaysExtract)))
    }
    assign("alwaysExtract", alwaysExtract, env = globalenv())
    mmp__add_status(stage = "SETTINGS", item = "alwaysExtract", name = "Always extract", status = "success")
    
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
    pkgs <- c('tidyverse','testthat','cli','rlang','crayon', 'assertthat'
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

    if (!dir.exists(OUTPUT_PATH)) dir.create(OUTPUT_PATH)
    if (!dir.exists(paste0(OUTPUT_PATH, '/tables')))
        dir.create(paste0(OUTPUT_PATH, '/tables'))

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
    STATUS[[stage]]$name[which(STATUS[[stage]]$item == item)] <- name
    assign("STATUS", STATUS, env = globalenv())
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
##########################################################################                                               
MMP_tryCatch <- function(expr, logFile,Category, expectedClass=NULL, msg=NULL, return=NULL, showWarnings=FALSE) {
    if (!exists('PROGRESS')) PROGRESS=NULL
    max.warnings<-4
    warnings<-0
    W <- NULL
    w.handler <- function(w){ # warning handler                                                                          
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
}



MMP_tryCatch_db <- function(name = 'niskin',
                            stage = "STAGE2",
                            item = "aimsNiskin",
                            label = "AIMS niskin",
                            PATH = NISKIN_PATH,
                            db_user = "wq_nut2") {
    MSG <- paste0("Extracting ", label, " data from the database")
    tryCatch({
        status <- system2("java",
                          args = paste0("-jar dbExport.jar ", PATH, name, ".sql ", PATH, name, ".csv ", db_user),
                          stdout = TRUE, stderr = TRUE)
        ## Catch the errors returned by the database
        ##print(status)
        if (stringr::str_detect(status[6], 'Error')) {
            msg <- paste0(MSG, ": There is a problem with the SQL: ", stringr::str_replace(status[8], "java.sql.SQLSyntaxErrorException: (.*)", "\\1"))

            MMP_log(status = "FAILURE",
                    logFile = LOG_FILE,
                    Category = msg,
                    msg=NULL) 
            mmp__change_status(stage = stage, item = item, status = "failure")
            ## stop(msg, call. = FALSE)
        } else {
            MMP_log(status = "SUCCESS",
                    logFile = LOG_FILE,
                    Category = MSG,
                    msg=NULL) 
            mmp__change_status(stage = stage, item = item, status = "success")
        }
        
        ## warning('Be')
    },
    error = function(e) {
        MMP_log(status = "FAILURE",
                logFile = LOG_FILE,
                Category = paste0(MSG, ": ", e["message"]),
                msg=NULL) 
        mmp__change_status(stage = stage, item = item, status = "failure")
    },
    warning = function(w) {
                                        # print(paste0(paste(w, collapse = ''), ' This is a warning warning'))
        MMP_log(status = "FAILURE",
                logFile = LOG_FILE,
                Category = paste0(MSG, ": ", w["message"]),
                msg=NULL) 
        mmp__change_status(stage = stage, item = item, status = "failure")
    }
    )
}

MMP_checkData <- function(name = "niskin",
                    stage = "STAGE2",
                    item = "aimsNiskin",
                    label = "AIMS niskin",
                    PATH = NISKIN_PATH) {
    if (file.exists(paste0(PATH, name, ".csv"))) {
        MMP_log(status = "SUCCESS",
                logFile = LOG_FILE,
                Category = paste0(label, " data exists"),
                msg=NULL) 
        mmp__change_status(stage = stage, item = item, status = "success")
        filesize <- R.utils::hsize(file.size(paste0(PATH, name, ".csv")))
        mmp__change_name(stage = stage, item = item, name = paste0(label, "  [",filesize, "]"))
    } else {
        MMP_log(status = "FAILURE",
                logFile = LOG_FILE,
                Category = paste0(label, " data does not exist"),
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
