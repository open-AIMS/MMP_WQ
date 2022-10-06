
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
    MMP_loadPackages()         ## load required packages
    MMP_parseCLA(args)         ## parse command line arguments
    MMP_define_paths()         ## define the location of paths/files
    if (1 %in% runStage) {
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
                      items = c("aimsNiskin","jcuNiskin","jcuCYNiskin",
                                "jcuEventNiskin","jcuCYEventNiskin",
                                "flntu", "cairnsTransect",
                                "waterTemp","salinity","dhd","disturbances",
                                "DataReport"),
                      names = c("AIMS niskin data","JCU niskin data","JCY CY niskin data",
                                "JCU Event niskin data","JCU CY Event niskin data",
                                "AIMS FLNTU loggers","Cairns transect data",
                                "Water temperature loggers","Salinity loggers",
                                "Degree heating weeks","Disturbance table",
                                "Data report"),
                      status = c("pending","pending","pending","pending","pending",
                                 "pending","pending","pending","pending","pending","pending","pending")
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
    ## args <- commandArgs()
    report_year <- grep('--reportYear=.*', args)
    if(length(report_year) == 0)
        stop('A final report year must be supplied as a command line argument, such as: Rscript <script.R> --reportYear=2022')
    reportYear <- args[report_year]
    reportYear <- gsub('--reportYear=(.*)','\\1', reportYear)
    assign("reportYear", reportYear, env = globalenv())
    mmp__add_status(stage = "SETTINGS", item = "reportYear", name = "Report year", status = "success")
    
    runStage <- grep('--runStage=.*', args)
    if(length(runStage) == 0)
        stop('A run stage must be supplied as a command line argument, such as: Rscript <script.R> --runStage=1')
    runStage <- args[runStage]
    runStage <- eval(parse(text=gsub('--runStage=(.*)','\\1', runStage)))
    assign("runStage", runStage, env = globalenv())
    assign("CURRENT_STAGE", runStage[1], env = globalenv())
    mmp__add_status(stage = "SETTINGS", item = "runStage", name = "Run stages", status = "success")
    mmp__add_status(stage = "SETTINGS", item = "CURRENT_STAGE", name = "Current stage", status = "success")
    
    mmp__change_status(stage = "STAGE1", item = "Parse command line args", status = "success")
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
    pkgs <- c('tidyverse','testthat','cli','rlang','crayon'
              )

    for (p in pkgs) {
        ## unforunately we must do this the base r way until rlang is
        ## loaded
        eval(parse(text=paste0("suppressPackageStartupMessages(if(!require(",
                               p,",quietly = TRUE, warn.conflicts = FALSE)) missing <- c(missing, ",
                               p,"))"))) 
    }

    mmp__change_status(stage = "STAGE1", item = "Load packages", status = "success")

    if(missing!="") 
        stop(paste('The following required package(s) are missing: ',paste(missing, collapse=', ')))
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

    if (!dir.exists(OUTPUT_PATH)) dir.create(OUTPUT_PATH)
    if (!dir.exists(paste0(OUTPUT_PATH, '/tables')))
        dir.create(paste0(OUTPUT_PATH, '/tables'))

    if (!dir.exists(DOCS_PATH)) dir.create(DOCS_PATH)

    mmp__change_status(stage = "STAGE1", item = "Prepare file system", status = "success")
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

    
    
}

mmp__outerBox.top <- function(outer.box.width, this.box.width) {
    top <- paste0("\u2554",
                  strrep("\u2550", this.box.width),
                  "\u2564",
                  strrep("\u2550", outer.box.width - this.box.width),
                  "\u2557",
                  "\n"
                  )
    top
}

mmp__outerBox.bottom <- function(outer.box.width, this.box.width) {
    bottom <- paste0("\u255A",
                     strrep("\u2550", this.box.width),
                     '\u2567',
                     strrep("\u2550", outer.box.width - this.box.width),
                     "\u255D",
                     "\n"
                     )
    bottom
}


mmp__settingsBox <- function(settings, box.width, box.nchar, box.margins, currentTime) {
    box.text <- NULL
    keys <- settings$names
    values <- sapply(settings$items, function(x) eval(parse(text = x)))
    status <- settings$status
    for (i in 1:length(keys)) {
        box.text <- c(box.text,
                           paste0("\u2551",
                                  strrep(" ", box.margins),
                                  switch(status[i],
                                         'pending' = crayon::white(cli::symbol$line),
                                         'success' = crayon::green(cli::symbol$tick),
                                         'failure' = crayon::red(cli::symbol$cross)
                                         ),
                                  " ", crayon::blue(keys[i]), ": ",
                                  crayon::white(values[i]),
                                  strrep(" ", box.width - (box.nchar[i])-box.margins*2 -1),
                                  "\u2502",
                                  strrep(" ", box.margins)
                                  )
                           )
    }
    box.text
}

mmp__mainBox <- function(settings.box.text, box.width, settings.box.width, box.margins) {
    main.box.text <- c("MMP Water Quality Report Analysis", "")
    ## format the title to be centered
    for (i in 1:length(main.box.text))
        main.box.text[i] <- cli::ansi_align(main.box.text[i],
                                            width = box.width - settings.box.width - 1,
                                            align = 'center')

    ## add the stages as left justified 
    for (j in 1:length(runStage)) {
            main.box.text <- c(main.box.text,
                               cli::ansi_align(STATUS[[paste0("STAGE",runStage[j])]]$title,
                                               width = box.width - settings.box.width - 1,
                                               align = 'left')
                               )
            for (i in 1:length(STATUS[[paste0("STAGE",runStage[j])]]$items)) {
                if (j == CURRENT_STAGE | STATUS[[paste0("STAGE",runStage[j])]]$status[i] == 'failure') {
                    main.box.text <- c(main.box.text,
                                       cli::ansi_align(
                                                paste0(strrep(" ", box.margins),      
                                                       switch(STATUS[[paste0("STAGE",runStage[j])]]$status[i],
                                                              'pending' = crayon::white(cli::symbol$line),
                                                              'success' = crayon::green(cli::symbol$tick),
                                                              'failure' = crayon::red(cli::symbol$cross)
                                                              ),
                                                       " ", crayon::blue(STATUS[[paste0("STAGE",runStage[j])]]$name[i])
                                                       ),
                                                width = box.width - settings.box.width - 1,
                                                align = 'left'
                                            )
                                       )
                }
                
        } 
    }
    main.box.nchar <- nchar(main.box.text)
    main.box.text
}

mmp__combinedBoxes <- function(top,settings.box.text, main.box.text, bottom, box.width, settings.box.width, box.margins) {
    combined.text <- NULL
    for (i in 1:max(length(settings.box.text), length(main.box.text))) {
        combined.text <- c(combined.text,
                           paste0(
                               ifelse(i>length(settings.box.text),
                                      paste0("\u2551",
                                             cli::ansi_align("", width = settings.box.width, align = 'center'),
                                             "\u2502",
                                             strrep(" ", box.margins)), 
                                      settings.box.text[i]),
                               ifelse(i>length(main.box.text),
                                      cli::ansi_align("", width = box.width - settings.box.width - 1, align = 'center'),
                                      main.box.text[i]),
                               "\u2551",
                               "\n"))
        }
    combined.text <- c(top,combined.text,bottom)
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

MMP_test <- function() {
    for (i in 1:10) {
        for (j in 1:100000000) {
        }
        MMP_openning_banner()
        cat(paste0(i,'\n'))
        if (i > 5) STATUS$SETTINGS$status[4] <<- 'failure'
        }
    }
