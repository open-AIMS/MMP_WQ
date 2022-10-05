
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
    if (runStage == 1) {
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
    STATUS <<- list(
        PATHS = list(title = 'Create paths',
                      items = c('DATA_PATH', 'OUTPUT_PATH', 'PARAMS_PATH', 'DOCS_PATH'),
                      names = c('Data path', 'Output path', 'Parameters path', 'Documents path'),
                      status = 'pending'
                      )
    )
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
    reportYear <<- gsub('--reportYear=(.*)','\\1', reportYear)

    STATUS$PATHS$items <<- c(STATUS$PATHS$items, 'reportYear')
    STATUS$PATHS$names <<- c(STATUS$PATHS$names, 'Report year')
    
    runStage <- grep('--runStage=.*', args)
    if(length(runStage) == 0)
        stop('A run stage must be supplied as a command line argument, such as: Rscript <script.R> --runStage=1')
    runStage <- args[runStage]
    runStage <<- gsub('--runStage=(.*)','\\1', runStage)
    STATUS$PATHS$items <<- c(STATUS$PATHS$items, 'runStage')
    STATUS$PATHS$names <<- c(STATUS$PATHS$names, 'Run stage')
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
    ## location of folder containing perpetual data used in this project
    PARAMS_PATH <<- '../parameters'
    ## location of folder containing outputs (individual figures and tables)
    OUTPUT_PATH <<- '../outputs'
    ## location of folder containing generated documents 
    DOCS_PATH <<- '../docs'
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
}


#########################################################################
## The following function generates an informative banner that         ##
## displays the a range of settings associated with the analysis       ##
#########################################################################
MMP_openning_banner <- function(){
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

  STATUS$PATHS$items <- c(STATUS$PATHS$items, 'currentTime')
  STATUS$PATHS$names <- c(STATUS$PATHS$names, 'Date/Time')
  
  box.style <- cli:::box_styles()
  box.width <- 80
  box.margins <- 1
  
  ## get the width of the path box
  path.box.nchar <-nchar(
          paste0(STATUS$PATHS$names, ': ',sapply(STATUS$PATHS$items, function(x) eval(parse(text = x))))
      )
  path.box.width <- max(path.box.nchar) +
  2 +              # add one for the status character
  box.margins*2    # add the left and right margin
  
  ## Outer box (top)
  top <- paste0(box.style["double", "top_left"],
                strrep(box.style["double", "horizontal"], path.box.width),
                '\u2564',
                strrep(box.style["double", "horizontal"], box.width - path.box.width),
                box.style["double", "top_right"],
                "\n"
                )
  cat(top)
  path.box.text <- NULL
  keys <- STATUS$PATH$names
  values <- sapply(STATUS$PATHS$items, function(x) eval(parse(text = x)))           
  for (i in 1:length(keys)) {
      path.box.text <- c(path.box.text,
                         paste0(box.style["double", "vertical"],
                                strrep(" ", box.margins),
                                crayon::green(cli::symbol$tick), " ", crayon::blue(keys[i]), ": ",
                                crayon::white(values[i]),
                                strrep(" ", path.box.width - (path.box.nchar[i])-box.margins*2 -1),
                                box.style["single", "vertical"],
                                strrep(" ", box.margins)
                                )
                         )
  }

  title.box.text <- c("MMP Water Quality Report Analysis", "")
  title.box.nchar <- nchar(title.box.text)
  
  for (i in 1:max(length(path.box.text), length(title.box.text))) {
      cat(paste0(path.box.text[i],
                 ifelse(i>length(title.box.text),
                        cli::ansi_align("", width = box.width - path.box.width - 1, align = 'center'),
                        cli::ansi_align(title.box.text[i], width = box.width - path.box.width - 1, align = 'center')),
                 box.style["double", "vertical"],
                 "\n"))
  }

  ## Outer box (bottom)
  bottom <- paste0(box.style["double", "bottom_left"],
                strrep(box.style["double", "horizontal"], path.box.width),
                '\u2567',
                strrep(box.style["double", "horizontal"], box.width - path.box.width),
                box.style["double", "bottom_right"],
                "\n"
                )
  cat(bottom)
  
  
}

MMP_test <- function() {
    for (i in 1:10) {
        for (j in 1:100000000) {
        }
        system('clear')
        MMP_openning_banner()
        cat(paste0(i,'\n'))
        }
    }
