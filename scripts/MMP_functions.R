
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

    runStage <- grep('--runStage=.*', args)
    if(length(runStage) == 0)
        stop('A run stage must be supplied as a command line argument, such as: Rscript <script.R> --runStage=1')
    runStage <- args[runStage]
    runStage <<- gsub('--runStage=(.*)','\\1', runStage)
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
  maxStringLength <- max(nchar(c(
      DATA_PATH,
      PARAMS_PATH,
      OUTPUT_PATH,
      DOCS_PATH,
      reportYear,
      currentTime)))
  cat(paste0(
      paste0(rep('*', 19 + maxStringLength + 1), collapse = ''), '*\n',
      '* Data path:       ', DATA_PATH, paste0(rep(' ', maxStringLength-nchar(DATA_PATH)), collapse=''), ' *\n',
      '* Parameters path: ', PARAMS_PATH, paste0(rep(' ', maxStringLength-nchar(PARAMS_PATH)), collapse=''), ' *\n',
      '* Output path:     ', OUTPUT_PATH, paste0(rep(' ', maxStringLength-nchar(OUTPUT_PATH)), collapse=''), ' *\n',
      '* Docs path:       ', DOCS_PATH, paste0(rep(' ', maxStringLength-nchar(DOCS_PATH)), collapse=''), ' *\n',
      '* Report Year:     ', reportYear, paste0(rep(' ', maxStringLength-nchar(reportYear)), collapse=''), ' *\n',
      '* Date:            ', currentTime, paste0(rep(' ', maxStringLength-nchar(currentTime)), collapse=''), ' *\n',
    paste0(rep('*',19 + maxStringLength + 2),collapse=''),'\n',
    collapse=''
  ))
}
