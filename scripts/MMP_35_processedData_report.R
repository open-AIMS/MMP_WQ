source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

DOC_BASE_TEMPLATE <<- paste0(DOCS_PATH, "/MMP_processData.editMe")
## DOC_OUTPUT <<- paste0(DOCS_PATH, "/MMP_processData_report.Rmd")
DOC_OUTPUT <<- paste0(DOCS_PATH, "/MMP_processData_report.qmd")


## 1. get the list of doc list files
files <- list.files(paste0(DATA_PATH, "/reports"), full.names = TRUE)

## 2. compile a list of items:
##    - parameter files
##    - STATUS items
##  this is necessary because the parameter files are not listed in the
##  STATUS list (since they are not extracted, compiled or processed)
## ITEMS <- c('ParamFiles',
##           str_remove(STATUS[[paste0("STAGE",CURRENT_STAGE)]]$title, "Stage.*-."),
##            STATUS[[paste0("STAGE",CURRENT_STAGE)]]$items)
ITEMS <- 'ParamFiles'
for (i in 3:CURRENT_STAGE) {
    ITEMS <- c(ITEMS, 
               str_remove(STATUS[[paste0("STAGE",i)]]$title, "Stage.*-."),
               STATUS[[paste0("STAGE",i)]]$items)
}


## 3. order according to order of items in ITEMS (params then STATUS)
files <- files[sapply(ITEMS, function(x) {
    str_which(files, x)
}, USE.NAMES = FALSE) %>%
unlist]

## 3. paste doc lists together into a single string
## doc_string <- sapply(str_subset(files, 'STAGE3'),
doc_string <- sapply(files,
       function(x) {
           load(x)
           doc_list
       }
       ) %>%
    unlist(use.names = FALSE) %>%
    paste(collapse = '\n')

## 4. add the current log onto the end
doc_string <- paste0(doc_string,
                     "\n\n# Log file\n\n",
                     '``` LOG\n',
                     read_file(LOG_FILE),
                     '```\n'
                     )

## 5. take the template (docs/MMP_processData.editMe) and copy it to
## the quarto file
file.copy(from = DOC_BASE_TEMPLATE,
          to = DOC_OUTPUT,
          overwrite = TRUE) 

## 6. append the doc_string to the quarto file
write(doc_string,
      file = DOC_OUTPUT,
      append = TRUE)
           
## 7. Render document to html
quarto::quarto_render(DOC_OUTPUT)

