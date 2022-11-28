source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

DOC_BASE_TEMPLATE <<- paste0(DOCS_PATH, "/MMP_processData.editMe")
## DOC_OUTPUT <<- paste0(DOCS_PATH, "/MMP_processData_report.Rmd")
DOC_OUTPUT <<- paste0(DOCS_PATH, "/MMP_processData_report.qmd")

load(file = paste0(DATA_PATH, "/processed/DOC_REPORT_LIST.RData"))

## 1. Take the template (docs/MMP_processData.editMe)

file.copy(from = DOC_BASE_TEMPLATE,
          to = DOC_OUTPUT,
          overwrite = TRUE) 
write(unlist(DOC_REPORT_LIST),
      file = DOC_OUTPUT,
      append = TRUE)
           
## 3. Render document to html
quarto::quarto_render(DOC_OUTPUT)

## system(paste0('quarto render ', DOC_OUTPUT))
## system(paste0('quarto render ../docs/MMP_processData.qmd'))



## rmarkdown::render(DOC_OUTPUT,
##                   output_format = my_html_document(template = paste0("../docs/resources/report_template.html"),
##                                                    ## css = "../resources/style.css",
##                                                    theme = "spacelab",
##                                                    toc = TRUE,
##                                                    toc_float = TRUE,
##                                                    highlight = "pygments")
##                   )
##                output_format = html_document())





## a <- readr::read_file(DOC_BASE_TEMPLATE)


## b <- "a bit more text"

## b <- str_replace(a, '# References\n', paste0(b, '\n\n# References\n'))

## readr::write_file(b, DOC_OUTPUT)

## file.copy(from = DOC_BASE_TEMPLATE,
##           to = DOC_OUTPUT,
##           overwrite = TRUE) 


## a <- readr::read_file(DOC_BASE_TEMPLATE)
## b <- jsonlite::toJSON(a, pretty = TRUE)

## jsonlite::toJSON(rbind(list(jsonlite::fromJSON(b),
##                             jsonlite::fromJSON(b))),
##                  pretty = TRUE)
## b

## jsonlite::toJSON(b, auto_unbox = TRUE) %>% cat 
