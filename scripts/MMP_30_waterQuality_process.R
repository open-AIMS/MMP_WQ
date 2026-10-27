source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

assign("CURRENT_STAGE", 3, env = globalenv())

unlink(paste0(DATA_PATH, "/reports/STAGE", CURRENT_STAGE, "_*.RData"))

## assign("DOC_REPORT_LIST", list(), env = globalenv())

## - wq.sites
## - lookup
## - wd.guidelines
## - river.lookup
## - LTmedian.discharge.river
source("MMP_30.5_parameter_files.R")

## - AIMS niskin
## - AIMS Cairns transect
## - JCU niskin
## - JCU CY niskin
## - JCU Event niskin
## - JCU CY Event niskin
source("MMP_31_waterQuality_process_niskin.R")

## - AIMS FLNTU logggers
## - Water temperature loggers
## - Salinity loggers
source("MMP_32_waterQuality_process_loggers.R")

## - Degree heating weeks
## - Disturbance table
## - tides
## - BOM weather
source("MMP_33_waterQuality_load_other.R")

source("MMP_34_waterQuality_process_timeseries.R")

## Preapare data for indices
source("MMP_35_processedData_indices_data.R")

## Prepare data for GAMMs - note these are now for GPs (I need to change the names)
source("MMP_36_processedData_gam_data.R")

## Produce report
source("MMP_35_processedData_report.R")

if (1 == 2) {
  
## Package up all the data that go into analyses
source("MMP_38_package_up_data.R")

}

if (length(list.files("../data/reports", pattern = "STAGE3", full.names = TRUE)) == 0) {
  if (length(list.files("../data/reports/backup", pattern = "STAGE3", full.names = TRUE)) != 0) {
    report_files <- list.files("../data/reports/backup", pattern = "STAGE3", full.names = TRUE)
    fname <- basename(report_files)
    for (f in 1:length(fname))
      file.copy(from = report_files[f], paste0("../data/reports/", fname[f]),
        recursive = TRUE)
    }
} else {
  report_files <- list.files("../data/reports", pattern = "STAGE3", full.names = TRUE)
  fname <- basename(report_files)
  for (f in 1:length(fname))
    file.copy(from = report_files[f], paste0("../data/reports/backup/", fname[f]),
      recursive = TRUE)
}
