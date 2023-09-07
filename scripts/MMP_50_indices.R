source("MMP_functions.R")
source("MMP_functions_indices.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

assign("CURRENT_STAGE", 5, env = globalenv())
CURRENT_ITEM <- "calculate indices"

#######################################################################################################
## 2018 Notes                                                                                        ##
## 1. Renee has indicated that we need to exclude all JCU PN and PP data from the index calculations ##
#######################################################################################################

## We need to decide what data are going to contribute to index and what index metric we are going to use...

## Differences to last year
## - not based on four year running means
## - cairns transect included
## - jcu data included
## - numerous additional sites
## - Measures grouped into Subindicators (Nutrients, Physico-chem, Chlorophyll)
## - index rescaled to the range [0,1]]

## The following are commented out.. The code essentially converts the
## old units information that were present in the guidelines file into
## a dedicated units file.
## Triggers <- read.table('parameters/WQ.guidelines.txt', header=TRUE, sep=';', strip.white=TRUE)
## wq.units = Triggers %>% rename(Measure=var, Name.graphs=units, Name.graphs.abbr=ShortRName, Name.tab.abbr=ShortName, Name.latex.abbr=ShortLatexName, Name.latex=Parameter) %>%
##     dplyr:::select(Measure, Name.graphs, Name.graphs.abbr, Name.tab.abbr, Name.latex, Name.latex.abbr, ymin,ymax)
##write.table(wq.units, file='parameters/wq.units.txt', row.names=FALSE, quote=FALSE, sep=';')

## Calculate water quality index THE OLD WAY, yet using the new
## guidelines:
## - melt the data
## - join with guideline values
## - only use the annual values (NOT WET AND DRY SEASONS)
## - 4 year running means
## - calculate extremes

#pdf(file=paste0("log/",reportYear,"/waterQualityIndices.pdf"), width=15, height=10)

## The historic data were only collected three times per year (1 wet,
## 2 dry).  Consequently, a four year, right-aligned rolling mean was
## applied (to smooth out fluctuations resulting from the time/date of
## sampling influence - which is larger with smaller samples).  The
## new site-specific guidelines have been developed based on the
## historic data for each site for the intention of measuring the
## relative state of each site in its own historical context.  It would be inappropriate to apply
## these guidelines to the historic data as this introduces
## substantiall circularity. 

## For this year, there will be four indices
## 1. <2016 formulation.
##    - 4 year running means of each depth weighted average anolyte
##    - previous guideline values based only on broad waterbodies
##    - only AIMS sites used in previous years (no JCU sites)
##    - indices calculated as simple average of PN, PP, NOx, CHL-a and
##      mean of (TSS,NTU)
## 2. an intermediate alternative
##    - all AIMS sites (no JCU sites)
##    - new (site-specific) guideline values (annual only)
##    - calculated from 4 year running averages
##    - indices calculated as simple average of PN, PP, NOx, CHL-a and
##      mean of (TSS,NTU)
##    - NOTE - this alternative introduces circularity
## 3. an alternative that calculates a 2016 index based purely on AIMS data
##    - all AIMS sites (no JCU sites)
##    - new guideline values
##    - indices calculated as simple average of PN, PP, NOx, CHL-a and
##      mean of (TSS,NTU)
##    - report Year based on water year
## 4. an alternative that calculates a 2016 index based on AIMS and JCU data
##    - all sites (AIMS + JCU)
##    - hierarchical indices
##    - separate annual indices for each source (AIMS, JCU)
##    - aggregated indices
## 5. an alternative that calculates the 2016 index based on AIMS +
##      JCU data
##    - all sites (AIMS + JCU) - although no PP or PN from JCU
##    - hierarchical indices
##    - aggregated data (for wet and dry separately)
##    - indices based on wet/dry guidelines - aggregate wet and dry
##    into annual index

MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                       SECTION = paste0("# ", str_to_title(CURRENT_ITEM), "\n\n"),
                       TABSET = paste0("::: panel-tabset \n\n"),
                       TABSET_END = paste0("::: \n\n")
                       )

source("MMP_51_indices_0.R")

source("MMP_52_indices_1.R")

source("MMP_53_indices_2.R")

source("MMP_35_processedData_report.R")
