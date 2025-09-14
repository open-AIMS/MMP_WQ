#########################################################################
## The following function expresses takes the date and returns the     ##
## water year.  Unlike a calendar year (which spans from Jan 01 to Dec ##
## 31), the water year starts on October 1 and goes through to         ##
## September 31.  This provides a reference year for teh sampling      ##
## effort.                                                             ##
## Parameters:                                                         ##
##    Dt:      a Date                                                  ##
## Return:                                                             ##
##    numeric: a numeric representation of the year                    ##
#########################################################################
MMP_waterYear <- function(Dt) {
    as.numeric(as.character(format(Dt+(as.Date("1970-12-31")-as.Date("1970-10-01")+1), format="%Y")))
}

MMP_reneeYear <- function(Dt) {
    as.numeric(as.character(format(Dt+(as.Date("1970-12-31")-as.Date("1970-09-01")+1), format="%Y")))
}

MMP_financialYear <- function(Dt) {
    as.numeric(as.character(format(Dt+(as.Date("1970-12-31")-as.Date("1970-07-01")+1), format="%Y")))
}    

MMP_oldSamplingYear <- function(Dt) {
       as.numeric(as.character(format(Dt+(as.Date("1970-12-31")-as.Date("1970-04-15")+1), format="%Y")))
}
#########################################################################
## The following function expresses takes the date and returns the     ##
## water year.  Unlike a calendar year (which spans from Jan 01 to Dec ##
## 31), the water year starts on October 1 and goes through to         ##
## September 31.  This provides a reference year for teh sampling      ##
## effort.                                                             ##
## Parameters:                                                         ##
##    Dt:      a Date                                                  ##
## Return:                                                             ##
##    numeric: a numeric representation of the year                    ##
#########################################################################
MMP_categoricalWaterYear <- function(Date) {
  yr <- as.numeric(as.character(format(Date+(as.Date("1970-12-31")-as.Date("1970-10-01")+1), format="%Y")))
  yr0 <-yr-1
  factor(paste(yr0,"-",yr))
}

MMP_categoricalFinancialYear <- function(Date) {
  yr <- as.numeric(as.character(format(Date+(as.Date("1970-12-31")-as.Date("1970-07-01")+1), format="%Y")))
  yr0 <-yr-1
  factor(paste(yr0,"-",yr))
}



#######################################################################
## The following function generates decimal date version of the Date ##
## field                                                             ##
## Parameters:                                                       ##
##    dt:       a Date                                               ##
## Return:                                                           ##
##    numberic: a numeric (decimal) version of the data              ##
#######################################################################
MMP_decimalDate <- function(dt) {
    ## Lubridate seems not to handle missing dates very well (or at all for some functions)
    ## So I will create a temporary variable to store the dates, replacing NA with 1900-01-01
    ## and then after converting to decimal, convert the 1900 values to NA
    dt <- as.Date(ifelse(is.na(dt), '1900-01-01', as.character(dt)))
    dt <- decimal_date(dt)
    dt <- ifelse(dt==1900, NA, dt)
    dt
}

MMP_correctLocations <- function(data) {
    data$LOCATION_NAME[data$LOCATION_NAME=="Orpheus Island" & data$Date > as.Date("2006-01-01")] <- "Pelorus / Orpheus Island"
    data$LOCATION_NAME[data$LOCATION_NAME=="Long Island" & data$Date > as.Date("2005-07-30") & data$Date < as.Date("2007-03-10")] <- "Pine Island"
    data$MMP_SITE_NAME[data$MMP_SITE_NAME=="Dunk South"] <- "Dunk Island South East"
    droplevels(data)
}

#######################################################################
## The following function reorders the reef.alias such that they are ##
## in a North-South, East-West order more useful for panel plots.    ##
## Parameters:                                                       ##
##     x:      factor containing reef.alias                          ##
## Returns:                                                          ##
##     x:      ordered factor containing reef.alias                  ##
#######################################################################
MMP_reorderReefs <- function(data) {
    data %>% arrange(-LATITUDE,LONGITUDE) %>%
        ## mutate(reef.alias=factor(reef.alias, levels=unique(reef.alias)))
        mutate(MMP_SITE_NAME=factor(MMP_SITE_NAME, levels=unique(MMP_SITE_NAME)))
}

MMP_selectReefs <- function(data,source='niskin') {
    ## lookup <- read_csv(paste0(PARAMS_PATH, '/lookup.csv'), trim_ws = TRUE) %>% suppressMessages()
    lookup <- read.csv(paste0(PARAMS_PATH, '/lookup.csv'), strip.white = TRUE) %>% suppressMessages()
    if (source=='niskin') {
        rfs <- as.character((lookup %>% filter(Niskin==T))$SHORT_NAME)
        data <- data %>% filter(SHORT_NAME %in% rfs) %>% droplevels %>% mutate(MMP=TRUE)
    } else if (source=='JCU') {
        rfs <- as.character((lookup %>% filter(JCU==T))$SHORT_NAME)
        data <- data %>% filter(SHORT_NAME %in% rfs) %>% droplevels
    } else if (source=='flntu') {
        rfs <- as.character((lookup %>% filter(FLNTU==T))$SHORT_NAME)
        data <- data %>% filter(SHORT_NAME %in% rfs) %>% droplevels  
    } else if (source=='coral') {
        rfs <- as.character((lookup %>% filter(Coral==T))$SHORT_NAME)
        data <- data %>% filter(SHORT_NAME %in% rfs) %>% droplevels 
    } else if (source=='WaterTemp') {
        rfs <- as.character((lookup %>% filter(WaterTemp==T))$SHORT_NAME)
        data <- data %>% filter(SHORT_NAME %in% rfs) %>% droplevels 
    }
    data
}

########################################################################
## The following functions were used for the Piecewise (broken stick) ##
## model relating PN with the new machine to PN with the old machine. ##
## A piecewise regression was used as it was clear that the           ##
## relationship displayed two substantially different trajectories.   ##
########################################################################
after <- function(x,bp) ifelse(x>bp, x-bp,0)
before <- function(x,bp) ifelse(x>bp, x, x)

###############################################################################
## The following function corrects 'HAND_NH4' for minimum detection.         ##
## It seems that all the other chemicals have already had this done          ##
## prior to or within the database.                                          ##
## Parameters:                                                               ##
##    data:   a data frame containing the chemicals in their stored units    ##
## Returns:                                                                  ##
##    data:   a data frame containing the chemicals in their reporting units ##
###############################################################################
MMP_limitDetection <- function(data) {
    cols <- colnames(data)
    if(any(cols == 'HAND_NH4_UM')) data$HAND_NH4_UM<- ifelse(data$HAND_NH4_UM>0.01, data$HAND_NH4_UM, 0.005)
    data
}

###############################################################################
## This function converts from measured/stored units into units              ##
## required for MMP reporting.  It does so by multiplying by the             ##
## atomic mass of each.                                                      ##
## Parameters:                                                               ##
##    data:   a data frame containing the chemicals in their stored units    ##
## Returns:                                                                  ##
##    data:   a data frame containing the chemicals in their reporting units ##
###############################################################################
MMP_convertUnits <- function(data) {
    cols <- colnames(data)
    if(any(cols == 'DIP_UM'))      data$DIP <- data$DIP_UM*30.97
    if(any(cols == 'PP_UM'))       data$PP <- data$PP_UM*30.97
    if(any(cols == 'TDP_UM'))      data$TDP <- data$TDP_UM*30.97

    if(any(cols == 'SI_UM'))       data$SI <- data$SI_UM*28.09

    if(any(cols == 'NH4_UM'))      data$NH4 <- data$NH4_UM*14.001
    if(any(cols == 'NO2_UM'))      data$NO2 <- data$NO2_UM*14.001
    if(any(cols == 'NO3_UM'))      data$NO3 <- data$NO3_UM*14.001
    if(any(cols == 'HAND_NH4_UM')) data$HAND_NH4 <- data$HAND_NH4_UM*14.001
    if(any(cols == 'PN_UM'))       data$PN <- data$PN_UM*14.001
    if(any(cols == 'TDN_UM'))      data$TDN <- data$TDN_UM*14.001
    if(any(cols == 'PN_SHIM_UM'))  data$PN_SHIM<- data$PN_SHIM_UM*14.001
    
    if(any(cols == 'DOC_UM'))      data$DOC <- data$DOC_UM*12.011
    if(any(cols == 'POC_UM'))      data$POC <- data$POC_UM*12.011
    
    data
}

###############################################################################
## The following function generates different combinations of                ##
## chemicals when each of those primary chemicals are present in the         ##
## data. The only complication is that when there are missing data for       ##
## one of the chemicals, the resulting combination should also be NA.        ##
## As of the 2017/2018 report year, HAND_NH4 is only measured on the         ##
## Cape Fergusen and not other vessels.  Consequently, this should           ##
## only be used in calculations for the Cairns Transect.  For all            ##
## others (including index calculations involving Cairns transect            ##
## sites), NH4 should be used.                                               ##
## NOTE, it is really only the non _UM versions of the data that we          ##
## are going to use.  The _UM versions are from the database, but the        ##
## non _UM versions are those in the correct units.                          ##
## Parameters:                                                               ##
##    data:   a data frame containing the chemicals in their stored units    ##
## Returns:                                                                  ##
##    data:   a data frame containing the chemicals in their reporting units ##
###############################################################################
MMP_derivedChem <- function(data, type='MMP') {
    cols <- colnames(data)
    if(type=='MMP' & any(cols == 'NO3') & any(cols == 'NO2') & any(cols == 'NH4')) {
        data$DIN<- data$NO3+data$NO2+data$NH4
        data$DIN[is.na(data$NH4)] <- NA
    }
    if(type=='Cairns' & any(cols == 'NO3') & any(cols == 'NO2') & any(cols == 'HAND_NH4')) {
        data$DIN<- data$NO3+data$NO2+data$HAND_NH4
        data$DIN[is.na(data$HAND_NH4)] <- NA
    }
    if(type=='MMP' & any(cols == 'NO3_UM') & any(cols == 'NO2_UM') & any(cols == 'NH4_UM')) {
        data$DIN_UM<- data$NO3_UM+data$NO2_UM+data$NH4_UM
        data$DIN_UM[is.na(data$NH4_UM)] <- NA
    }
    if(type=='Cairns' & any(cols == 'NO3_UM') & any(cols == 'NO2_UM') & any(cols == 'HAND_NH4_UM')) {
        data$DIN_UM<- data$NO3_UM+data$NO2_UM+data$HAND_NH4_UM
        data$DIN_UM[is.na(data$HAND_NH4_UM)] <- NA
    }
    if(any(cols == 'NO3') & any(cols == 'NO2')) {
        data$NOx<- data$NO3+data$NO2
        data$NOx[data$NOx==0] <- 0.01
    }
    if(any(cols == 'NO3_UM') & any(cols == 'NO2_UM')) {
        data$NOx_UM<- data$NO3_UM+data$NO2_UM
        data$NOx_UM[data$NOx_UM==0] <- 0.01
    }
    if(type=='MMP' & any(cols == 'TDN') & any(cols == 'NH4') & any(cols == 'NO3') & any(cols == 'NO2') & any(cols == 'NH4')) {
        data$DON<- data$TDN - data$NH4 - data$NO3 - data$NO2
        if (any(cols=='TDN_UM')) data$DON_UM<- data$TDN_UM- data$NH4_UM- data$NO3_UM- data$NO2_UM
    }
    if(type=='Cairns' & any(cols == 'TDN') & any(cols == 'NH4') & any(cols == 'NO3') & any(cols == 'NO2') & any(cols == 'HAND_NH4')) {
        data$DON<- data$TDN - data$NH4 - data$NO3 - data$NO2
        data$DON_UM<- data$TDN_UM- data$NH4_UM- data$NO3_UM- data$NO2_UM
    }
    if(any(cols == 'DIP')) {
        data$PO4<- data$DIP
        data$PO4_UM<- data$DIP_UM
    }
    if(any(cols == 'TDP') & any(cols == 'DIP')) data$DOP<- data$TDP - data$DIP
    if(any(cols == 'TDP_UM') & any(cols == 'DIP_UM')) data$DOP_UM<- data$TDP_UM- data$DIP_UM

    if(any(cols == 'POC') & any(cols == 'DRIFTCHL_UGPERL')) {
        data$POC_CHL<- data$POC / data$DRIFTCHL_UGPERL
    }
    if(any(cols == 'POC_UM') & any(cols == 'DRIFTCHL_UGPERL')) {
        data$POC_CHL_UM<- data$POC_UM/ data$DRIFTCHL_UGPERL
    }
    if(any(cols == 'PN') & any(cols == 'DRIFTCHL_UGPERL')){
        data$PN_CHL<- data$PN / data$DRIFTCHL_UGPERL
    }
    if(any(cols == 'PN_UM') & any(cols == 'DRIFTCHL_UGPERL')){
        data$PN_CHL_UM<- data$PN_UM/ data$DRIFTCHL_UGPERL
    }
    if(any(cols == 'PP') & any(cols == 'DRIFTCHL_UGPERL')) {
        data$PP_CHL<- data$PP / data$DRIFTCHL_UGPERL
    }
    if(any(cols == 'PP_UM') & any(cols == 'DRIFTCHL_UGPERL')) {
        data$PP_CHL_UM<- data$PP_UM/ data$DRIFTCHL_UGPERL
    }
    if(any(cols == 'TSS_MGPERL') & any(cols == 'DRIFTCHL_UGPERL')) data$CHL_TSS<- data$DRIFTCHL_UGPERL /data$TSS_MGPERL
    if(any(cols == 'POC') & any(cols == 'TSS_MGPERL')) {
        data$POC_TSS<- (data$POC/1000)/ data$TSS_MGPERL
    }
    if(any(cols == 'POC_UM') & any(cols == 'TSS_MGPERL')) {
        data$POC_TSS_UM<- (data$POC_UM/1000)/ data$TSS_MGPERL
    }
    if(any(cols == 'PN') & any(cols == 'TSS_MGPERL')) data$PN_TSS<- (data$PN/1000) / data$TSS_MGPERL
    if(any(cols == 'PP') & any(cols == 'TSS_MGPERL')) data$PP_TSS<- (data$PP/1000) / data$TSS_MGPERL
    if(any(cols == 'POC') & any(cols == 'PN')) data$POC_PN<- data$POC/ data$PN
    if(any(cols == 'POC_UM') & any(cols == 'PN_UM')) data$POC_PN_UM<- data$POC_UM/ data$PN_UM
    if(any(cols == 'POC') & any(cols == 'PP')) data$POC_PP<- data$POC/ data$PP
    if(any(cols == 'POC_UM') & any(cols == 'PP_UM')) data$POC_PP_UM<- data$POC_UM/ data$PP_UM
    
    if(any(cols == 'PP') & any(cols == 'PN')) data$PN_PP<- data$PN/ data$PP
    if(any(cols == 'PP_UM') & any(cols == 'PN_UM')) data$PN_PP_UM<- data$PN_UM/ data$PP_UM
    
    if(any(cols == 'PN') & any(cols == 'TDN')) data$TotalN<- data$PN + data$TDN
    if(any(cols == 'PN_UM') & any(cols == 'TDN_UM')) data$TotalN_UM<- data$PN_UM+ data$TDN_UM
    
    if(any(cols == 'PP') & any(cols == 'TDP')) data$TotalP<- data$PP + data$TDP
    if(any(cols == 'PP_UM') & any(cols == 'TDP_UM')) data$TotalP_UM<- data$PP_UM+ data$TDP_UM
 
    cols <- colnames(data)
    if(any(cols == 'TotalN') & any(cols == 'TotalP')) data$TN_TP<- data$TotalN / data$TotalP
    if(any(cols == 'TotalN_UM') & any(cols == 'TotalP_UM')) data$TN_TP_UM<- data$TotalN_UM/ data$TotalP_UM
    
    if(any(cols == 'DOC') & any(cols == 'DON')) data$DOC_DON<- data$DOC / data$DON
    if(any(cols == 'DOC_UM') & any(cols == 'DON_UM')) data$DOC_DON_UM<- data$DOC_UM/ data$DON_UM
 
    if(any(cols == 'DOC') & any(cols == 'DOP')) data$DOC_DOP<- data$DOC / data$DOP
    if(any(cols == 'DOC_UM') & any(cols == 'DOP_UM')) data$DOC_DOP_UM<- data$DOC_UM/ data$DOP_UM
 
    if(any(cols == 'DON') & any(cols == 'DOP')) data$DON_DOP<- data$DON / data$DOP
    if(any(cols == 'DON_UM') & any(cols == 'DOP_UM')) data$DON_DOP_UM<- data$DON_UM/ data$DOP_UM
 
    if(any(cols == 'DON_UM') & any(cols == 'DOP_UM')) {
        data$DON_DOP_UM<- data$DON_UM/ data$DOP_UM
    }

    if(any(cols == 'NOx') & any(cols == 'PO4')) data$NOx_PO4<- data$NOx / data$PO4
    if(any(cols == 'NOx_UM') & any(cols == 'PO4_UM')) data$NOx_PO4_UM<- data$NOx_UM/ data$PO4_UM
    if(any(cols == 'SI') & any(cols == 'NOx_UM')) data$SI_NOx_UM<- data$SI/ data$NOx_UM
    if(any(cols == 'SI') & any(cols == 'NOx')) data$SI_NOx<- data$SI/ data$NOx
    if(any(cols == 'SI') & any(cols == 'PO4_UM')) data$SI_PO4_UM<- data$SI/ data$PO4_UM
    if(any(cols == 'SI') & any(cols == 'PO4')) data$SI_PO4<- data$SI/ data$PO4
    data
}

###############################################################################
## This function performs a conditional sequential aggregation on the        ##
## data based on the STATION_CLASS (CR or R).  Data are first split          ##
## according to STATION_NAME and then depending on whether the               ##
## STATION_CLASS is CR or R, it will be aggregated differently across        ##
## DEPTH_CODE                                                                ##
## Parameters:                                                               ##
##    data:   a data frame containing the chemicals in their stored units    ##
## Returns:                                                                  ##
##    data:   a data frame containing the chemicals in their reporting units ##
###############################################################################

MMP_aggregateWQDuplicates <- function(data) {
    data.agg <- plyr:::ddply(subset(data),~STATION_NAME, function(x) {
        a1<-a2<-NULL         
        if (x$STATION_CLASS[1] == "CR") { ## Aggregate across depths
            a1<-plyr:::ddply(x, ~DEPTH_CODE, function(x) { ##Aggregate within depths
                data.frame(Station=unique(x$STATION_NAME),Date=mean(x$Date),Time=mean(x$Time),plyr:::numcolwise(mean,na.rm=TRUE)(x[,-which(colnames(x) %in% c("DUPLICATE"))]),
                           plyr:::catcolwise(function(x) x[1])(x))
            })
            data.frame(Station=unique(a1$Station),Date=mean(a1$Date),Time=mean(x$Time),plyr:::numcolwise(mean,na.rm=TRUE)(a1),
                       plyr:::catcolwise(function(a1) a1[1])(a1))
        }else{
            plyr:::ddply(x, ~DEPTH_CODE, function(x) { ##Aggregate within depths
                data.frame(Station=unique(x$STATION_NAME),Date=mean(x$Date),Time=mean(x$Time),plyr:::numcolwise(mean,na.rm=TRUE)(x[,-which(colnames(x) %in% c("DUPLICATE"))]),
                           plyr:::catcolwise(function(x) x[1])(x))
            })
        }
    })
    data.agg
}


## ########################################################################
## ## The following function is used by MMP_consecutiveDays to determine ##
## ## whether a sample has been collected over two consecutive days      ##
## ########################################################################
nextInSequence <- function(data,nms) {
  p1 <- substr(nms,1,3)
  p2 <- as.numeric(substr(nms,4,6))+1
  nms2<-paste(p1,sprintf("%03.0f",p2),sep="")
  nms2
}

######################################################################################
## The following function determines whether a sample has been                      ##
## collected over two consecutive days.                                             ##
## Parameters:                                                                      ##
##    data:   a data frame containing at least LOCATION_NAME and Station            ##
##    nms:    a vector of Station names (these are actually 6 digit codes)          ##
## Returns:                                                                         ##
##    data:   a data frame containing the a new field Collection that               ##
##              expresses the Station as a collection (that may span multiple days) ##
######################################################################################
MMP_consecutiveDaysOld <- function(data) {
    d<-plyr:::ddply(subset(data), ~LOCATION_NAME, function(x) {
        x<-x[order(x$Station),]     
                                        #get the set of stations
        nms <- as.character(unique(x$Station))
        dtt<-NULL   
                                        #get the set of stations that would be next in the sequence
        nms2<-nextInSequence(x,nms)
                                        #remove the next in sequence stations from the original set of stations
        nms<-nms[!nms %in% nms2]
                                        #   for (nm in nms[seq(1,length(nms), b=2)]) {
        for (nm in nms) {
                                        #print(nm)
            p1 <- substr(nm,1,3)
            p2 <- as.numeric(substr(nm,4,6))+1
            pp<-paste(p1,sprintf("%03.0f",p2),sep="")
                                        #print(pp)
            dtt<-rbind(dtt,data.frame(subset(x, Station %in% c(as.character(nm),pp)),Collection=nm))
        }
        dtt
    })
    d
}

## The above has an issue.  It works with the older data, but not with some new.
## As rough as it sounds, we used to know that a sample was the same (E.g. a R and CR) over multiple days, if
## the SAMPLE_NAME was the next number in the sequence.  As I said, this used to work.  However,
## it no longer seems to be adhered to.  As a result, there is a need to switch to another
## method.  I will try making a collection that is:
## Any records collected from a LOCATION_NAME within 48 hours of the first date.

MMP_consecutiveDays <- function(data) {
    d<-plyr:::ddply(subset(data), ~LOCATION_NAME, function(x) {
        x=arrange(x,Date)
        dts = unique(x$Date)
        dts2 = dts + 2
        dtt<-NULL
        dts=dts[!dts %in% dts2]
        for (dd in dts) {
            dtt= dtt %>% bind_rows(x %>% filter(between(Date,dd,dd+2)) %>% mutate(Collection=unique(filter(x,Date==dd)$STATION_NAME)[1]))
        }
        
        dtt
    })
    d %>% filter(!(is.na(PN_UM) & is.na(PP_UM))) ## This hopefully removes the isolated CR cases.  It will also remove some M (Mooring?) cases??
}


###############################################################################################
## Rules                                                                                     ##
## If the maximum depth is provided (as secchi depth) and is greater than the sample depths  ##
##  - add the maximum depth with a value of 0                                                ##
## If there is a missing value, then substitute in the value of the next depth, unless it is ##
##  the last value that is missing in which case it gets the same as the one above           ##
## If all of the depths are the same, the mean of the values is returned                     ##
## If all of the values are the same, then the first of these is returned                    ##
## For the remaining, trapezoidal integration is performed                                   ##
###############################################################################################
prepare.for.trapez <- function(y,depth,max.depth) {
  depth <- depth[order(depth)] 
  max.depth <- ifelse(all(is.na(max.depth)),NA,max(max.depth,na.rm=TRUE))
  if(!is.na(max.depth) & max(depth, na.rm=TRUE)<max.depth) {
    depth <- c(depth,max.depth)
    y <- c(y,NA)
  }
  miss <- which(is.na(y))
  y[miss]<-ifelse(miss==length(y), y[miss-1],y[miss+1])
  if(length(depth)>0){
    if(length(unique(y))==1) { #if all values are the same
      return(list(IntDepth=max(depth),y=y[1]))
    }else {
      #print(depth)
      if (length(unique(depth))==1){ #if all depths are the same
        return(list(IntDepth=max(depth), y=mean(y,na.rm=TRUE)))
      }else {
        return(list(IntDepth=max(depth), data.frame(y=y, x=depth)))
      }
    }
  }else return(list(IntDepth=NA, y=NA))
}
trapez <- function (y,x) 
{
  idx = 2:length(x)
  area <- (as.double((x[idx] - x[idx - 1]) %*% (y[idx] + y[idx - 1]))/2)
  return(list(Area=area, Mean=area/max(x)))
}
trapezoid.mean <- function(y, depth, max.depth) {
  data <- prepare.for.trapez(y, depth,max.depth)
  if (is.data.frame(data[[2]])) {
    return(trapez(data[[2]]$y, data[[2]]$x)$Mean)
  }else return(data[[2]])
}


#################################################################################################
## The following function calculates depth weighted averages based on                          ##
## trapezoidal integration.  There are numerous alteratives, depending                         ##
## on the chosen integration depth (how depth is measured that is).                            ##
## - wm weighted mean based on DEPTH                                                           ##
## - wmsd weighted mean based on SECCHI_DEPTH                                                  ##
## - wmad weighted mean based on ACCOUSTIC_DEPTH                                               ##
## These alternatives are calculated for each chemical etc                                     ##
## Parameters:                                                                                 ##
##    data:   a data frame containing the chemicals, Collection and various depth measurements ##
## Returns:                                                                                    ##
##    data:   a data frame containing the depth weighted averages                              ##
#################################################################################################

#########################################################################################################################################################################################################################################################
## NOTES:                                                                                                                                                                                                                                              ##
##    Integration Rules:                                                                                                                                                                                                                               ##
##                                                                                                                                                                                                                                                     ##
##    - If the maximum depth is provided (as either the maximum secchi or acoustic depth) and is this is greater than the sample depths,  then add the maximum depth and assign it a reading equal to the sample reading for the lowest sampling depth ##
##    - If any sampling measures are missing, then substitute in the value of the next lowest depth, unless it is the lowest depth that is missing, in which case, substitute the value of the sample at the depth above                               ##
##    - If all of the depths are the same, the mean of the corresponding sampling values is returned.                                                                                                                                                  ##
##    - If all of the sample measurements are the same, then return just the first of these.                                                                                                                                                           ##
##    - For the remaining, perform trapezoidal integration and return the mean value.                                                                                                                                                                  ##
#########################################################################################################################################################################################################################################################
MMP_depthWeightedAverages <- function(data) {
    data.av <- data %>% group_by(Collection) %>% arrange(DEPTH) %>%
        do({
            x <- .
            #print(x)
            #print(x$Collection)
            numeric_cols <- which(vapply(x, is.numeric, logical(1))==TRUE)
            data.frame(Date=mean(x$Date), Time=mean(x$Time),
                       IntDepthSD= prepare.for.trapez(x$DIP, x$DEPTH,x$SECCHI_DEPTH)$IntDepth,
                       IntDepthAD= prepare.for.trapez(x$DIP, x$DEPTH,x$ACOUSTIC_DEPTH)$IntDepth,
                       ## x %>% summarise_each(funs(mean(., na.rm=TRUE)), numeric_cols, -DEPTH),
                       x %>% summarise(across(c(where(is.numeric),-DEPTH), ~ mean(.x, na.rm = TRUE))),
                       plyr:::numcolwise(trapezoid.mean, depth=x$DEPTH, max.depth=x$DEPTH)(x[,-which(colnames(x) %in% c("DEPTH","STATION_CLASS"))]),
                       plyr:::numcolwise(trapezoid.mean, depth=x$DEPTH, max.depth=x$SECCHI_DEPTH)(x[,-which(colnames(x) %in% c("DEPTH","STATION_CLASS"))]),             
                       plyr:::numcolwise(trapezoid.mean, depth=x$DEPTH, max.depth=x$ACOUSTIC_DEPTH)(x[,-which(colnames(x) %in% c("DEPTH","STATION_CLASS"))]),
                       plyr:::catcolwise(function(x) x[1])(x)
                       )
        })
    colnames(data.av)<-gsub("(.*)\\.1","\\1.wm",colnames(data.av))
    colnames(data.av)<-gsub("(.*)\\.2","\\1.wmsd",colnames(data.av))
    colnames(data.av)<-gsub("(.*)\\.3","\\1.wmad",colnames(data.av))
    data.av$Month <- format(data.av$Date,"%b")
    data.av$Season <- factor(ifelse(data.av$Month %in% c("Nov","Dec","Jan","Feb","Mar","Apr"), "Wet","Dry"))
    data.av
}



#########################################################################
## The following function adds Region and Subregion to the data source ##
#########################################################################
MMP_region_subregion<- function(data, Source=NULL) {
    ## lookup <- read_csv(paste0(PARAMS_PATH, '/lookup.csv'), trim_ws = TRUE) %>% suppressMessages()
    ## coral.lookup <- read_csv(paste0(PARAMS_PATH, '/coral.lookup.csv'), trim_ws = TRUE) %>% suppressMessages()
    lookup <- read.csv(paste0(PARAMS_PATH, '/lookup.csv'), strip.white = TRUE) %>% suppressMessages()
    coral.lookup <- read.csv(paste0(PARAMS_PATH, '/coral.lookup.csv'), strip.white = TRUE) %>% suppressMessages()
    if (is.null(Source)) data %>% left_join(lookup %>% dplyr:::select(SHORT_NAME,Region,Reg,Subregion,Subreg)) %>% as.data.frame
    else if (Source=='Niskin')  data %>% left_join(lookup %>% dplyr:::select(SHORT_NAME,Region,Reg,Subregion,Subreg,Niskin)) %>% as.data.frame
    else if (Source=='FLNTU')  data %>% left_join(lookup %>% dplyr:::select(SHORT_NAME,Region,Reg,Subregion,Subreg,FLNTU)) %>% as.data.frame
    else if (Source=='WaterTemp')  data %>% left_join(lookup %>% dplyr:::select(SHORT_NAME,Region,Reg,Subregion,Subreg,WaterTemp)) %>% as.data.frame
    else if (Source=='JCU')  data %>% left_join(lookup %>% dplyr:::select(SHORT_NAME,Region,Reg,Subregion,Subreg,JCU)) %>% as.data.frame
    else if (Source=='CY')  data %>% left_join(lookup %>% dplyr:::select(SHORT_NAME,Region,Reg,Subregion,Subreg,JCU)) %>% as.data.frame
    else if (Source=='WaterSalinity')  data %>% left_join(lookup %>% dplyr:::select(SHORT_NAME,Region,Reg,Subregion,Subreg)) %>% as.data.frame
    ## DHD and disturbance are both related to coral data and these sites do not necessarily have a SHORT_NAME, so these will remain reef.alias.
    else if (Source=='DHD')  data %>% left_join(coral.lookup %>% dplyr:::select(MMP_SITE_NAME,Region,Reg,Subregion,Subreg)) %>% as.data.frame
    else if (Source=='disturbance')  data %>% left_join(coral.lookup %>% dplyr:::select(MMP_SITE_NAME,Region,Reg,Subregion,Subreg)) %>% as.data.frame
}

MMP_GBRMPA_specs <- function(data,WQ=TRUE) {
    if (WQ==TRUE) {
        wq.guidelines <- read.table(paste0(PARAMS_PATH, '/wq.guidelines.txt'), header=TRUE, sep=';', strip.white = TRUE)
        data %>%
            left_join(wq.guidelines %>%
                      ## dplyr:::select(GBRMPA_group, GBRMPA_water_area) %>%
                      separate_rows(SHORT_NAME, sep=',') %>%
                      dplyr::select(SHORT_NAME,GBRMPA_water_area) %>%
                      distinct()) %>%
            droplevels()
    }
}

MMP_designLatest <- function(data, WQ=TRUE) {
    if (WQ==TRUE) {
        wq.sites <- read_csv(paste0(PARAMS_PATH, '/wq.sites.csv'), trim_ws = TRUE) %>% suppressMessages()
        data %>%
            left_join(wq.sites %>% mutate(AIMS=rowSums(.[,c("Water (AIMS MMP)", "Water (AIMS and JCU)")],na.rm=TRUE),
                                          JCU=rowSums(.[,c("Water (JCU)", "Water (AIMS and JCU)")],na.rm=TRUE)) %>%
                      dplyr:::select(GBRMPA_group, SHORT_NAME, Water_Samples, AIMS, JCU) %>%
                      distinct()
                      ) %>%
            droplevels()
    }
}

###############################################################################
## The following function creates consistent reef names for the forams       ##
## data.  If the forams are no longer required, then this can be depreciated ##
##                                                                           ##
## Parameters:                                                               ##
##    loc:      character vector of foram index location names               ##
## Return:                                                                   ##
##    label:    character vector of reef alias names                         ##
###############################################################################
MMP_locationLabels <- function(loc) {
    require('car')
    Label<-car:::recode(loc,"'Snapper North'='Snapper Isl.';
                       'Fitzroy West'='Fitzroy Isl.';
                       'High West'='High Isl.';
                       'Franklands West'='Russell Isl.';
                       'Dunk North'='Dunk Isl.';
                       'Palms West'='Pelorus/Orpheous Isl.';
                       'Pandora'='Pandora Reef';
                       'Magnetic'='Geoffrey Bay';
                       'Haughton'='Haughton River';
                       'Burdekin Mouth'='Burdekin Mouth';
                       'Double Cone'='Double Cone Isl.';
                       'Daydream'='Daydream Isl.';
                       'Pine'='Pine Isl.';
                       'Barren'='Barren Isl.';
                       'Keppels South'='Humpy Isl.';
                       'Pelican'='Pelican Isl.';
                       'Seaforth'='Seaforth Isl.';
                       'Repulse'='Repulse Mooring'
      ")
    Label
}

###################################################################
## The following function adds a vector of booleans to indicate  ##
## whether the reef is a historic reef (TRUE) for the purpose of ##
## backwards compatibility.                                      ##
###################################################################
MMP_HistoricReef <- function(reef) {
    ifelse(reef %in% c('Cape Tribulation',
                       'Port Douglas',
                       'Double Island',
                       'Green Island',
                       "Yorkey's Knob",
                       'Fairlead Buoy',
                       'Fitzroy West',
                       'High West',
                       'Franklands West',
                       'Dunk North',
                       'Palms West',
                       'Pandora',
                       'Magnetic',
                       'Haughton 2',
                       'Double Cone',
                       'Pine',
                       'Seaforth',
                       'Repulse Islands dive mooring',
                       'Barren',
                       'Keppels South',
                       'Pelican'),
           TRUE,FALSE)
}


########################################################################
## The following function adds a Field to the bom data that indicates ##
## a Location for where the data are collected.  This is a little     ##
## easier to work with than the STATION_NUMBER.                       ##
## Parameters:                                                        ##
##    data:    a dataframe containing STATION_NUMBER                  ##
## Returns:                                                           ##
##    data:    a dataframe containing LOCATION                        ##
########################################################################
MMP_bomStations <- function(bom) {  
    ## it seems that in more recent times, there are additional 0's at
    ## the start of the STATION_NUMBER
    bom$STATION_NUMBER <- str_replace(bom$STATION_NUMBER, '^0+', '')
    bom$LOCATION <- 'NA'
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="31011", "Cairns Aero",bom$LOCATION)          #MMP
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="31037", "Low Isles Lighthouse",bom$LOCATION) #MMP
    bom$LOCATION<-ifelse(bom$STATION_NUMBER=="31192", "Green Island",bom$LOCATION)           #MMP
    bom$LOCATION<-ifelse(bom$STATION_NUMBER=="31213", "Cape Flattery",bom$LOCATION)          #MMP
    bom$LOCATION<-ifelse(bom$STATION_NUMBER=="32040", "Townsville Aero",bom$LOCATION)        #MMP
    bom$LOCATION<-ifelse(bom$STATION_NUMBER=="32141", "Lucinda Point",bom$LOCATION)          #MMP
    bom$LOCATION<-ifelse(bom$STATION_NUMBER=="33106", "Hamilton Island Airport",bom$LOCATION)#MMP
    bom$LOCATION<-ifelse(bom$STATION_NUMBER=="33210", "St Lawrence",bom$LOCATION)            #MMP
    bom$LOCATION<-ifelse(bom$STATION_NUMBER=="33247", "Proserpine Airport",bom$LOCATION)     #MMP 
    bom$LOCATION<-ifelse(bom$STATION_NUMBER=="33255", "Hamilton Island",bom$LOCATION)        #MMP
    bom$LOCATION<-ifelse(bom$STATION_NUMBER=="33294", "Yeppoon The Esplanade",bom$LOCATION)  #MMP
    bom$LOCATION<-ifelse(bom$STATION_NUMBER=="33295", "Alva Beach",bom$LOCATION)             #MMP
    bom$LOCATION<-ifelse(bom$STATION_NUMBER=="33317", "Hay Point",bom$LOCATION)              #MMP
    bom$LOCATION<-ifelse(bom$STATION_NUMBER=="39059", "Lady Elliot Island",bom$LOCATION)     #MMP
    bom$LOCATION<-ifelse(bom$STATION_NUMBER=="39122", "Heron Island Res Stn",bom$LOCATION)   #MMP
    bom$LOCATION<-ifelse(bom$STATION_NUMBER=="39304", "Heron Island",bom$LOCATION)           #MMP
    bom$LOCATION<-ifelse(bom$STATION_NUMBER=="39322", "Rundle Island",bom$LOCATION)          #MMP
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="1007", "Troughton Island",bom$LOCATION)      #Kimberleys
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="5007", "Learmonth Airport",bom$LOCATION)     #WA
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="5094", "Barrow Island Airport",bom$LOCATION) #WA
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="14015", "Darwin Airport",bom$LOCATION)       #Darwin
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="14072", "NA",bom$LOCATION)                   #??
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="14198", "Jabiru Airport",bom$LOCATION)       #Darwin
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="14274", "McCluer Island",bom$LOCATION)       #Darwin
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="14508", "Gove Airport",bom$LOCATION)         #NT
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="14412", "Gove Airport",bom$LOCATION)         #NT
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="14518", "Groote Eylandt Airport",bom$LOCATION)#NT
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="14948", "Port Keats Airport",bom$LOCATION)   #NT
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="27058", "Horn Island",bom$LOCATION)          #QLD
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="28008", "Lockhart River Airport",bom$LOCATION)#QLD
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="31209", "Cooktown Airport",bom$LOCATION)     #QLD
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="33119", "Mackay M.O",bom$LOCATION)           #QLD
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="33083", "Cardowan",bom$LOCATION)             #QLD
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="39123", "Gladstone Radar",bom$LOCATION)      #QLD
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="40068", "Double Island Point Lighthouse",bom$LOCATION)#QLD
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="200001", "Middle Percy Island",bom$LOCATION) #QLD
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="200713", "NA",bom$LOCATION)                  #QLD
    bom$LOCATION <- ifelse(bom$STATION_NUMBER=="200782", "NA",bom$LOCATION)   #??
    bom$LOCATION <- factor(bom$LOCATION)
    droplevels(bom)
}

## The following are a series of functions that should not be called  ##
## directly.  They relate to calculating properties of the tidal data ##
########################################################################
peaks<-function(series,span=3) 
{ 
    z <- embed(series, span) 
    s <- span%/%2 
    v<- max.col(z) == 1 + s 
    result <- c(rep(FALSE,s),v) 
    result <- c(result,NA)
    result
} 
troughs<-function(series,span=3) 
{ 
    series <- -1*series
    z <- embed(series, span) 
    s <- span%/%2 
    v<- max.col(z) == 1 + s 
    result <- c(rep(FALSE,s),v) 
    result <- c(result, NA)
    result
} 
highlowTide <- function(data) {
    data$high <- peaks(data$Height)
    data$low <- troughs(data$Height)
    subset(data, high==TRUE | low==TRUE)
}
tideRange <- function(data) {
    dt <- NULL
    for (i in 2:nrow(data)) {
        dt<-rbind(dt,
                  data.frame(DateTime=data$DateTime[i], 
                             Date=data$Date[i], 
                             Height=data$Height[i], 
                             range=data$Height[i-1]-data$Height[i], 
                             high=data$high[i]))
    }
    dt
}
maxTideRange <- function(data) {
    plyr:::ddply(data,~Date, function(df) {
        data.frame(Range=max(abs(df$Range)))
    })
}

########################################################################
## The following function is used to calculate the daily tidal range. ##
## This in turn might be useful as a proxy for tidal water movement.  ##
## Parameters:                                                        ##
##    tidelist:  a list - each item is tidal data for a specific      ##
##                  location                                          ##
## Returns:                                                           ##
##    list:      a list of daily tidal ranges                         ##
########################################################################
MMP_processTides <- function(tidelist) {
    ##The following function is written in C++     
    ##It replaces an R function that employs a necessary for loop (cannot be vectorized because it needs to compare across rows) 
    library(Rcpp)
    cppFunction(' 
  NumericVector tideRng(NumericVector x) {
    int n=x.size();
    NumericVector range(n);
    for (int i=1; i<n; i++) {
      range[i]=x[i-1]-x[i];
    }
    return range;
  }', showOutput=FALSE, rebuild=TRUE
                )
    tides.daily <- list()
    for (i in 1:length(tidelist)) {
        tides.daily[[i]] <- highlowTide(tidelist[[i]])
        tides.daily[[i]]$Range <- tideRng(tides.daily[[i]]$Height)
        tides.daily[[i]] <- maxTideRange(tides.daily[[i]])
        tides.daily[[i]]$Date <- as.Date(tides.daily[[i]]$Date)
    }
    names(tides.daily) <- names(tidelist)
    tides.daily
}


## The following function is used to generate individual river
## discharge figures that blend river dischage with median and
## long-term values
mmp__discharge_plot <- function(Discharge = ..1, Annual = ..2, Baseline = ..3) {
    Discharge %>%
        ggplot(aes(y = DISCHARGE_RATE_DAILY, x = Date)) +
        geom_rect(data = NULL,
                  aes(ymin=-Inf,
                      ymax=Inf,
                      xmin=MAXDATE-years(1)+days(1),
                      xmax=MAXDATE), fill='grey') +
        {if(!is.null(nrow(Baseline))) geom_hline(data = Baseline,
                   aes(yintercept = LTmedian/20),
                   linetype = 'dashed') } + 
        geom_point(data = Annual, aes(y = discharge.c.annual/20),
                   colour = '#D55E00') +
        geom_line(data = Annual, aes(y = discharge.c.annual/20),
                  colour = '#D55E00', alpha = 0.5) +
        geom_path(colour = '#56B4E9') +
        scale_y_continuous(str_wrap('Daily river discharge (ML x 10,000)', 25),
                           sec.axis = sec_axis(~.*20,
                                               name = str_wrap("Annual river discharge (GL x 10,000)", 25),
                                               label = function(x) x/10000),
                           label = function(x) x/10000) +
        theme_classic() +
        theme(axis.title.x = element_blank(),
              axis.line.y.left = element_line(colour = '#56b4e9'),
              axis.text.y.left = element_text(colour = '#56b4e9'),
              axis.title.y.left = element_text(colour = '#56b4e9',
                                               margin = margin(r = 2, unit = 'lines')),
              axis.ticks.y.left = element_line(colour = '#56b4e9'),
              axis.line.y.right = element_line(colour = '#D55E00'),
              axis.text.y.right = element_text(colour = '#D55E00'),
              axis.title.y.right = element_text(colour = '#D55E00',
                                                margin = margin(l = 2, unit = 'lines')),
              axis.ticks.y.right = element_line(colour = '#D55E00')
              )
}

mmp__boxplots <- function(data) {
    load(file=paste0(DATA_PATH, '/primary/other/wq.units.RData'))
    load(file=paste0(DATA_PATH, '/primary/other/wq.guidelines.RData'))

    dt <- data %>%
        left_join(wq.units %>% dplyr::select(Measure, Name.graphs)) %>%
        mutate(Site = str_replace_all(MMP_SITE_NAME, ' ', '~')) %>%
        mutate(Site = str_replace_all(Site, '\'', '*minute*')) 

    wq.guide <- dt %>%
        dplyr::select(Site, MMP_SITE_NAME, Measure, Name.graphs) %>%
        distinct() %>% 
        left_join(wq.guidelines %>%
                  dplyr::select(MMP_SITE_NAME, Measure, GL, DirectionOfFailure, GL.Season, Location) %>%
                  distinct()) %>%
        mutate(GL.Season = factor(GL.Season))

    dt %>%
        ggplot(aes(y = Value, x = Season)) +
        geom_boxplot(aes(colour = Season, fill = Season), width = 0.2, alpha = 0.3, show.legend = FALSE) +
        geom_point(stat = 'summary', fun = mean, aes(colour = Season, fill = Season), show.legend = FALSE) +
        geom_segment(data = wq.guide %>% filter(GL.Season %in% c('Dry','Wet')) %>% droplevels,
                     aes(y = GL, yend = GL,
                         x = as.numeric(GL.Season)-0.5,
                         xend = as.numeric(GL.Season) +0.5,
                         colour = GL.Season),
                     linetype = '21', size = 0.7, show.legend = FALSE) +
        geom_hline(data = wq.guide %>% filter(GL.Season == 'Annual') %>% droplevels,
                   aes(yintercept = GL), linetype = 'dashed') +
        facet_grid2(Site ~ Name.graphs, axes = "all", scales = 'free', independent = 'y',
                    labeller = label_parsed) +
        theme_classic() +
        theme(strip.background = element_blank(),
              axis.title = element_blank())
}

mmp__timeseries_plot <- function(flntu, tides, temperature, weather, discharge, GL.chl=NA, GL.ntu=NA, subtitle='') {
    flntu <- flntu %>%
        arrange(Date) %>%
        filter(Date>=START_DATE) %>%
        droplevels() %>%
        complete(Date = seq.Date(min(Date), max(Date), by = 'day')) 

    ## Define min and max dates
    minDate <- min(flntu$Date)
    ## maxDate <- max(flntu$Date)
    maxDate <- MAXDATE
    
    ##Construct base plot
    par(mar=c(4,6.5,0.5,6))
    plot(1:nrow(flntu)~Date, data = flntu,
         type="n",
         axes=F,
         ann=F,
         ylim=c(0,1),
         ## xlim=c(min(c(flntu$Date)), max(c(flntu$Date,flntu$Date))))
         xlim=c(min(c(flntu$Date)), MAXDATE))

    ##Construct yearly ribbon underlay
    ## tm2 <-seq(as.Date("2001-09-01"), max(flntu$Date), by="2 year")
    ## tm3 <-seq(as.Date("2002-08-30"), max(flntu$Date), by="2 year")
    tm2 <-seq(as.Date("2001-09-01"), MAXDATE, by="2 year")
    tm3 <-seq(as.Date("2002-08-30"), MAXDATE, by="2 year")
    tm2 <- tm2[tm2>as.Date("2007-08-01")]
    tm3 <- tm3[tm3>as.Date("2007-08-01")]
    ## if(length(tm2)>length(tm3)) tm3 <- c(tm3, max(c(flntu$Date,flntu$Date)))
    if(length(tm2)>length(tm3)) tm3 <- c(tm3, MAXDATE)
    rect(tm2,par()$usr[3],tm3,1, col="gray95", border=NA)

    ##Construct time axes
    if ((maxDate - minDate) < 365) {
        tm <- seq(as.Date("2000-01-01"), maxDate, by = '1 month')
    } else if ((maxDate - minDate) > (365*7)) {
        tm <- seq(as.Date("2000-01-01"), maxDate, by = '6 month')
    } else {
        tm <-seq(as.Date("2000-01-01"), maxDate, by="3 month")
    }
    tm1 <-seq(as.Date("2000-04-01"), maxDate, by="year")
    axis(1, at=tm, lab=strtrim(format(tm,"%b"),3), cex.axis=1, mgp=c(0,.5,0),tcl=-0.3)
    axis(1, at=tm1, lab=format(tm1,"%Y"), cex.axis=1.25, cex.lab=1.25,cex=1.25,tcl=0, mgp=c(0,2.5,0))

    ##Chlorophyll========================================================
    ch.dat <-  flntu
    ## tt<-data.frame(Date=seq(startDate,endDate,by="day"))
    ## ch.ddat<-merge(ch.dat, tt, by="Date", all=TRUE) 
    ## ch.ddat <- ch.ddat[order(ch.ddat$Date),]
    ch.ddat <- flntu
    maxy <- max(ch.ddat$CHL_QA_AVG, na.rm=TRUE)
    lines(scales::rescale(CHL_QA_AVG,from=c(0,maxy),to=c(0,0.3))~Date,ch.ddat, col="#008000")
    abline(h=scales::rescale(GL.chl, from=c(0,maxy),to=c(0,0.3)), col="#008000",lty=2)
    axis(2, las=1, at=scales::rescale(pretty(seq(0,maxy,l=100)), to=c(0,0.3)), 
         lab=pretty(seq(0,maxy,l=100)),
         cex.axis=1,
         tcl=-0.2,
         mgp=c(0,0.5,0),
         col="#008000",
         col.ticks="#008000",
         col.axis="#008000")
    mtext(expression(paste(atop("Chlorophyll-a",(mu*g~L^-1)))),
          2,
          col="#008000",
          line=2.5,
          at=scales::rescale(mean(maxy,0), to=c(0,0.3)),
          cex=1)

    ##Tides
    tides <- tides %>%
        filter(Date >= minDate, Date <= maxDate) %>%
        droplevels()
    ## tides <- subset(tides, Date >= startDate & Date <= endDate)
    maxy <- max(tides$Range, na.rm=TRUE)
    lines(scales::rescale(Range,from=c(0,maxy),to=c(0.50,0.70))~Date, tides, col="grey60")
    axis(4, las=1, at=scales::rescale(pretty(seq(0,maxy,l=100),n=3), to=c(0.50,0.70)), 
         lab=pretty(seq(0,maxy,l=100),n=3), cex.axis=1, tcl=-0.2, mgp=c(0,0.3,0))
    mtext(expression(paste(atop("Daily Tidal","Range (m)"))),4, line=4.5,
          at=scales::rescale(mean(maxy,0), to=c(0.5,0.7)), cex=1)
    
    ##Turbidity - log
    flntu <- flntu %>%
        mutate(logNTU = log(NTU_QA_AVG))
    maxy <- max(flntu$logNTU, na.rm=TRUE)
    maxy <- max(log(50))
    miny <- min(flntu$logNTU, na.rm=TRUE)
    lo <-0.1
    miny <- log(lo)
    lines(scales::rescale(logNTU,from=c(miny,maxy),to=c(0.33,0.6))~Date, flntu, col="#7B0000")
    abline(h=scales::rescale(log(GL.ntu+lo), from=c(miny,maxy),to=c(0.33,0.6)), col="#7B0000",lty=2)
    pret <- pretty(flntu$NTU_QA_AVG)+lo
    pret <- c(lo,1,2,5,10,20,50)
    axis(2, las=1, at=scales::rescale(log(pret+lo), from=c(miny,maxy),to=c(0.33,0.6)), 
         lab=pret, cex.axis=1, tcl=-0.2,
         mgp=c(0,0.5,0), col="#7B0000", col.ticks="#7B0000", col.axis="#7B0000")
    mtext(expression(paste(atop("Turbidity","(NTU)"))),
          2, col="#7B0000", line=2.5,
          at=scales::rescale(mean(maxy,0), to=c(0.33,0.65)),
          cex=1)

    ## Temperature
    ## if (!all(is.na(temperature$Temp))) {
    ##     maxy <- max(temperature$Temp, na.rm=TRUE)
    ##     miny <- min(temperature$Temp, na.rm=TRUE)
    ##     lines(scales::rescale(Temp,from=c(20,maxy),to=c(0.20,0.35))~Date, temperature, col="grey60")
    ##     axis(4, las=1, at=scales::rescale(pretty(seq(20,maxy,l=100),n=3), to=c(0.20,0.35)), 
    ##          lab=pretty(seq(20,maxy,l=100),n=3), cex.axis=1, tcl=-0.2, mgp=c(0,0.3,0))
    ##     mtext(expression(paste(atop("Daily Temp.",(degree~C)))),4, line=4.5, at=scales::rescale(mean(maxy,0), to=c(0.2,0.35)), cex=1)
    ## }

    ##Windspeed
    weather <- weather %>% mutate(Dt = as.numeric(Date))
    lx <-seq(min(weather$Dt, na.rm=TRUE), max(weather$Dt, na.rm=TRUE), l=100)
  ##   dd <- seq(startDate, endDate,by="day")
  ## weather <- merge(weather,data.frame(Date=dd), all=TRUE)
  ##   weather <- weather[order(weather$Date),]
    maxy <- max(weather$WIND_SPEED, na.rm=TRUE)
    lines(scales::rescale(WIND_SPEED,from=c(0,maxy),to=c(0.83,1))~Date, weather, col="grey60")
    library(mgcv)
    tryCatch({
        ll <- gam(WIND_SPEED~s(Dt), data=weather)
        ly <- predict(ll, newdata=data.frame(Dt=lx), type="response", se=TRUE)
        lx <- as.Date(lx, "1970-01-01")
        predvar <- ly$se.fit^2
        SE2<-sqrt(predvar+ll$sig2)
        lines(scales::rescale(ly$fit,from=c(0,maxy),to=c(0.83,1))~lx, col="gray30")
        lines(scales::rescale(ly$fit+1*SE2,from=c(0,maxy),to=c(0.83,1))~lx, col="gray30", lty=2)
        lines(scales::rescale(ly$fit-1*SE2,from=c(0,maxy),to=c(0.83,1))~lx, col="gray30", lty=2)
    },error=function(x) print("Not enough wind data"))
    axis(4, las=1, at=scales::rescale(pretty(seq(0,maxy,l=100),n=3), to=c(0.83,1)), 
         lab=pretty(seq(0,maxy,l=100),n=3), cex.axis=1, tcl=-0.2, mgp=c(0,0.5,0))
    mtext(expression(paste(atop("Wind speed",(km~h^-1)))),4, line=4.5, at=scales::rescale(mean(maxy,0), to=c(0.83,1)), cex=1) 

    ##Discharge
    if (nrow(discharge)>0) {
        maxy <- max(discharge$DISCHARGE_RATE_DAILY, na.rm=TRUE)
        lines(scales::rescale(DISCHARGE_RATE_DAILY,from=c(0,maxy),to=c(0.70,1))~Date, discharge, col="blue")
                                        #abline(h=scales::rescale(1.5, from=c(0,maxy),to=c(0.70,1)), col="blue",lty=2)
        axis(2, las=1, at=scales::rescale(pretty(seq(0,maxy,l=100)), to=c(0.70,1)), 
             lab=pretty(seq(0,maxy,l=100)/10000), cex.axis=1, tcl=-0.2, mgp=c(0,0.5,0), col="blue", col.ticks="blue", col.axis="blue")
        mtext(expression(paste(atop("River discharge",(ML~day^-1 %*% 10^4)))),2, col="blue", line=2.5, at=scales::rescale(mean(maxy,0), to=c(0.7,1)), cex=1)
    } else {
        axis(2, las=1, at=scales::rescale(pretty(seq(0,10000,l=100)), to=c(0.70,1)), 
             lab=pretty(seq(0,10000,l=100)/10000), cex.axis=1, tcl=-0.2, mgp=c(0,0.5,0), col="blue", col.ticks="blue", col.axis="blue")
        mtext(expression(paste(atop("River discharge",(ML~day^-1 %*% 10^4)))),2, col="blue", line=2.5, at=scales::rescale(mean(10000,0), to=c(0.7,1)), cex=1)
        
    }
  
    legend("topleft",legend=subtitle, bty="n", text.font=1, cex=1,inset=-0.03,xpd=TRUE)#,xjust=0,adj=c(0,1), cex=1.5)
}

mmp__timeseries_prepare_flntu <- function(flntu, Site, START_DATE) {
    flntu %>%
        filter(MMP_SITE_NAME == Site,
               Date >= START_DATE) %>%
        droplevels()
}
mmp__timeseries_prepare_waterTemp <- function(waterTemp, Site, START_DATE) {
    if (waterTemp %>% filter(MMP_SITE_NAME == Site) %>% nrow() >0) { 
        waterTemp <- waterTemp %>% filter(MMP_SITE_NAME == Site) %>% droplevels() %>%
            filter(!is.na(Date),
                   Date >= START_DATE) %>%
            droplevels() %>% 
            arrange(Date) %>%
            complete(Date = seq.Date(min(Date), max(Date), by = 'week'))
    } else {
        waterTemp <- NULL
    }
    waterTemp
}
mmp__timeseries_prepare_wind <- function(wind, Site, START_DATE) { 
    lookup <- read.csv(paste0(PARAMS_PATH, '/lookup.csv'), strip.white = TRUE) %>% suppressMessages()
    wind %>%
        left_join(lookup %>% filter(reef.alias == Site) %>%
                  dplyr::select(reef.alias, BOM),
                  by = c('LOCATION' = 'BOM')) %>%
        mutate(MMP_SITE_NAME = reef.alias) %>% 
        filter(MMP_SITE_NAME == Site) %>% droplevels() %>%
        filter(!is.na(Date),
               Date >= START_DATE) %>%
        arrange(Date) %>%
        complete(Date = seq.Date(min(Date), max(Date), by = 'week'))
}
mmp__timeseries_prepare_discharge <- function(discharge, Sub_region, START_DATE) {
    discharge %>%
        filter(Subregion == Sub_region) %>%
        droplevels() %>%
        ## filter(Date >= as.Date('2006-01-01')) %>%
        filter(Date >= START_DATE) %>%
        group_by(Subregion, Date) %>%
        summarise(DISCHARGE_RATE_DAILY = sum(PARAM_VALUE)) %>%
        suppressMessages() %>%
        suppressWarnings()
}
mmp__timeseries_prepare_guidelines <- function(Site) {
    load(file=paste0(DATA_PATH, '/primary/other/wq.guidelines.RData'))
    GL.chl <- wq.guidelines %>%
        filter(MMP_SITE_NAME==Site, GL.Season=="Annual", Measure=="DRIFTCHL_UGPERL.wm") %>%
        pull(GL) %>%
        unique()
    GL.chl <- ifelse(length(GL.chl>0), as.vector(GL.chl),NA)
    GL.ntu <- wq.guidelines %>%
        filter(MMP_SITE_NAME==Site, GL.Season=="Annual", Measure=="NTU") %>%
        pull(GL) %>%
        unique()
    GL.ntu <- ifelse(length(GL.ntu>0), as.vector(GL.ntu),NA)
    list(GL.chl = GL.chl, GL.ntu = GL.ntu)
}
mmp__timeseries_prepare_data <- function(flntu, tides, waterTemp, wind, discharge, Site, START_DATE) {
    ## FLNTU
    flntu <- mmp__timeseries_prepare_flntu(flntu, Site, START_DATE)
    if (nrow(flntu)<1) return(NULL)
    ## Tides
    tides <- tides[[Site]]
    ## Water temperature
    waterTemp <- mmp__timeseries_prepare_waterTemp(waterTemp, Site, START_DATE)
    ## Wind
    wind <- mmp__timeseries_prepare_wind(wind, Site, START_DATE) 
    ## Discharge
    Subregion <- flntu %>% pull(Subregion) %>% unique() %>% na.omit() %>% as.character()
    discharge <- mmp__timeseries_prepare_discharge(discharge, Sub_region = Subregion, START_DATE)
    ## Guideline values
    GL <- mmp__timeseries_prepare_guidelines(Site)
    
    return(list(flntu = flntu, tides = tides,
                waterTemp = waterTemp, wind = wind,
                discharge = discharge,
                GL.chl = GL$GL.chl,
                GL.ntu = GL$GL.ntu))
}
