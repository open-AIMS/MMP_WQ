rollAv <- function(Val, Year, span.yrs=4,location='Mean') {
    nc = length(unique(Year))
    Xmat=matrix(unlist(lapply(unique(Year), function(x) {
        ifelse(ceiling(((Year) - (x))/span.yrs) %in% c(0),1,NA)#;
        #aa/length(aa[aa==1 & !is.na(Val)]);
    })),ncol=nc)
    if (location=='Mean') apply(Val * Xmat, 2, mean,na.rm=TRUE)
    else apply(Val * Xmat, 2, median,na.rm=TRUE)
}

MMP_WQI_lastYear<- function(dat) {
    Benchmark=dat$GL
    e <- (log(dat$Value,2) - log(Benchmark,2))*ifelse(dat$DirectionOfFailure=='H',-1,1)
    e[e>1]<-1
    e[e< -1] <- -1
    e
}


theme_mmp <- ggplot2::theme_classic(10) +
    ggplot2::theme(axis.line.x = ggplot2::element_line(),
                   axis.line.y = ggplot2::element_line())
theme_mmp_qaqc <- ggplot2::theme_gray(12) +
    ggplot2::theme(text = ggplot2::element_text(size=8),
                   panel.border=ggplot2::element_rect(fill=NA, size=0.5),
                   panel.spacing=unit(1,'pt'),
                   axis.text.x=ggplot2::element_text(angle=45, hjust=1),
                   strip.background=ggplot2::element_rect(fill=NA,color=NA,size=0.5),
                   strip.text.y=element_text(angle=0))

trafficLightPalette <- (c('#FF0000','#FFC000','#FFFF00','#92D050','#00B050'))

QAQC_labeller <- function(labels,multi_line=TRUE) {
    if (names(labels)=='Subregion') {
        labels[[1]]=gsub('~','\n', labels[[1]])
        r <- list(as.character(labels[[1]]))
    } else {
        r <- label_parsed(labels,multi_line=multi_line)
    } 
    ## browser()
    return(r)
}
class(QAQC_labeller) <- "labeller"

MMP_generateOldGrades <- function(x) {
    ifelse(is.na(x),'NA',ifelse(x>=0.5, 'A', ifelse(x>=0, 'B', ifelse(x>=-1/3, 'C',  ifelse(x>=0-2/3, 'D', 'E')))))
}

MMP_generateGrades <- function(x) {
    ifelse(is.na(x),'NA',ifelse(x>=0.5 + (2/3*0.5), 'A', ifelse(x>=0.5+1/3*0.5, 'B', ifelse(x>=0.5, 'C',  ifelse(x>=0.25, 'D', 'E')))))
}
