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

mmp__qaqc <- function(wq.qaqc, level = 1, type = 1, ...) {
    switch(level,
           '1' = mmp__qaqc_1(wq.qaqc, type, ...),
           '2' = mmp__qaqc_2(wq.qaqc, type, ...)
           )
}

mmp__qaqc_1 <- function(wq.qaqc, type, ...) {
    list2env(list2(...))
    wq.qaqc <-
        wq.qaqc %>%
        filter(Measure %in% c('DRIFTCHL_UGPERL.wm','TSS_MGPERL.wm',
                              'SECCHI_DEPTH.wm','PP.wm','PN.wm','NOx.wm', 'NTU')) %>%
        ungroup() %>%
        mutate(Subregion=gsub(' ','~',Subregion),
               Subregion=factor(Subregion, levels=unique(Subregion))) %>%
        {if(type %in% c('1','2','3','4'))
             filter(., waterYear == reportYear)
         else if (type %in% c('5'))
             filter(., reneeYear == reportYear)
         else
             .
        } %>% 
        left_join(wq.units %>%
                  dplyr:::select(Measure,Name.graphs.abbr)) %>%
        left_join(wq.sites %>%
                  dplyr:::select(MMP_SITE_NAME, Latitude)) %>% 
        arrange(Latitude) %>%
        mutate(Subregion=factor(Subregion, levels=unique(Subregion)),
               MMP_SITE_NAME=factor(MMP_SITE_NAME, levels=unique(MMP_SITE_NAME))) %>%
        suppressMessages() %>%
        suppressWarnings()

    if(type %in% c('1','2','3','4')) {
        GL <- wq.qaqc %>%
            dplyr::select(Name.graphs.abbr,Subregion,MMP_SITE_NAME,GL) %>%
            distinct() %>%
            mutate(lower=GL/2, upper=GL*2,lower1=GL/4, upper1=GL*4)
    } else if (type %in% c('5')) {
        GL <- wq.qaqc %>%
            dplyr:::select(Name.graphs.abbr,Subregion,MMP_SITE_NAME,GL,GL.Season) %>%
            distinct() %>%
            mutate(lower=GL/2, upper=GL*2,lower1=GL/4, upper1=GL*4,Season=GL.Season)
    } else {
    } 

    switch(type,
           "1" = mmp__qaqc_1_1(wq.qaqc, GL),
           "2" = mmp__qaqc_1_1(wq.qaqc, GL),
           "3" = mmp__qaqc_1_1(wq.qaqc, GL),
           "4" = mmp__qaqc_1_4(wq.qaqc, GL),
           "5" = mmp__qaqc_1_5(wq.qaqc, GL)
           )
}

mmp__qaqc_1_1 <- function(wq.qaqc, GL) {
    ggplot(wq.qaqc, aes(x=Value,y=MMP_SITE_NAME)) +
        geom_blank(aes(color=Season)) +
        geom_segment(dat=GL, aes(x=lower1,xend=upper1,
                                 y=MMP_SITE_NAME, yend=MMP_SITE_NAME),
                     size=5, color='purple', alpha=0.1) +
        geom_segment(dat=GL, aes(x=lower,xend=upper,
                                 y=MMP_SITE_NAME, yend=MMP_SITE_NAME),
                     size=5, color='purple', alpha=0.3) +
        geom_text(aes(x=GL), label='I', size=5,color='purple') +
        geom_point(data=wq.qaqc %>%
                       filter(Source=='AIMS Niskin') %>%
                       droplevels(),
                   aes(color=Season, shape=Source, size=Source),
                   position=position_jitter(height=0.0, width=0),
                   show.legend=FALSE) +
        geom_point(data = wq.qaqc %>%
                       filter(Source=='AIMS FLNTU') %>%
                       droplevels(),
                   aes(color=Season,shape=Source,size=Source),
                   position=position_jitter(height=0.4, width=0),
                   show.legend=FALSE) +
        facet_grid(Subregion~Name.graphs.abbr, scales='free', space='free_y',
                   labeller = QAQC_labeller,as.table=FALSE) +
        scale_x_log10('Depth weighted averages',breaks=c(0.1,0.5,1,5,10,50,100)) +
        scale_y_discrete('') +
        scale_fill_manual('Season', values=c('red','blue')) +
        scale_color_manual('Season', values=c('red','blue')) +
        scale_shape_manual('Source', values=c(16,16))  +
        scale_size_manual('Source', values=c(0.1,1)) +
        theme_mmp_qaqc 
}

mmp__qaqc_1_4 <- function(wq.qaqc, GL) {
    ggplot(wq.qaqc,aes(x=Value,y=MMP_SITE_NAME)) +
        geom_blank(aes(color=Season)) +
        geom_segment(dat=GL,aes(x=lower1,xend=upper1,y=MMP_SITE_NAME,
                                yend=MMP_SITE_NAME), size=5, color='purple', alpha=0.1) +
        geom_segment(dat=GL,aes(x=lower,xend=upper,y=MMP_SITE_NAME,
                                yend=MMP_SITE_NAME), size=5, color='purple', alpha=0.3) +
        geom_text(aes(x=GL), label='I', size=5,color='purple') +
        geom_point(data=wq.qaqc %>%
                       filter(Source %in% c('CY Niskin','AIMS Niskin','JCU Niskin')) %>%
                       droplevels(),
                   aes(color=Season,shape=Source,size=Source),
                   position=position_jitter(height=0.0, width=0), show.legend=FALSE) +
        geom_point(data=wq.qaqc %>%
                       filter(Source=='AIMS FLNTU') %>%
                       droplevels(),
                   aes(color=Season,shape=Source,size=Source),
                   position=position_jitter(height=0.4, width=0), show.legend=FALSE) +
        facet_grid(Subregion~Name.graphs.abbr, scales='free',
                   space='free_y', labeller = QAQC_labeller,as.table=FALSE) +
        scale_x_log10('Depth weighted averages', breaks=c(0.1,0.5,1,5,10,50,100)) +
        scale_y_discrete('') +
        scale_fill_manual('Season', values=c('red','blue')) +
        scale_color_manual('Season', values=c('red','blue')) +
        scale_shape_manual('Source', values=c(16,16,17,17)) +
        scale_size_manual('Source', values=c(0.1,1,1,1)) +
        theme_mmp_qaqc +
        theme(strip.text.x=element_text(size=rel(0.9)),
              axis.text.x=element_text(size=rel(0.8),angle=90, hjust=1,vjust=0.5))
}

mmp__qaqc_1_5 <- function(wq.qaqc, GL) {
    ggplot(wq.qaqc,aes(x=Value,y=MMP_SITE_NAME)) +
        geom_blank(aes(color=Season)) +
        geom_segment(dat=GL,aes(x=lower1,xend=upper1,
                                y=MMP_SITE_NAME, yend=MMP_SITE_NAME,color=Season),
                     size=5, alpha=0.1,show.legend=FALSE) +
        geom_segment(dat=GL,aes(x=lower,xend=upper,
                                y=MMP_SITE_NAME, yend=MMP_SITE_NAME,color=Season),
                     size=5, alpha=0.3,show.legend=FALSE) +
        geom_text(aes(x=GL,color=Season), label='I', size=5,show.legend=FALSE) +
        geom_point(data=wq.qaqc %>%
                       filter(Source %in% c('CY Niskin','AIMS Niskin','JCU Niskin')) %>%
                       droplevels(),
                   aes(color=Season,shape=Source,size=Source),
                   position=position_jitter(height=0.0, width=0), show.legend=FALSE) +
        geom_point(data=wq.qaqc %>%
                       filter(Source=='AIMS FLNTU') %>%
                       droplevels(),
                   aes(color=Season,shape=Source,size=Source),
                   position=position_jitter(height=0.4, width=0),
                   show.legend=FALSE) +
        facet_grid(Subregion~Name.graphs.abbr, scales='free',
                   space='free_y', labeller = QAQC_labeller,as.table=FALSE) +
        scale_x_continuous('Depth weighted averages',
                           trans=scales::pseudo_log_trans(sigma=0.1, base=exp(10)),
                           breaks=c(0.1,0.5,1,5,10,50,100)) +
        scale_fill_manual('Season', values=c('purple','red','blue')) +
        scale_color_manual('Season', values=c('purple','red','blue')) +
        scale_shape_manual('Source', values=c(16,16,17,17)) +
        scale_size_manual('Source', values=c(0.1,0.8,0.8,0.8)) +
        theme_mmp_qaqc +
        theme(strip.text.x=element_text(size=rel(0.9)),
              axis.text.x=element_text(size=rel(0.8),angle=90, hjust=1,vjust=0.5))             
}
