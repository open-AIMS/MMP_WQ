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

## level refers to the qaqc level
## type refers to the index type
mmp__qaqc <- function(wq.qaqc, level = 1, type = 1, ...) {
    switch(level,
           '1' = mmp__qaqc_1(wq.qaqc, type, ...),
           '2' = mmp__qaqc_2(wq.qaqc, type, ...),
           '3' = mmp__qaqc_3(wq.qaqc, type, ...)
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
        {if(type %in% c('0','1','2','3','4'))
             filter(., waterYear == reportYear)
         else if (type %in% c('5','6'))
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

    if(type %in% c('0','1','2','3','4')) {
        GL <- wq.qaqc %>%
            dplyr::select(Name.graphs.abbr,Subregion,MMP_SITE_NAME,GL) %>%
            distinct() %>%
            mutate(lower=GL/2, upper=GL*2,lower1=GL/4, upper1=GL*4)
    } else if (type %in% c('5','6')) {
        GL <- wq.qaqc %>%
            dplyr:::select(Name.graphs.abbr,Subregion,MMP_SITE_NAME,GL,GL.Season) %>%
            distinct() %>%
            mutate(lower=GL/2, upper=GL*2,lower1=GL/4, upper1=GL*4,Season=GL.Season)
    } else {
    } 

    switch(type,
           "0" = mmp__qaqc_1_1(wq.qaqc, GL),
           "1" = mmp__qaqc_1_1(wq.qaqc, GL),
           "2" = mmp__qaqc_1_1(wq.qaqc, GL),
           "3" = mmp__qaqc_1_1(wq.qaqc, GL),
           "4" = mmp__qaqc_1_4(wq.qaqc, GL),
           "5" = mmp__qaqc_1_5(wq.qaqc, GL),
           "6" = mmp__qaqc_1_5(wq.qaqc, GL)
           )
}

mmp__qaqc_2 <- function(wq.qaqc, type, ...) {
    list2env(list2(...))
    wq.qaqc <-
        wq.qaqc %>%
        filter(Measure %in% c('DRIFTCHL_UGPERL.wm','TSS_MGPERL.wm',
                              'SECCHI_DEPTH.wm','PP.wm','PN.wm','NOx.wm', 'NTU')) %>%
        ungroup() %>%
        mutate(Subregion=gsub(' ','~',Subregion),
               Subregion=factor(Subregion, levels=unique(Subregion))) %>%
        {if(type %in% c('0','1','2'))
             filter(., Year == reportYear)
         else if (type %in% c('3','4'))
             filter(., waterYear == reportYear)
         else if (type %in% c('5','6'))
             filter(., reneeYear == reportYear)
         else
             .
        } %>% 
        left_join(wq.units %>%
                  dplyr:::select(Measure,Name.graphs.abbr)) %>%
        left_join(wq.sites %>%
                  dplyr:::select(MMP_SITE_NAME, Latitude)) %>% 
        {if(type %in% c('0','1','2','3','4'))
             mutate(., Season='Annual')
         else if (type %in% c('5','6'))
             mutate(., Season=GL.Season)
         else
             .
        } %>%
        arrange(Latitude) %>%
        mutate(Subregion=factor(Subregion, levels=unique(Subregion)),
               MMP_SITE_NAME=factor(MMP_SITE_NAME, levels=unique(MMP_SITE_NAME))) %>%
        suppressMessages() %>%
        suppressWarnings()

    if(type %in% c('0','1','2','3','4')) {
        GL <- wq.qaqc %>%
            dplyr::select(Name.graphs.abbr,Subregion,MMP_SITE_NAME,GL) %>%
            distinct() %>%
            mutate(lower=GL/2, upper=GL*2,lower1=GL/4, upper1=GL*4)
    } else if (type %in% c('5','6')) {
        GL <- wq.qaqc %>%
            dplyr:::select(Name.graphs.abbr,Subregion,MMP_SITE_NAME,GL,GL.Season) %>%
            distinct() %>%
            mutate(lower=GL/2, upper=GL*2,lower1=GL/4, upper1=GL*4,Season=GL.Season)
    } else {
    } 

    switch(type,
           "0" = mmp__qaqc_2_1(wq.qaqc, GL),
           "1" = mmp__qaqc_2_1(wq.qaqc, GL),
           "2" = mmp__qaqc_2_1(wq.qaqc, GL),
           "3" = mmp__qaqc_2_1(wq.qaqc, GL),
           "4" = mmp__qaqc_2_4(wq.qaqc, GL),
           ## "5" = mmp__qaqc_2_4(wq.qaqc, GL),
           "5" = mmp__qaqc_2_5(wq.qaqc, GL),
           "6" = mmp__qaqc_2_5(wq.qaqc, GL)
           )
}

mmp__qaqc_3 <- function(wq.qaqc, type, ...) {
    list2env(list2(...))
    wq.qaqc <-
        wq.qaqc %>%
        {if(type %in% c('0','2','3','4','5','6'))
             filter(., Measure %in% c('DRIFTCHL_UGPERL.wm','TSS_MGPERL.wm',
                                      'SECCHI_DEPTH.wm','PP.wm','PN.wm','NOx.wm', 'NTU')) 
         else if (type %in% c('1'))
             filter(., Measure %in% c('DRIFTCHL_UGPERL.wm','TSS_MGPERL.wm',
                                      'SECCHI_DEPTH.wm','PP.wm','PN.wm','NOx.wm')) 
         else
             .
        } %>% 
        ungroup() %>%
        mutate(Subregion=gsub(' ','~',Subregion),
               Subregion=factor(Subregion, levels=unique(Subregion))) %>%
        {if(type %in% c('0','1','2'))
             filter(., Year == reportYear)
         else if (type %in% c('3','4'))
             filter(., waterYear == reportYear)
         else if (type %in% c('5','6'))
             filter(., reneeYear == reportYear)
         else
             .
        } %>% 
        left_join(wq.units %>%
                  dplyr:::select(Measure,Name.graphs.abbr)) %>%
        left_join(wq.sites %>%
                  dplyr:::select(MMP_SITE_NAME, Latitude)) %>% 
        mutate(Season='Annual') %>%
        arrange(Latitude) %>%
        mutate(Subregion=factor(Subregion, levels=unique(Subregion)),
               MMP_SITE_NAME=factor(MMP_SITE_NAME, levels=unique(MMP_SITE_NAME))) %>%
        {if(type %in% c('6'))
             mutate(., Index = scales::rescale(Score, to = c(-1,1), from = c(0,1)))
         else
             .
        } %>%
        suppressMessages() %>%
        suppressWarnings()

    if(type %in% c('0','1','2','3')) {
        GL <- wq.qaqc %>%
            dplyr::select(Name.graphs.abbr,Subregion,MMP_SITE_NAME,GL) %>%
            distinct() %>%
            mutate(lower=GL/2, upper=GL*2,lower1=GL/4, upper1=GL*4)
    } else if (type %in% c('4','5','6')) {
        GL <- NULL
    } else if (type %in% c('8')) {
        GL <- wq.qaqc %>%
            dplyr:::select(Name.graphs.abbr,Subregion,MMP_SITE_NAME,GL,GL.Season) %>%
            distinct() %>%
            mutate(lower=GL/2, upper=GL*2,lower1=GL/4, upper1=GL*4,Season=GL.Season)
    } else {
    } 

    switch(type,
           "0" = mmp__qaqc_3_1(wq.qaqc, GL),
           "1" = mmp__qaqc_3_1(wq.qaqc, GL),
           "2" = mmp__qaqc_3_1(wq.qaqc, GL),
           "3" = mmp__qaqc_3_1(wq.qaqc, GL),
           "4" = mmp__qaqc_3_1(wq.qaqc, GL),
           "5" = mmp__qaqc_3_1(wq.qaqc, GL),
           "6" = mmp__qaqc_3_1(wq.qaqc, GL)
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

mmp__qaqc_2_1 <- function(wq.qaqc, GL) {
    ggplot(wq.qaqc,aes(x=Value,y=MMP_SITE_NAME)) +
        geom_blank(aes(color=Season)) +
        geom_segment(dat=GL,aes(x=lower1,xend=upper1,
                                y=MMP_SITE_NAME, yend=MMP_SITE_NAME),
                     size=5, color='purple', alpha=0.1) +
        geom_segment(dat=GL,aes(x=lower,xend=upper,y=MMP_SITE_NAME,
                                yend=MMP_SITE_NAME),
                     size=5, color='purple', alpha=0.3) +
        geom_text(aes(x=GL), label='I', size=5,color='purple') +
        geom_point(aes(fill=Season,color=Season),shape=21,show.legend = FALSE,size=2) +
        facet_grid(Subregion~Name.graphs.abbr, scales='free', space='free_y',
                   labeller = QAQC_labeller,as.table=FALSE) +
        scale_x_log10('Four year running means', breaks=c(0.1,0.5,1,5,10,50,100)) +
        scale_y_discrete('') +
        scale_fill_manual('Season', values=c('white','blue'),guide=FALSE) +
        scale_color_manual('Season', values=c('black','blue'),guide=FALSE) +
        theme_mmp_qaqc
}

mmp__qaqc_2_4 <- function(wq.qaqc, GL) {
    ggplot(wq.qaqc, aes(x=Value,y=MMP_SITE_NAME)) +
        geom_blank() +
        geom_segment(dat=GL,aes(x=lower1,xend=upper1,y=MMP_SITE_NAME, yend=MMP_SITE_NAME),
                     size=5, color='purple', alpha=0.1) +
        geom_segment(dat=GL,aes(x=lower,xend=upper,y=MMP_SITE_NAME, yend=MMP_SITE_NAME),
                     size=5, color='purple', alpha=0.3) +
        geom_text(aes(x=GL), label='I', size=5,color='purple') +
        geom_point(aes(shape=Source),fill='white', show.legend=FALSE,size=1) +
        facet_grid(Subregion~Name.graphs.abbr, scales='free', space='free_y',
                   labeller = QAQC_labeller,as.table=FALSE) +
        scale_x_log10('Annual means', breaks=c(0.1,0.5,1,5,10,50,100)) +
        scale_y_discrete('') +
        scale_fill_manual('Season', values=c('white','blue'),guide=FALSE) +
        scale_color_manual('Season', values=c('black','blue'),guide=FALSE) +
                                        #scale_shape_manual('Source', values=c(21,21,24)) + #if no CY
        scale_shape_manual('Source', values=c(21,21,24,25)) +
        theme_mmp_qaqc +
        theme(strip.text.x=element_text(size=rel(0.9)),
              axis.text.x=element_text(size=rel(0.8),angle=90, hjust=1,vjust=0.5))  
}
mmp__qaqc_2_5 <- function(wq.qaqc, GL) {
    ggplot(wq.qaqc, aes(x=Value,y=MMP_SITE_NAME)) +
        geom_blank() +
        geom_segment(dat=GL,aes(x=lower1,xend=upper1,y=MMP_SITE_NAME, yend=MMP_SITE_NAME,
                                color=Season),
                     size=5, alpha=0.1,show.legend=FALSE) +
        geom_segment(dat=GL,aes(x=lower,xend=upper,y=MMP_SITE_NAME, yend=MMP_SITE_NAME,
                                color=Season),
                     size=5, alpha=0.3,show.legend=FALSE) +
        geom_text(aes(x=GL,color=Season), label='I', size=5,show.legend=FALSE) +
        geom_point(aes(color=Season), shape=16,show.legend=FALSE,size=1) +
        facet_grid(Subregion~Name.graphs.abbr, scales='free', space='free_y',
                   labeller = QAQC_labeller,as.table=FALSE) +
        scale_x_continuous('Annual means',
                           trans=scales::pseudo_log_trans(sigma=0.1, base=exp(10)),
                           breaks=c(0.1,0.5,1,5,10,50,100)) +
        scale_fill_manual('Season', values=c('purple','red','blue')) +
        scale_color_manual('Season', values=c('purple','red','blue')) +
        theme_mmp_qaqc +
        theme(strip.text.x=element_text(size=rel(0.9)),
              axis.text.x=element_text(size=rel(0.8),angle=90, hjust=1,vjust=0.5))            
}

mmp__qaqc_3_1 <- function(wq.qaqc, GL) {
    ggplot(wq.qaqc,aes(x=Index,y=MMP_SITE_NAME)) +
        geom_blank(aes(color=Season)) +
        annotate(geom='rect',xmin=-1,xmax=-2/3,ymin=-Inf, ymax=Inf,
                 fill=trafficLightPalette[1],color=NA)+
        annotate(geom='rect',xmin=-2/3,xmax=-1/3,ymin=-Inf, ymax=Inf,
                 fill=trafficLightPalette[2],color=NA)+
        annotate(geom='rect',xmin=-1/3,xmax=0,ymin=-Inf, ymax=Inf,
                 fill=trafficLightPalette[3],color=NA)+
        annotate(geom='rect',xmin=0,xmax=0.5,ymin=-Inf, ymax=Inf,
                 fill=trafficLightPalette[4],color=NA)+
        annotate(geom='rect',xmin=0.5,xmax=1,ymin=-Inf, ymax=Inf,
                 fill=trafficLightPalette[5],color=NA)+
        geom_point(aes(fill=Season,color=Season),shape=21,show.legend=FALSE, size=2) +
        facet_grid(Subregion~Name.graphs.abbr, scales='free', space='free_y',
                   labeller = QAQC_labeller,as.table = FALSE) +
        scale_x_continuous('Index (Modified Amplitude)', expand=c(0.07,0)) +
        scale_y_discrete('') +
        scale_fill_manual('Season', values=c('white'), guide=FALSE) +
        scale_color_manual('Season', values=c('black'),guide=FALSE) +
        theme_mmp_qaqc
}

## level refers to the spatial level (region, subregion etc)
## type refers to the index type
mmp__indicator_trends <- function(wq.idx, level = 1, type = 1, ...) {
    switch(level,
           '1' = mmp__indicator_trends_1(wq.idx, type, ...),
           '2' = mmp__indicator_trends_2(wq.idx, type, ...),
           '3' = mmp__indicator_trends_3_6(wq.idx, ...),   # must be type = 6
           '4' = mmp__indicator_trends_4_6(wq.idx, ...),   # must be type = 6
           '5' = mmp__indicator_trends_5_6(wq.idx, ...),   # must be type = 6
           '6' = mmp__indicator_trends_6_6(wq.idx, ...),   # must be type = 6
           '7' = mmp__indicator_trends_7_6(wq.idx, ...)    # must be type = 6
           )
}

## Regional
mmp__indicator_trends_1 <- function(wq.idx, type = 1, ...) {
    switch(type,
           '0' = mmp__indicator_trends_1_1(wq.idx, ...),
           '1' = mmp__indicator_trends_1_1(wq.idx, ...),
           '2' = mmp__indicator_trends_1_1(wq.idx, ...),
           '3' = mmp__indicator_trends_1_4(wq.idx, ...),
           '4' = mmp__indicator_trends_1_4(wq.idx, ...),
           '5' = mmp__indicator_trends_1_4(wq.idx, ...),
           '6' = mmp__indicator_trends_1_6(wq.idx, ...)
           )
}

## Regional, type 0
mmp__indicator_trends_1_1 <- function(wq.idx, ...) {
    list2env(list2(...))
    wq.idx <- wq.idx %>%
        mutate(across(reportCardYear, ~ year(reportCardYear), .names = "Year")) %>%
        ungroup() %>%
        left_join(wq.sites %>%
                  dplyr::select(Region, Latitude, Region) %>%
                  group_by(Region) %>%
                  summarize_all(funs(mean(., na.rm=TRUE)))) %>% 
        arrange(desc(Latitude)) %>%
        mutate(Region=factor(Region, levels=unique(Region))) %>%
        mutate(Grade=MMP_generateOldGrades(Index))
    
    ggplot(wq.idx %>%
           filter(Year>2007),
           aes(y=Index, x=reportCardYear)) +
        geom_hline(yintercept=0, linetype='dashed') +
        geom_line() +
        geom_point(aes(fill=Grade),shape=21, size=3, show.legend = TRUE) +
        scale_y_continuous('Water Quality Index',limits=c(-1,1)) +
        scale_x_date('', limits=c(as.Date('2006-01-01'),
                                  as.Date(paste0(reportYear,'-01-01')))) +
        scale_fill_manual('',breaks=c('A','B','C','D','E'),
                          values=rev(trafficLightPalette),
                          limits=c('A','B','C','D','E'),
                          labels=c('Very Good','Good','Moderate','Poor','Very Poor')) +
        theme_mmp +
        theme(strip.background=element_blank(),
              panel.margin.x=unit(1,'line'))
}

## Regional, type 4
mmp__indicator_trends_1_4 <- function(wq.idx, ...) {
    list2env(list2(...))
    wq.idx <- wq.idx %>%
        mutate(across(reportCardYear, ~ year(reportCardYear), .names = "Year")) %>%
        ungroup() %>%
        left_join(wq.sites %>%
                  dplyr::select(Region, Latitude, Region) %>%
                  group_by(Region) %>%
                  summarize_all(funs(mean(., na.rm=TRUE)))) %>% 
        arrange(desc(Latitude)) %>%
        mutate(Region=factor(Region, levels=unique(Region))) %>%
        mutate(Grade=MMP_generateOldGrades(Index))
    
    ggplot(wq.idx, ##%>%
           ## filter(Year>2007),
           aes(y=Index, x=reportCardYear)) +
        geom_hline(yintercept=0, linetype='dashed') +
        geom_line() +
        geom_point(aes(fill=Grade),shape=21, size=3, show.legend = TRUE) +
        scale_y_continuous('Water Quality Index',limits=c(-1,1)) +
        scale_x_date('', limits=c(as.Date('2006-01-01'),
                                  as.Date(paste0(reportYear,'-01-01')))) +
        scale_fill_manual('',breaks=c('A','B','C','D','E'),
                          values=rev(trafficLightPalette),
                          limits=c('A','B','C','D','E'),
                          labels=c('Very Good','Good','Moderate','Poor','Very Poor')) +
        theme_mmp +
        theme(strip.background=element_blank(),
              panel.margin.x=unit(1,'line'))
}

## Year/site/measure level
mmp__indicator_trends_1_6 <- function(wq.idx, ...) {
    list2env(list2(...))
    wq.idx <- wq.idx %>%
        ungroup() %>% 
        left_join(wq.sites %>%
                  dplyr::select(Region, Latitude, Region) %>%
                  group_by(Region) %>%
                  summarize_all(funs(mean(., na.rm=TRUE)))) %>% 
        arrange(desc(Latitude)) %>%
        mutate(Region = factor(Region, levels = unique(Region)),
               Subregion = factor(Subregion, levels = unique(Subregion)),
               MMP_SITE_NAME = factor(MMP_SITE_NAME, levels = unique(MMP_SITE_NAME)))%>%
        mutate(Index = scales::rescale(Score, to = c(-1,1), from = c(0,1))) %>%
        mutate(reportCardYear = as.Date(paste0(reneeYear,'-01-01'))) %>%
        mutate(Grade = MMP_generateOldGrades(Index))

    ggplot(wq.idx, aes(y = Index, x = reportCardYear, group = MMP_SITE_NAME)) +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        geom_line() +
        geom_point(aes(fill = Grade), shape = 21, size = 3, show.legend = FALSE) +
        scale_y_continuous('Water Quality Index', limits = c(-1,1)) +
        scale_x_date('', limits = c(as.Date('2006-01-01'),
                                    as.Date(paste0(reportYear,'-01-01')))) +
        scale_fill_manual('',breaks = c('A','B','C','D','E'),
                          values = rev(trafficLightPalette),
                          limits = c('A','B','C','D','E'),
                          labels = c('Very Good','Good','Moderate','Poor','Very Poor')) +
        theme_mmp +
        theme(strip.background = element_blank(),
              panel.margin.x = unit(1,'line'),
              panel.border = element_rect(fill = NA))
}


## Subregional
mmp__indicator_trends_2 <- function(wq.idx, type = 1, ...) {
    switch(type,
           '0' = mmp__indicator_trends_2_1(wq.idx, ...),
           '1' = mmp__indicator_trends_2_1(wq.idx, ...),
           '2' = mmp__indicator_trends_2_1(wq.idx, ...),
           '3' = mmp__indicator_trends_2_4(wq.idx, ...),
           '4' = mmp__indicator_trends_2_4(wq.idx, ...),
           '5' = mmp__indicator_trends_2_4(wq.idx, ...),
           '6' = mmp__indicator_trends_2_6(wq.idx, ...)
           )
}

## Subregional, type 0
mmp__indicator_trends_2_1 <- function(wq.idx, ...) {
    list2env(list2(...))
    wq.idx <- wq.idx %>%
        ungroup() %>%
        left_join(wq.sites %>%
                  dplyr::select(Subregion, Latitude, Subregion) %>%
                  group_by(Subregion) %>%
                  summarize_all(funs(mean(., na.rm=TRUE)))) %>% 
        arrange(desc(Latitude)) %>%
        mutate(Subregion=factor(Subregion, levels=unique(Subregion))) %>%
        mutate(Grade=MMP_generateOldGrades(Index))

    ggplot(wq.idx %>%
           filter(Year>2007),
           aes(y=Index, x=reportCardYear)) +
        geom_hline(yintercept=0, linetype='dashed') +
        geom_line() +
        geom_point(aes(fill=Grade),shape=21, size=3, show.legend = TRUE) +
        scale_y_continuous('Water Quality Index',limits=c(-1,1)) +
        scale_x_date('', limits=c(as.Date('2006-01-01'),
                                  as.Date(paste0(reportYear,'-01-01')))) +
        scale_fill_manual('',breaks=c('A','B','C','D','E'),
                          values=rev(trafficLightPalette),
                          limits=c('A','B','C','D','E'),
                          labels=c('Very Good','Good','Moderate','Poor','Very Poor')) +
        theme_mmp +
        theme(strip.background=element_blank(),
              panel.margin.x=unit(1,'line'))
}

## Subregional, type 3
mmp__indicator_trends_2_4 <- function(wq.idx, ...) {
    list2env(list2(...))
    wq.idx <- wq.idx %>%
        ungroup() %>%
        left_join(wq.sites %>%
                  dplyr::select(Subregion, Latitude, Subregion) %>%
                  group_by(Subregion) %>%
                  summarize_all(funs(mean(., na.rm=TRUE)))) %>% 
        arrange(desc(Latitude)) %>%
        mutate(Subregion=factor(Subregion, levels=unique(Subregion))) %>%
        mutate(Grade=MMP_generateOldGrades(Index))

    ggplot(wq.idx, ##%>%
           ##filter(Year>2007),
           aes(y=Index, x=reportCardYear)) +
        geom_hline(yintercept=0, linetype='dashed') +
        geom_line() +
        geom_point(aes(fill=Grade),shape=21, size=3, show.legend = TRUE) +
        scale_y_continuous('Water Quality Index',limits=c(-1,1)) +
        scale_x_date('', limits=c(as.Date('2006-01-01'),
                                  as.Date(paste0(reportYear,'-01-01')))) +
        scale_fill_manual('',breaks=c('A','B','C','D','E'),
                          values=rev(trafficLightPalette),
                          limits=c('A','B','C','D','E'),
                          labels=c('Very Good','Good','Moderate','Poor','Very Poor')) +
        theme_mmp +
        theme(strip.background=element_blank(),
              panel.margin.x=unit(1,'line'))
}

## Subregion/measure level
mmp__indicator_trends_2_6 <- function(wq.idx, ...) {
    list2env(list2(...))
    wq.idx <- wq.idx %>%
        ungroup() %>% 
        left_join(wq.sites %>%
                  dplyr::select(Region, Latitude, Region) %>%
                  group_by(Region) %>%
                  summarize_all(funs(mean(., na.rm=TRUE)))) %>% 
        arrange(desc(Latitude)) %>%
        mutate(Region = factor(Region, levels = unique(Region)),
               Subregion = factor(Subregion, levels = unique(Subregion)))%>%
        mutate(Index = scales::rescale(Score, to = c(-1,1), from = c(0,1))) %>%
        mutate_at(vars(Lower, Upper),
                  function(x) scales::rescale(x, to = c(-1,1), from = c(0,1))) %>% 
        mutate(reportCardYear = as.Date(paste0(reneeYear,'-01-01'))) %>%
        mutate(Grade = MMP_generateOldGrades(Index))

    ggplot(wq.idx, aes(y = Index, x = reportCardYear)) +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        geom_line() +
        geom_point(aes(fill = Grade), shape = 21, size = 3, show.legend = TRUE) +
        scale_y_continuous('Water Quality Index', limits = c(-1, 1)) +
        scale_x_date('', limits = c(as.Date('2006-01-01'),
                                    as.Date(paste0(reportYear,'-01-01')))) +
        scale_fill_manual('', breaks = c('A','B','C','D','E'),
                          values = rev(trafficLightPalette),
                          limits = c('A','B','C','D','E'),
                          labels = c('Very Good','Good','Moderate','Poor','Very Poor')) +
        theme_mmp +
        theme(strip.background = element_blank(),
              panel.margin.x = unit(1, 'line'),
              panel.border= element_rect(fill = NA))
}

## Region/measure level
mmp__indicator_trends_3_6 <- function(wq.idx, ...) {
    list2env(list2(...))
    wq.idx <- wq.idx %>%

        ungroup() %>% 
        left_join(wq.sites %>%
                  dplyr::select(Region, Latitude, Region) %>%
                  group_by(Region) %>%
                  summarize_all(funs(mean(., na.rm=TRUE)))) %>% 
        arrange(desc(Latitude)) %>%
        mutate(Region = factor(Region, levels = unique(Region))) %>%
        mutate(Index = scales::rescale(Score, to = c(-1,1), from = c(0,1))) %>%
        mutate_at(vars(Lower, Upper),
                  function(x) scales::rescale(x, to = c(-1,1), from = c(0,1))) %>% 
        mutate(reportCardYear = as.Date(paste0(reneeYear,'-01-01'))) %>%
        mutate(Grade = MMP_generateOldGrades(Index))

    ggplot(wq.idx, aes(y = Index, x = reportCardYear)) +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        geom_line() +
        geom_point(aes(fill = Grade), shape = 21, size = 3, show.legend = TRUE) +
        scale_y_continuous('Water Quality Index', limits = c(-1, 1)) +
        scale_x_date('', limits = c(as.Date('2006-01-01'),
                                    as.Date(paste0(reportYear,'-01-01')))) +
        scale_fill_manual('', breaks = c('A','B','C','D','E'),
                          values = rev(trafficLightPalette),
                          limits = c('A','B','C','D','E'),
                          labels = c('Very Good','Good','Moderate','Poor','Very Poor')) +
        theme_mmp +
        theme(strip.background = element_blank(),
              panel.margin.x = unit(1, 'line'),
              panel.border= element_rect(fill = NA))

}

## Subregion/subindicator level
mmp__indicator_trends_4_6 <- function(wq.idx, ...) {
    list2env(list2(...))
    wq.idx <- wq.idx %>%
        ungroup() %>% 
        left_join(wq.sites %>%
                  dplyr::select(Region, Latitude, Region) %>%
                  group_by(Region) %>%
                  summarize_all(funs(mean(., na.rm=TRUE)))) %>% 
        arrange(desc(Latitude)) %>%
        mutate(Region=factor(Region, levels=unique(Region)),
               Subregion=factor(Subregion, levels=unique(Subregion)))%>%
        mutate(Index = scales::rescale(Score, to = c(-1,1), from = c(0,1))) %>%
        mutate_at(vars(Lower, Upper),
                  function(x) scales::rescale(x, to = c(-1,1), from = c(0,1))) %>% 
        mutate(reportCardYear = as.Date(paste0(reneeYear,'-01-01'))) %>%
        mutate(Grade = MMP_generateOldGrades(Index))

    ggplot(wq.idx, aes(y = Index, x = reportCardYear)) +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        geom_line() +
        geom_point(aes(fill = Grade), shape = 21, size = 3, show.legend = TRUE) +
        scale_y_continuous('Water Quality Index', limits = c(-1, 1)) +
        scale_x_date('', limits = c(as.Date('2006-01-01'),
                                    as.Date(paste0(reportYear,'-01-01')))) +
        scale_fill_manual('', breaks = c('A','B','C','D','E'),
                          values = rev(trafficLightPalette),
                          limits = c('A','B','C','D','E'),
                          labels = c('Very Good','Good','Moderate','Poor','Very Poor')) +
        theme_mmp +
        theme(strip.background = element_blank(),
              panel.margin.x = unit(1, 'line'),
              panel.border= element_rect(fill = NA))

}

## Region/subindicator level
mmp__indicator_trends_5_6 <- function(wq.idx, ...) {
    list2env(list2(...))
    wq.idx <- wq.idx %>%
        ungroup() %>% 
        left_join(wq.sites %>%
                  dplyr::select(Region, Latitude, Region) %>%
                  group_by(Region) %>%
                  summarize_all(funs(mean(., na.rm=TRUE)))) %>% 
        arrange(desc(Latitude)) %>%
        mutate(Region = factor(Region, levels = unique(Region)))%>%
        mutate(Index = scales::rescale(Score, to = c(-1,1), from = c(0,1))) %>%
        mutate_at(vars(Lower, Upper),
                  function(x) scales::rescale(x,to=c(-1,1), from=c(0,1))) %>% 
        mutate(reportCardYear = as.Date(paste0(reneeYear,'-01-01'))) %>%
        mutate(Grade = MMP_generateOldGrades(Index))

    ggplot(wq.idx, aes(y = Index, x = reportCardYear)) +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        geom_line() +
        geom_point(aes(fill = Grade), shape = 21, size = 3, show.legend = TRUE) +
        scale_y_continuous('Water Quality Index', limits = c(-1, 1)) +
        scale_x_date('', limits = c(as.Date('2006-01-01'),
                                    as.Date(paste0(reportYear,'-01-01')))) +
        scale_fill_manual('', breaks = c('A','B','C','D','E'),
                          values = rev(trafficLightPalette),
                          limits = c('A','B','C','D','E'),
                          labels = c('Very Good','Good','Moderate','Poor','Very Poor')) +
        theme_mmp +
        theme(strip.background = element_blank(),
              panel.margin.x = unit(1, 'line'),
              panel.border= element_rect(fill = NA))

}

## subregion/subindicator level
mmp__indicator_trends_6_6 <- function(wq.idx, ...) {
    list2env(list2(...))
    wq.idx <- wq.idx %>%
        ungroup %>% 
        left_join(wq.sites %>%
                  dplyr::select(Region, Latitude, Region) %>%
                  group_by(Region) %>%
                  summarize_all(funs(mean(., na.rm=TRUE)))) %>% 
        arrange(desc(Latitude)) %>%
        mutate(Region = factor(Region, levels = unique(Region)),
               Subregion = factor(Subregion, levels = unique(Subregion)))%>%
        mutate(Index = scales::rescale(Score, to = c(-1,1), from = c(0,1))) %>%
        mutate_at(vars(Lower, Upper),
                  function(x) scales::rescale(x, to = c(-1,1), from = c(0,1))) %>% 
        mutate(reportCardYear = as.Date(paste0(reneeYear,'-01-01'))) %>%
        mutate(Grade = MMP_generateOldGrades(Index))

    ggplot(wq.idx, aes(y = Index, x = reportCardYear)) +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        geom_line() +
        geom_point(aes(fill = Grade), shape = 21, size = 3, show.legend = TRUE) +
        scale_y_continuous('Water Quality Index', limits = c(-1, 1)) +
        scale_x_date('', limits = c(as.Date('2006-01-01'),
                                    as.Date(paste0(reportYear,'-01-01')))) +
        scale_fill_manual('', breaks = c('A','B','C','D','E'),
                          values = rev(trafficLightPalette),
                          limits = c('A','B','C','D','E'),
                          labels = c('Very Good','Good','Moderate','Poor','Very Poor')) +
        theme_mmp +
        theme(strip.background = element_blank(),
              panel.margin.x = unit(1, 'line'),
              panel.border= element_rect(fill = NA))

}

## region/indicator level
mmp__indicator_trends_7_6 <- function(wq.idx, ...) {
    list2env(list2(...))
    wq.idx <- wq.idx %>%
        ungroup %>% 
        left_join(wq.sites %>%
                  dplyr::select(Region, Latitude, Region) %>%
                  group_by(Region) %>%
                  summarize_all(funs(mean(., na.rm=TRUE)))) %>% 
        arrange(desc(Latitude)) %>%
        mutate(Region = factor(Region, levels = unique(Region)))%>%
        mutate(Index = scales::rescale(Score, to = c(-1,1), from = c(0,1))) %>%
        mutate(Grade = MMP_generateOldGrades(Index)) %>%
        mutate_at(vars(Lower, Upper),
                  function(x) scales::rescale(x, to = c(-1,1), from = c(0,1))) %>% 
        mutate(reportCardYear = as.Date(paste0(reneeYear,'-01-01'))) %>%
        mutate(Grade = MMP_generateOldGrades(Index))

    ggplot(wq.idx, aes(y = Index, x = reportCardYear)) +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        geom_line() +
        geom_point(aes(fill = Grade), shape = 21, size = 3, show.legend = TRUE) +
        scale_y_continuous('Water Quality Index', limits = c(-1, 1)) +
        scale_x_date('', limits = c(as.Date('2006-01-01'),
                                    as.Date(paste0(reportYear,'-01-01')))) +
        scale_fill_manual('', breaks = c('A','B','C','D','E'),
                          values = rev(trafficLightPalette),
                          limits = c('A','B','C','D','E'),
                          labels = c('Very Good','Good','Moderate','Poor','Very Poor')) +
        theme_mmp +
        theme(strip.background = element_blank(),
              panel.margin.x = unit(1, 'line'),
              panel.border= element_rect(fill = NA))

}
