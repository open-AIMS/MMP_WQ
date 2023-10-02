
WQfitGAM <- function(data, bs='tp', k=5,fx=T, fam=quasipoisson(link='log'),
                     ignore_new_model_type=TRUE) {
    require(mgcv)
    ## wq.units <- read.table(PARAMS_PATH, '/wq.units.txt', header=TRUE,sep=';',strip.white=TRUE,quote="'")
    wq.units <- get(load(paste0(DATA_PATH, "/primary/other/wq.units.RData")))
    print(paste0(unique(data$Subregion),": ",unique(data$Measure)))
    err<-0
    valsLessThanOrEqualZero = data %>% filter(Value<=0)
    if (dim(valsLessThanOrEqualZero)[1]>0 & fam$family!='gaussian') {
        print(valsLessThanOrEqualZero)
        data=data %>% filter(Value>0)
    }
    data$Value = ifelse(is.infinite(data$Value), NA,data$Value)
    
    data = data %>% filter(!is.na(Value))
    if (nrow(data)==0) return(0)
    stable_outcome = FALSE
    while (stable_outcome==FALSE) {
        err=0
        ## New for 2020
        tryCatch(model <- gam(Value ~ s(Dt.num, bs=bs)+s(Mnth,bs='cc',k=5,fx=F) + s(MMP_SITE_NAME, bs='re'),
                              method='REML', 
                              correlation=corCAR1(form=~Dt.num|MMP_SITE_NAME), knots=list(Mnth=seq(1,12,length=5)),
                              data=data, family=fam),
                 error=function(x) err<<-0.5)
        if (ignore_new_model_type) err <- 0.5
        if (err==0) print('The above was fitted with Model type 2020')

        ## Fit the model (have a hierarchy of model options)
        if(err==0.5) {
            tryCatch(model <- gamm(Value ~ s(Dt.num, bs=bs,k=k,fx=fx)+s(Mnth,bs='cc',k=5,fx=F)
                                  ,correlation=corCAR1(form=~Dt.num|MMP_SITE_NAME), knots=list(Mnth=seq(1,12,length=5)),
                                   random=list(MMP_SITE_NAME=~1),data=data, family=fam),
                     error=function(x) err<<-1)
            print(err)
            if (err==0.5) print('The above was fitted with Model type 1')
        }
        if (err==1) {
            tryCatch(model <- gamm(Value ~ s(Dt.num, bs='tp')+s(Mnth,bs='cc',k=5,fx=F)
                                  ,knots=list(Mnth=seq(1,12,length=5))
                                  ,correlation=corCAR1(form=~Dt.num|MMP_SITE_NAME), random=list(MMP_SITE_NAME=~1),data=data,
                                   family=fam, control=list(maxIter=100)),
                     error=function(x) err<<-2)
            print(err)
            if (err==1) print('The above was fitted with Model type 2')
        }
        if (err==2) {
            tryCatch(model <- gamm(Value ~ s(Dt.num, bs='tp')+s(Mnth,bs='cc',k=5,fx=F)
                                  ,knots=list(Mnth=seq(1,12,length=5))
                                 , random=list(MMP_SITE_NAME=~1),data=data, family=fam,
                                   control=list(maxIter=100)),
                     error=function(x) err<<-3)
            if (err==2) print('The above was fitted with Model type 3')
        }
        if (err==3) {
            tryCatch(model <- gamm(Value ~ s(Dt.num, bs='tp',k=3)+s(Mnth,bs='cc',k=5,fx=F),
                                  ,knots=list(Mnth=seq(1,12,length=5))
                                 , random=list(MMP_SITE_NAME=~1),data=data, family=fam, control=list(maxIter=100)),
                     error=function(x) err<<-4)
            if (err==3) print('The above was fitted with Model type 4')
        }
        if (err==4) {
            tryCatch(model <- gamm(Value ~ s(Dt.num, k=3)+s(Mnth,bs='cc',k=5,fx=F),
                                  ,knots=list(Mnth=seq(1,12,length=5))
                                 , random=list(MMP_SITE_NAME=~1),data=data, family=fam),
                     error=function(x) err<<-5)
            if (err==4) print('The above was fitted with Model type 5')
        }
        if (err==5) {
            print('FAILED')
            nms = unique(data$MMP_SITE_NAME[data$Source=='AIMS Niskin'])
            data = data %>% filter(MMP_SITE_NAME==nms)                    
        }
        if (err<5) stable_outcome=TRUE
    }
    ## Start with the long-term trend
    Date <- as.POSIXct(seq(as.POSIXct(min(data$Date)),as.POSIXct(max(data$Date)),by="month"))
    if (err==0) {
        res <- resid(model)
    } else {
        res <- resid(model$gam)
    }
    ndf=expand.grid(Dt.num=decimal_date(Date),Mnth=0, MMP_SITE_NAME=NA)
    ndf$Date<-Date
                                        #pp<-predict(model$gam, newdata=ndf, type="response", se.fit=TRUE)
    if (err==0){
        fv.terms <- predict(model, newdata=ndf, type = "iterms",se.fit=TRUE, exclude='s(MMP_SITE_NAME)', newdata.guaranteed = TRUE)
        pp.fit <- fv.terms$fit[, 1] + coefficients(model)[[1]][1]
    } else {
        fv.terms <- predict(model$gam, newdata=ndf, type = "iterms",se.fit=TRUE)
        pp.fit <- fv.terms$fit[, 1] + coefficients(model$lme)[[1]][1]
    }
    pp.se <- fv.terms$se.fit[, 1] 
    
    if (err==0) {
        q=qt(0.975, df=model$df.resid)
        ndf$pred<-model$family$linkinv(pp.fit) #pp$fit
        ndf$lower <- model$family$linkinv(pp.fit - pp.se) #pp$fit - 1*pp$se.fit
        ndf$upper <- model$family$linkinv(pp.fit + pp.se) #pp$fit + 1*pp$se.fit
        ndf$lower2 <- model$family$linkinv(pp.fit - q*pp.se) #pp$fit - q*pp$se.fit
        ndf$upper2 <- model$family$linkinv(pp.fit + q* pp.se) #pp$fit + q*pp$se.fit
    } else {
        q=qt(0.975, df=model$gam$df.resid)
        ndf$pred<-model$gam$family$linkinv(pp.fit) #pp$fit
        ndf$lower <- model$gam$family$linkinv(pp.fit - pp.se) #pp$fit - 1*pp$se.fit
        ndf$upper <- model$gam$family$linkinv(pp.fit + pp.se) #pp$fit + 1*pp$se.fit
        ndf$lower2 <- model$gam$family$linkinv(pp.fit - q*pp.se) #pp$fit - q*pp$se.fit
        ndf$upper2 <- model$gam$family$linkinv(pp.fit + q* pp.se) #pp$fit + q*pp$se.fit
    }
    ndf$variable=unique(data$Measure)
                                        #print(ndf)
    ndf = ndf %>% left_join(wq.units %>% dplyr:::select(variable=Measure,Name.graphs,Name.graphs.abbr))
    
    ##partial residuals for long term trend
    if (err==0) {
        fv.terms <- predict(model, type = "iterms")
        w.resid <- model$residuals * sqrt(model$weights)
        centre <- predict(model, newdata=data.frame(Dt.num=decimal_date(mean(data$Date)),
                                                    Mnth=6, MMP_SITE_NAME=NA), exclude='s(MMP_SITE_NAME)', newdata.guaranteed = TRUE)
        p.resid <- model$family$linkinv(fv.terms[, 1] + w.resid + coefficients(model)[[1]][1]) #centre) #coefficients(model$lme)[[1]][1])
        
        ndf_resid=expand.grid(Dt.num=model$model$Dt.num,Mnth=0)
        ndf_resid$pred<-p.resid #as.vector(pp)+res 
        ndf_resid$variable <- unique(data$Measure)
        ndf_resid$Date<-date_decimal(ndf_resid$Dt.num)
        ndf_resid = ndf_resid %>% left_join(wq.units %>% dplyr:::select(variable=Measure,Name.graphs,Name.graphs.abbr)) %>%
            mutate(Source=data$Source)
    } else {
        fv.terms <- predict(model$gam, type = "iterms")
        w.resid <- model$gam$residuals * sqrt(model$gam$weights)
        centre <- predict(model$gam, newdata=data.frame(Dt.num=decimal_date(mean(data$Date)),
                                                        Mnth=6))
        p.resid <- model$gam$family$linkinv(fv.terms[, 1] + w.resid + coefficients(model$lme)[[1]][1]) #centre) #coefficients(model$lme)[[1]][1])
        
        ndf_resid=expand.grid(Dt.num=model$gam$model$Dt.num,Mnth=0)
        ndf_resid$pred<-p.resid #as.vector(pp)+res 
        ndf_resid$variable <- unique(data$Measure)
        ndf_resid$Date<-date_decimal(ndf_resid$Dt.num)
        ndf_resid = ndf_resid %>% left_join(wq.units %>% dplyr:::select(variable=Measure,Name.graphs,Name.graphs.abbr)) %>%
            mutate(Source=data$Source)
    }
    
    ## Now for the seasonal trend
    ndf1=expand.grid(Dt.num=decimal_date(mean(Date)),
                     Mnth=seq(0,12,l=100), MMP_SITE_NAME=NA)
                                        #pp<-predict(model$gam, newdata=ndf1, type="response", se.fit=TRUE)
    if (err==0) {
        fv.terms <- predict(model, newdata=ndf1, type = "iterms",se.fit=TRUE, exclude='s(MMP_SITE_NAME)', newdata.guaranteed = TRUE)
        pp.fit <- fv.terms$fit[, 2] + coefficients(model)[[1]][1]
        pp.se <- fv.terms$se.fit[, 2]
        q=qt(0.975, df=model$df.resid)
        ndf1$pred<-model$family$linkinv(pp.fit) #pp$fit
        ndf1$lower <- model$family$linkinv(pp.fit - pp.se) #pp$fit - 1*pp$se.fit
        ndf1$upper <- model$family$linkinv(pp.fit + pp.se) #pp$fit + 1*pp$se.fit
        ndf1$lower2 <- model$family$linkinv(pp.fit - q*pp.se) #pp$fit - q*pp$se.fit
        ndf1$upper2 <- model$family$linkinv(pp.fit + q* pp.se) #pp$fit + q*pp$se.fit
    } else {
        fv.terms <- predict(model$gam, newdata=ndf1, type = "iterms",se.fit=TRUE)
        pp.fit <- fv.terms$fit[, 2] + coefficients(model$lme)[[1]][1]
        pp.se <- fv.terms$se.fit[, 2]
        q=qt(0.975, df=model$gam$df.resid)
        ndf1$pred<-model$gam$family$linkinv(pp.fit) #pp$fit
        ndf1$lower <- model$gam$family$linkinv(pp.fit - pp.se) #pp$fit - 1*pp$se.fit
        ndf1$upper <- model$gam$family$linkinv(pp.fit + pp.se) #pp$fit + 1*pp$se.fit
        ndf1$lower2 <- model$gam$family$linkinv(pp.fit - q*pp.se) #pp$fit - q*pp$se.fit
        ndf1$upper2 <- model$gam$family$linkinv(pp.fit + q* pp.se) #pp$fit + q*pp$se.fit
    }
    ndf1$variable=unique(data$Measure)
                                        #print(ndf)
    ndf1 = ndf1 %>% left_join(wq.units %>% dplyr:::select(variable=Measure,Name.graphs,Name.graphs.abbr))
    
    ## Seasonal residuals
    if (err==0) {
        fv.terms <- predict(model, type = "iterms")
        w.resid <- model$residuals * sqrt(model$weights)
        centre <- predict(model, newdata=expand.grid(Dt.num=mean(model$model$Dt.num),Mnth=model$model$Mnth, MMP_SITE_NAME=NA), exclude = 's(MMP_SITE_NAME)', newdata.guaranteed = TRUE)
        p.resid1 <- model$family$linkinv(fv.terms[, 2] + w.resid + coefficients(model)[[1]][1]) #centre) #coefficients(model$lme)[[1]][1])
        
        ndf1_resid=expand.grid(Dt.num=0,Mnth=model$model$Mnth)
        ndf1_resid$pred<-p.resid1 #as.vector(pp)+res 
        ndf1_resid$variable <- unique(data$Measure)
        ndf1_resid = ndf1_resid %>% left_join(wq.units %>% dplyr:::select(variable=Measure,Name.graphs,Name.graphs.abbr)) %>%
            mutate(Source=data$Source)
    } else {
        fv.terms <- predict(model$gam, type = "iterms")
        w.resid <- model$gam$residuals * sqrt(model$gam$weights)
        centre <- predict(model$gam, newdata=expand.grid(Dt.num=mean(model$gam$model$Dt.num),Mnth=model$gam$model$Mnth))
        p.resid1 <- model$gam$family$linkinv(fv.terms[, 2] + w.resid + coefficients(model$lme)[[1]][1]) #centre) #coefficients(model$lme)[[1]][1])
        
        ndf1_resid=expand.grid(Dt.num=0,Mnth=model$gam$model$Mnth)
        ndf1_resid$pred<-p.resid1 #as.vector(pp)+res 
        ndf1_resid$variable <- unique(data$Measure)
        ndf1_resid = ndf1_resid %>% left_join(wq.units %>% dplyr:::select(variable=Measure,Name.graphs,Name.graphs.abbr)) %>%
            mutate(Source=data$Source)
    }
    ## ## ndf1$pred<-pp$fit
    ## ## ndf1$lower <- pp$fit - 1*pp$se.fit
    ## ## ndf1$upper <- pp$fit + 1*pp$se.fit
    ## ## ndf1$variable <- unique(data$Measure)
    ## ## ndf1 = ndf1 %>% left_join(wq.units %>% dplyr:::select(variable=Measure,Name.graphs,Name.graphs.abbr))
    ## ndf1_resid=expand.grid(Dt.num=decimal_date(mean(Date)),
    ##     Mnth=model$gam$model$Mnth)
    ## p.resid <- model$gam$family$linkinv(fv.terms[, 2] + w.resid + coefficients(model$lme)[[1]][1]) #centre) #coefficients(model$lme)[[1]][1])
    ## ndf1_resid$pred<-p.resid
    ## ndf1_resid$variable <- unique(data$Measure)
    ## ndf1_resid = ndf1_resid %>% left_join(wq.units %>% dplyr:::select(variable=Measure,Name.graphs,Name.graphs.abbr))
    list(model,ndf,ndf1,ndf_resid,ndf1_resid)
}

chemPlot <- function(data, resid, y.title=TRUE, guideline=TRUE, transform=TRUE,
                     Source='AIMS Niskin', color.source=TRUE) {
    old.wq.guidelines <- read.csv('../parameters/old.wq.guidelines.csv', strip.white=TRUE)
    wq.units <- get(load(paste0(DATA_PATH, '/primary/other/wq.units.RData')))
    lims <- unlist(wq.units %>%
                   filter(Measure==unique(data$Measure)) %>%
                   dplyr:::select(ymin,ymax))
    lims <- range(lims,data$lower2, data$upper2)

    GeomRibbon$handle_na <- function(data, params) { data }  # handle gaps in data range by a gap in ribbons
    data <- data %>% mutate(across(c(pred, lower, upper, lower2, upper2), function(x) ifelse(.$Obs==0, NA, x)))
    if (grepl('Niskin',Source)) lims = range(lims,quantile(resid$pred,p=0.8))
    names(lims) <- c('ymin','ymax')
                                        #print(lims)
    p = ggplot(data, aes(y=pred,x=Date)) + coord_cartesian(ylim=lims)
    if (guideline) {
        data = data %>% left_join(old.wq.guidelines %>% dplyr:::select(Subregion,Measure,GL))
                                        #print(head(data))
        p = p + geom_hline(data=NULL, yintercept=unique(data$GL), linetype='dashed')
    }
    if (Source %in% c('AIMS Niskin','AIMS/JCU Niskin', 'AIMS/JCU OMO Niskin')) {
        if (Source %in% c('AIMS/JCU Niskin', 'AIMS/JCU OMO Niskin') & color.source==TRUE) {
            p = p + geom_vline(xintercept=as.POSIXct('2015-07-01'), color='gray') +
                geom_point(data=resid, size=0.75,show.legend=FALSE) +
                geom_point(aes(color=Source),data=resid, size=0.5,show.legend=FALSE) +
                scale_color_manual(breaks=c('AIMS Niskin','JCU Niskin'),values=c('yellow','red'))
        } else {
            p = p+geom_point(data=resid, size=0.5)
        }
        
        p = p+geom_ribbon(aes(ymin=lower2, ymax=upper2), alpha=0.1, fill='blue',color=NA) +
            geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3, fill='blue',color=NA) +
            geom_line(color='blue')
        
    } else if (Source=='AIMS FLNTU') {
        p = p+#geom_point(data=resid, size=0.5) +
            geom_ribbon(aes(ymin=lower2, ymax=upper2), alpha=0.1, fill='red',color=NA) + 
            geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3, fill='red',color=NA) +
            geom_line(color='red')
    }
    
    p=p+ theme_classic() + theme_mmp + theme(axis.title.x=element_blank())
    if (transform) {
        if (y.title==TRUE) {
            lab=unique(data$Name.graphs.abbr)
                                        #lab=unique(data$Name.graphs)
                                        #                    eval(parse(text=paste0('p=p+scale_y_continuous(expression(paste(',lab,')),trans=log_trans(), breaks=base_breaks(n=5), labels=base_breaks(n=5))')))
            eval(parse(text=paste0('p=p+scale_y_continuous(expression(paste(',lab,')),trans=scales::log_trans(), breaks=base_breaks(n=5), labels=prettyNum)')))
            p=p+theme(axis.title.y=element_text(size=rel(1.25), margin=margin(r=1,unit='line'),vjust=1))
        } else p=p+scale_y_continuous('',trans=scales::log_trans(), breaks=base_breaks(n=5), labels=prettyNum)
    } else {
        if (y.title==TRUE) {
            lab=unique(data$Name.graphs.abbr)
                                        #lab=unique(data$Name.graphs)
            eval(parse(text=paste0('p=p+scale_y_continuous(expression(paste(',lab,')))')))
            p=p+theme(axis.title.y=element_text(size=rel(1.25), margin=margin(r=1,unit='line'),vjust=1))
        } else p=p+scale_y_continuous('')
    }
    p
}




MMP__gam_extract <- function(mod, subregion, measure, field) {
    res <- try(
        mod %>%
        filter(Subregion == subregion,
               Measure == measure) %>%
        droplevels() %>%
        pull(!!field) %>%
        `[[`(1),
        TRUE
    )
    if (inherits(res, "try-error")) return(NULL)
    else return(res)
}


MMP_GAMchange <- function(mod, yrs=5, refYear = NULL) {
    if (!is.null(refYear)) yrs <- floor(max(mod$gam$model$Dt.num)) - refYear
    if(length(mod) == 2) { #fitted with GAMM
        ## ndf.list <- list(Dt.num = c(max(mod$gam$model$Dt.num),  max(mod$gam$model$Dt.num)-yrs))
        ## ndf <- as.data.frame(ndf.list) %>% mutate(Mnth=0, MMP_SITE_NAME = NA)
        ## fv.terms <- predict(mod$gam, newdata=ndf, type = "iterms",se.fit=TRUE)
        ## pp.fit <- fv.terms$fit[, 1] + coefficients(mod$lme)[[1]][1]

        ## pp.se <- fv.terms$se.fit[, 1] 
        ## q=qt(0.975, df=mod$gam$df.resid)
        ## ndf$pred<-mod$gam$family$linkinv(pp.fit) #pp$fit
        ## ndf$lower <- mod$gam$family$linkinv(pp.fit - pp.se) #pp$fit - 1*pp$se.fit
        ## ndf$upper <- mod$gam$family$linkinv(pp.fit + pp.se) #pp$fit + 1*pp$se.fit
        ## ndf$lower2 <- mod$gam$family$linkinv(pp.fit - q*pp.se) #pp$fit - q*pp$se.fit
        ## ndf$upper2 <- mod$gam$family$linkinv(pp.fit + q* pp.se) #pp$fit + q*pp$se.fit
        ## ndf <- ndf %>%
        ##     dplyr::select(Dt.num, response = pred, lower.CL = lower, upper.CL = upper) %>%
        ##     mutate(Date = as.Date(date_decimal(Dt.num)))
        ndf.list <- list(Dt.num = c(max(mod$gam$model$Dt.num),  max(mod$gam$model$Dt.num)-yrs))
        cont1 <- emmeans(mod, ~Dt.num,
                         data = mod$gam$model,
                         at=ndf.list,
                         cov.reduce = FALSE,
                         type='response') %>%
            as.data.frame() %>%
            mutate(Date = as.Date(date_decimal(Dt.num)))
        cont2 <- emmeans(mod, ~Dt.num,
                         data = mod$gam$model,
                         at = ndf.list,
                         cov.reduce = FALSE,
                         type='response') %>%
            pairs() %>%
            confint() %>%
            as.data.frame()
    } else {             #fitted with GAM 
        ndf.list = list(Dt.num=c(max(mod$model$Dt.num),  max(mod$model$Dt.num)-yrs))
        cont1 <- emmeans(mod, ~Dt.num,
                         at=ndf.list,
                         cov.reduce = FALSE,
                         type='response') %>%
            as.data.frame() %>%
            mutate(Date = as.Date(date_decimal(Dt.num)))
        cont2 <- emmeans(mod, ~Dt.num,
                         at=ndf.list,
                         cov.reduce = FALSE,
                         type='response') %>%
            pairs() %>%
            confint() %>%
            as.data.frame()
    }
    cont1 <- cont1 %>%
        rename_with(function(x) ifelse(x == 'emmean', 'response', x))
    cont2 <- cont2 %>%
        rename_with(function(x) ifelse(x == 'estimate', 'ratio', x))
    cont.tab <-
        cont1 %>%
        dplyr::select(Date, response,  lower.CL, upper.CL) %>%
        mutate(Value = sprintf('%.2f (%.2f-%.2f)', response, lower.CL, upper.CL)) %>%
        dplyr::select(Date, Value) %>%
        mutate(Yr=year(Date),
               Dt=sprintf('(%s,  %s)', min(Date), max(Date))) %>%
        dplyr::select(-Date) %>%
        pivot_wider(names_from=Yr, values_from=c(Value)) %>%
        bind_cols(cont2 %>% dplyr::select(ratio, lower.CL, upper.CL))
    cont.tab
}


MMP__worm_historic_and_alt <- function(hist.idx, alt.idx, minDate, MAXDATE) {
    if (nrow(hist.idx) > 0) {
        g <- ggplot(data = hist.idx, aes(y = Index, x = reportCardYear)) +
            geom_hline(yintercept = 0, linetype = 'dashed') +
            geom_line() +
            geom_point(aes(fill = Grade), shape = 21,
                       size = 3, show.legend = FALSE)  +
            geom_line(data = alt.idx) +
            geom_point(data = alt.idx,
                       aes(fill = Grade), shape = 23,
                       size = 3, show.legend = FALSE) +
            geom_linerange(data = alt.idx, aes(ymin = Lower, ymax = Upper)) +
            scale_y_continuous(expression(atop(Water~Quality, Index)),
                               limits = c(-1,1)) +
            scale_x_date('', limits = c(minDate, MAXDATE)) +
            scale_fill_manual('', breaks = c('A','B','C','D','E'),
                              values = rev(trafficLightPalette),
                              limits = c('A','B','C','D','E')) +
            theme_mmp +
            theme(strip.background = element_blank(),
                  panel.margin.x = unit(1,'line'),
                  axis.title.y = element_text(size = rel(1.25))) +
            annotate(geom = 'text', x = as.Date(minDate),
                     y=Inf, label = 'a)', vjust = 1)
    } else {
        g <- ggplot(data = alt.idx, aes(y = Index, x = reportCardYear)) +
            geom_hline(yintercept = 0, linetype = 'dashed') +
            geom_line(data = alt.idx) +
            geom_point(data = alt.idx,
                       aes(fill = Grade), shape = 23,
                       size = 3, show.legend = FALSE) +
            geom_linerange(data = alt.idx, aes(ymin = Lower, ymax = Upper)) +
            scale_y_continuous(expression(atop(Water~Quality, Index)),
                               limits = c(-1,1)) +
            scale_x_date('', limits = c(minDate, MAXDATE)) +
            scale_fill_manual('', breaks = c('A','B','C','D','E'),
                              values = rev(trafficLightPalette),
                              limits = c('A','B','C','D','E')) +
            theme_mmp +
            theme(strip.background = element_blank(),
                  panel.margin.x = unit(1,'line'),
                  axis.title.y = element_text(size = rel(1.25))) +
            annotate(geom = 'text', x = as.Date(minDate),
                     y=Inf, label = 'a)', vjust = 1)
    }
    g
}

MMP__gam_chl_niskin_flntu <- function(niskin, flntu, subr) {
    
    pp <- niskin %>%
        MMP__gam_extract(subregion = subr, measure = 'DRIFTCHL_UGPERL.wm', field = "plot") 
    pt <- flntu %>%
        MMP__gam_extract(subregion = subr, measure = 'DRIFTCHL_UGPERL.wm', field = "year") 

    if (!is.null(pt)) {
        GeomRibbon$handle_na <- function(data, params) { data }  # handle gaps in data range by a gap in ribbons
        d1 <- pt %>%
            mutate(across(c(pred, lower, upper, lower2, upper2),
                          function(x) ifelse(.$Obs==0, NA, x)))
        pp <- pp + geom_ribbon(data = d1,
                               aes(y = pred, ymin = lower,
                                   ymax = upper, x = Date),
                               fill = 'red',color=NA,alpha=0.3) +
            geom_line(data=d1, aes(y=pred, x=Date), color='red')
    }

    pp
}

theme_nothing <- function (font_size = 14, font_family = "", rel_small = 12/14) {
    theme_void(base_size = font_size, base_family = font_family) %+replace% 
        theme(line = element_blank(), rect = element_blank(), 
            text = element_text(family = font_family, face = "plain", 
                color = "black", size = font_size, lineheight = 0.9, 
                hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), 
                debug = FALSE), axis.line = element_blank(), 
            axis.line.x = NULL, axis.line.y = NULL, axis.text = element_blank(), 
            axis.text.x = NULL, axis.text.x.top = NULL, axis.text.y = NULL, 
            axis.text.y.right = NULL, axis.ticks = element_blank(), 
            axis.ticks.length = unit(0, "pt"), axis.title = element_blank(), 
            axis.title.x = NULL, axis.title.x.top = NULL, axis.title.y = NULL, 
            axis.title.y.right = NULL, legend.background = element_blank(), 
            legend.spacing = unit(font_size, "pt"), legend.spacing.x = NULL, 
            legend.spacing.y = NULL, legend.margin = margin(0, 
                0, 0, 0), legend.key = element_blank(), legend.key.size = unit(1.1 * 
                font_size, "pt"), legend.key.height = NULL, legend.key.width = NULL, 
            legend.text = element_text(size = rel(rel_small)), 
            legend.text.align = NULL, legend.title = element_text(hjust = 0), 
            legend.title.align = NULL, legend.position = "none", 
            legend.direction = NULL, legend.justification = "center", 
            legend.box = NULL, legend.box.margin = margin(0, 
                0, 0, 0), legend.box.background = element_blank(), 
            legend.box.spacing = unit(font_size, "pt"), panel.background = element_blank(), 
            panel.border = element_blank(), panel.grid = element_blank(), 
            panel.grid.major = NULL, panel.grid.minor = NULL, 
            panel.spacing = unit(font_size/2, "pt"), panel.spacing.x = NULL, 
            panel.spacing.y = NULL, panel.ontop = FALSE, strip.background = element_blank(), 
            strip.text = element_blank(), strip.text.x = NULL, 
            strip.text.y = NULL, strip.placement = "inside", 
            strip.placement.x = NULL, strip.placement.y = NULL, 
            strip.switch.pad.grid = unit(0, "cm"), strip.switch.pad.wrap = unit(0, 
                "cm"), plot.background = element_blank(), plot.title = element_blank(), 
            plot.subtitle = element_blank(), plot.caption = element_blank(), 
            plot.tag = element_text(face = "bold", hjust = 0, 
                vjust = 0.7), plot.tag.position = c(0, 1), plot.margin = margin(0, 
                0, 0, 0), complete = TRUE)
}

MMP__gam_ntu_flntu <- function(niskin, flntu, subr) {

    pt <- flntu %>%
        MMP__gam_extract(subregion = subr, measure = 'NTU', field = "plot") 
    if(is.null(pt)) {
        pp.ntu <- ggplot() + theme_nothing()
    } else {
        pp.ntu <- pt 
    }
    pp.ntu
}

inc.ltr <- function(x) {
    i <- match(x, letters)
    new.ltr <- letters[i+1]
    parent_env <- parent.env(environment())
    parent_env$ltr <<- new.ltr
    ## assign(ltr, new.ltr, envir = parent.env(environment()))
    new.ltr
}

inc <- function(x) eval.parent(substitute(x <-x +1))

MMP__gam_plot1 <- function(subr, index_plot, chl_plot, ntu_plot, wq.gams) {
    cnt <- 1
    if (subr != "Barron Daintree") {
        p.list <- list(
            index_plot,
            chl_plot +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "NOx.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "PO4.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            ntu_plot + annotate(geom = 'text',
                                x = as.POSIXct(paste0(minDate,' 12:00:00')),
                                y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "TSS_MGPERL.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "SECCHI_DEPTH.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "PN.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "PP.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "POC.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "DOC.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1)
        )
    } else {
        p.list <- list(
            index_plot,
            chl_plot +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "NOx.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "PO4.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "TSS_MGPERL.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "SECCHI_DEPTH.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "PN.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "PP.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "POC.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "DOC.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1)
        )
    }
    patchwork::wrap_plots(p.list, ncol = 2)
}

MMP__worm_historic <- function(hist.idx, minDate, MAXDATE) {
    if (nrow(hist.idx) > 0) {
        g <- ggplot(data = hist.idx, aes(y = Index, x = reportCardYear)) +
            geom_hline(yintercept = 0, linetype = 'dashed') +
            geom_line() +
            geom_point(aes(fill = Grade), shape = 21,
                       size = 3, show.legend = FALSE)  +
            scale_y_continuous(expression(atop(Water~Quality, Index)),
                               limits = c(-1,1)) +
            scale_x_date('', limits = c(minDate, MAXDATE)) +
            scale_fill_manual('', breaks = c('A','B','C','D','E'),
                              values = rev(trafficLightPalette),
                              limits = c('A','B','C','D','E')) +
            theme_mmp +
            theme(strip.background = element_blank(),
                  panel.margin.x = unit(1,'line'),
                  axis.title.y = element_text(size = rel(1.25))) +
            annotate(geom = 'text', x = as.Date(minDate),
                     y=Inf, label = 'a)', vjust = 1)
    } else {
        ## g <- ggplot(data = alt.idx, aes(y = Index, x = reportCardYear)) +
        ##     geom_hline(yintercept = 0, linetype = 'dashed') +
        ##     scale_y_continuous(expression(atop(Water~Quality, Index)),
        ##                        limits = c(-1,1)) +
        ##     scale_x_date('', limits = c(minDate, MAXDATE)) +
        ##     scale_fill_manual('', breaks = c('A','B','C','D','E'),
        ##                       values = rev(trafficLightPalette),
        ##                       limits = c('A','B','C','D','E')) +
        ##     theme_mmp +
        ##     theme(strip.background = element_blank(),
        ##           panel.margin.x = unit(1,'line'),
        ##           axis.title.y = element_text(size = rel(1.25))) +
        ##     annotate(geom = 'text', x = as.Date(minDate),
        ##              y=Inf, label = 'a)', vjust = 1)
        g <- ggplot(data = NULL) + theme_nothing()
    }
}

MMP__worm_alt <- function(alt.idx, subregion.worm, minDate, MAXDATE) {
    ggplot(data = alt.idx, aes(y = Index, x = reportCardYear)) +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        geom_line(data = subregion.worm,
                  aes(color = Subindicator), show.legend = FALSE) +
        geom_linerange(data = alt.idx, aes(ymin = Lower, ymax = Upper)) +
        geom_line(data= alt.idx) +
        geom_point(aes(fill = Grade), shape = 23, size = 3, show.legend = FALSE) +
        scale_y_continuous(expression(atop(Water~Quality,Index)), limits = c(-1,1)) +
        scale_x_date('', limits = c(minDate, MAXDATE)) +
        scale_fill_manual('', breaks = c('A','B','C','D','E'),
                          values = rev(trafficLightPalette),
                          limits = c('A','B','C','D','E')) +
        theme_mmp +
        theme(strip.background = element_blank(),
              panel.margin.x = unit(1,'line'),
              axis.title.y = element_text(size = rel(1.25))) +
        annotate(geom = 'text', x = as.Date(minDate),
                 y = Inf, label = 'b)', vjust = 1)
}

MMP__gam_plot2 <- function(subr, index_hist_plot, index_alt_plot, chl_plot, ntu_plot, wq.gams) {
    cnt <- 2
    if (subr != "Barron Daintree") {
        p.list <- list(
            index_hist_plot,
            index_alt_plot,
            chl_plot +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "NOx.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "PO4.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            ntu_plot + annotate(geom = 'text',
                                x = as.POSIXct(paste0(minDate,' 12:00:00')),
                                y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "TSS_MGPERL.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "SECCHI_DEPTH.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "PN.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "PP.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "POC.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "DOC.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1)
        )
    } else {
        p.list <- list(
            index_hist_plot,
            index_alt_plot,
            chl_plot +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "NOx.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "PO4.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "TSS_MGPERL.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "SECCHI_DEPTH.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "PN.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "PP.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "POC.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
            wq.gams %>% MMP__gam_extract(subregion = subr,
                                         measure = "DOC.wm", field = "plot") +
            annotate(geom = 'text',
                     x = as.POSIXct(paste0(minDate,' 12:00:00')),
                     y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1)
        )
    }
    patchwork::wrap_plots(p.list, ncol = 2) 
}

MMP__gam_plot3a <- function(subr, index_plot, chl_plot, ntu_plot, wq.gams) {
    cnt <- 1
    if (subr != "Barron Daintree") {
    patchwork::wrap_plots(
                   index_plot,
                   ntu_plot + annotate(geom = 'text',
                            x = as.POSIXct(paste0(minDate,' 12:00:00')),
                            y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
                   wq.gams %>% MMP__gam_extract(subregion = subr,
                                                measure = "SECCHI_DEPTH.wm", field = "plot") +
                   annotate(geom = 'text',
                            x = as.POSIXct(paste0(minDate,' 12:00:00')),
                            y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
                   wq.gams %>% MMP__gam_extract(subregion = subr,
                                                measure = "TSS_MGPERL.wm", field = "plot") +
                   annotate(geom = 'text',
                            x = as.POSIXct(paste0(minDate,' 12:00:00')),
                            y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
                   chl_plot +
                   annotate(geom = 'text',
                            x = as.POSIXct(paste0(minDate,' 12:00:00')),
                            y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1)
               ) +
        patchwork::plot_layout(ncol = 2)
    } else {
        patchwork::wrap_plots(
                       index_plot,
                       wq.gams %>% MMP__gam_extract(subregion = subr,
                                                    measure = "SECCHI_DEPTH.wm", field = "plot") +
                       annotate(geom = 'text',
                                x = as.POSIXct(paste0(minDate,' 12:00:00')),
                                y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
                       wq.gams %>% MMP__gam_extract(subregion = subr,
                                                    measure = "TSS_MGPERL.wm", field = "plot") +
                       annotate(geom = 'text',
                                x = as.POSIXct(paste0(minDate,' 12:00:00')),
                                y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
                       chl_plot +
                       annotate(geom = 'text',
                                x = as.POSIXct(paste0(minDate,' 12:00:00')),
                                y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1)
                   ) +
            patchwork::plot_layout(ncol = 2)
    }
}

MMP__gam_plot3b <- function(subr, wq.gams) {
    cnt <- 0
    patchwork::wrap_plots(
                   wq.gams %>% MMP__gam_extract(subregion = subr,
                                                measure = "NOx.wm", field = "plot") +
                   annotate(geom = 'text',
                            x = as.POSIXct(paste0(minDate,' 12:00:00')),
                            y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
                   wq.gams %>% MMP__gam_extract(subregion = subr,
                                                measure = "PO4.wm", field = "plot") +
                   annotate(geom = 'text',
                            x = as.POSIXct(paste0(minDate,' 12:00:00')),
                            y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
                   wq.gams %>% MMP__gam_extract(subregion = subr,
                                                measure = "PN.wm", field = "plot") +
                   annotate(geom = 'text',
                            x = as.POSIXct(paste0(minDate,' 12:00:00')),
                            y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
                   
                   wq.gams %>% MMP__gam_extract(subregion = subr,
                                                measure = "PP.wm", field = "plot") +
                   annotate(geom = 'text',
                            x = as.POSIXct(paste0(minDate,' 12:00:00')),
                            y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
                   wq.gams %>% MMP__gam_extract(subregion = subr,
                                                measure = "POC.wm", field = "plot") +
                   annotate(geom = 'text',
                            x = as.POSIXct(paste0(minDate,' 12:00:00')),
                            y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
                   wq.gams %>% MMP__gam_extract(subregion = subr,
                                                measure = "DOC.wm", field = "plot") +
                   annotate(geom = 'text',
                            x = as.POSIXct(paste0(minDate,' 12:00:00')),
                            y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1)
               ) +
        patchwork::plot_layout(ncol = 2)
}

MMP__gam_plot4 <- function(subr, index_hist_plot, index_alt_plot, chl_plot, ntu_plot, wq.gams) {
    cnt <- 2
    if (subr != "Barron Daintree") {
        patchwork::wrap_plots(
                       index_hist_plot,
                       index_alt_plot,
                       ntu_plot + annotate(geom = 'text',
                                           x = as.POSIXct(paste0(minDate,' 12:00:00')),
                                           y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
                       wq.gams %>% MMP__gam_extract(subregion = subr,
                                                    measure = "SECCHI_DEPTH.wm", field = "plot") +
                       annotate(geom = 'text',
                                x = as.POSIXct(paste0(minDate,' 12:00:00')),
                                y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
                       wq.gams %>% MMP__gam_extract(subregion = subr,
                                                    measure = "TSS_MGPERL.wm", field = "plot") +
                       annotate(geom = 'text',
                                x = as.POSIXct(paste0(minDate,' 12:00:00')),
                                y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
                       chl_plot +
                       annotate(geom = 'text',
                                x = as.POSIXct(paste0(minDate,' 12:00:00')),
                                y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1)
                   ) +
            patchwork::plot_layout(ncol = 2)
    } else {
        patchwork::wrap_plots(
                       index_hist_plot,
                       index_alt_plot,
                       wq.gams %>% MMP__gam_extract(subregion = subr,
                                                    measure = "SECCHI_DEPTH.wm", field = "plot") +
                       annotate(geom = 'text',
                                x = as.POSIXct(paste0(minDate,' 12:00:00')),
                                y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
                       wq.gams %>% MMP__gam_extract(subregion = subr,
                                                    measure = "TSS_MGPERL.wm", field = "plot") +
                       annotate(geom = 'text',
                                x = as.POSIXct(paste0(minDate,' 12:00:00')),
                                y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1),
                       chl_plot +
                       annotate(geom = 'text',
                                x = as.POSIXct(paste0(minDate,' 12:00:00')),
                                y = Inf, label = paste0(letters[inc(cnt)],')'), vjust = 1)
                   ) +
            patchwork::plot_layout(ncol = 2)
    }
}
geom_rugRect <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         outside = FALSE,
                         sides = "bl",
                         length = unit(0.03, "npc"),
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRugRect,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            outside = outside,
            sides = sides,
            length = length,
            na.rm = na.rm,
            ...
        )
    )
}

GeomRugRect <- ggproto("GeomRugRect", Geom,
                       optional_aes = c("x", "y"),
                       required_aes = c("xmin", "xmax"),
                       draw_panel = function(data, panel_params, coord, sides = "bl", outside = FALSE, length = unit(0.03, "npc")) {
                           require('grid')  
                           if (!inherits(length, "unit")) {
                               abort("'length' must be a 'unit' object.")
                           }
                           rugs <- list()
                           data <- coord$transform(data, panel_params)
                                        # For coord_flip, coord$tranform does not flip the sides where to
                                        # draw the rugs. We have to flip them.
                           if (inherits(coord, 'CoordFlip')) {
                               sides <- chartr('tblr', 'rlbt', sides)
                           }
                                        # move the rug to outside the main plot space
                           rug_length <- if (!outside) {
                                             list(min = length, max = unit(1, "npc") - length)
                                         } else {
                                             list(min = -1 * length, max = unit(1, "npc") + length)
                                         }
                           gp <- grid::gpar(col = alpha(data$colour, data$alpha), lty = data$linetype, lwd = data$size * .pt,  fill=data$fill)
                           if (!is.null(data$x)) {
                               if (grepl("b", sides)) {
                                   rugs$x_b <- grid::segmentsGrob(
                                                         x0 = unit(data$x, "native"), x1 = unit(data$x, "native"),
                                                         y0 = unit(0, "npc"), y1 = rug_length$min,
                                                         gp = gp
                                                     )
                                   print(data)
                                        #print(panel_params)
                                        #data[nrow(data), 'xmax'] <- panel_params$x.range[2] - diff(panel_params$x.range)*0.05
                                   rugs$x_b <- grid::rectGrob(
                                                         unit(data$xmin,  "native"), unit(0, "npc"),
                                                         width=data$xmax-data$xmin,
                                                         height=rug_length$min,
                                                         default.units='native',
                                                         just=c('left', 'bottom'), 
                                                         gp = gp
                                                     )
                               }
                           }
                           grid::gTree(children = do.call("gList", rugs))
                       },
                       default_aes = aes(colour = "black", size = 0.5, fill='white', linetype = 1, alpha = NA),
                       draw_key = draw_key_path
                       )


make_newdata = function(df) {
    Distance=with(df, seq(min(Distance), max(Distance), len=100))
}
my_gam = function(df) {
    if (nrow(df)>1) {
        err=0
        tryCatch(g<-gam(Value~ s(Distance, bs='cr', k=5), data=df, family=Gamma(link='log')),
                 error=function(x) err<<-1)
        if (err==0) return(g)
        if (err==1) {
            tryCatch(g<-gam(Value~ s(Distance, bs='cr', k=3), data=df, family=Gamma(link='log')),
                     error=function(x) err<<-2)
            if (err==2) return(NULL)
            return(g)
        }
    } else { return(NULL)}

}

pred_gam = function(mod, Distance) {
    if (is.null(mod)) {
        return(data.frame(Value=rep(NA, length(Distance)))%>%pull(Value))
    } else {
        return(predict.gam(mod, newdata=data.frame(Distance=Distance), type='response'))
    }
}
