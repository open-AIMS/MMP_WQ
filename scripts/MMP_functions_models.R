
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
    if (Source %in% c('AIMS Niskin','AIMS/JCU Niskin')) {
        if (Source == 'AIMS/JCU Niskin' & color.source==TRUE) {
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



