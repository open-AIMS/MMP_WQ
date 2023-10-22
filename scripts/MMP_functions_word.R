countExceeds <- function(value,GL,DOF) {
    v=value
    value = v[!is.na(v) & !is.na(GL)]
    GL=GL[!is.na(v) & !is.na(GL)]
    if (length(value)==0 | all(is.nan(v)) | any(is.na(DOF))) {
        return(NA)
    } else {
        if (unique(DOF)=='H') cnt=100*length(value[value>GL])/length(value)
        else cnt=100*length(value[value<GL])/length(value)
        cnt
    }
}


SE <- function(x) {
    sd(x,na.rm=TRUE)/sqrt(length(x))
}


MMP__docx_table <- function(dat, docx.tab.count) {
    m1=grep('NTU_m',colnames(dat))
    m2=grep('NTU_GL_m',colnames(dat))
    c1=ifelse(is.na(dat[,m1]) | is.na(dat[,m2]), 'white', ifelse(dat[,m1]>dat[,m2],'red','white'))
    
    wch=grepl('DRIFT|GL',colnames(dat))
    dat = dat[,!wch] %>% as.data.frame
    wch=!grepl('Subregion|Site|NTU_N',colnames(dat))
    dat[,wch] <- round(apply(dat[,wch],2,as.numeric),2)

    key_cols <- colnames(dat)
    what.cols <- str_replace(str_subset(key_cols, "NTU_N.*"),
                             "NTU_N_(.*) - (.*)", "Oct \\1 - Sept \\2")
    
    typology = data.frame(
        col_keys = key_cols,
        what = c('Subregion', 'Site',
                 rep(what.cols, each = 6)), 
        ## col_keys=c('Subregion','Site',
        ##            ## '2007 - 2008_NTU_N','2007 - 2008_NTU_mean', '2007 - 2008_NTU_se', '2007 - 2008_NTU_median', '2007 - 2008_NTU_GT', '2007 - 2008_NTU_GT5',
        ##            '2008 - 2009_NTU_N','2008 - 2009_NTU_mean', '2008 - 2009_NTU_se', '2008 - 2009_NTU_median', '2008 - 2009_NTU_GT', '2008 - 2009_NTU_GT5',
        ##            '2009 - 2010_NTU_N','2009 - 2010_NTU_mean', '2009 - 2010_NTU_se', '2009 - 2010_NTU_median', '2009 - 2010_NTU_GT', '2009 - 2010_NTU_GT5',
        ##            '2010 - 2011_NTU_N','2010 - 2011_NTU_mean', '2010 - 2011_NTU_se', '2010 - 2011_NTU_median', '2010 - 2011_NTU_GT', '2010 - 2011_NTU_GT5'),
        ## what=c('Subregion','Site',
        ##        ## rep('Oct 2007 - Sept 2008', 6),
        ##        rep('Oct 2008 - Sept 2009', 6),
        ##        rep('Oct 2009 - Sept 2010', 6),
        ##        rep('Oct 2010 - Sept 2011', 6)
        ##        ),
        measure=c('Subregion','Site',
                  rep(c('N','Annual Mean','SE','Annual Median','%d > Trigger','%d > 5 Trigger'),3)),
        stringsAsFactors = FALSE )

    tab <- flextable(data=dat) %>%
        set_header_df(mapping=typology, key='col_keys') %>%
        colformat_num(col_keys=typology$col_keys[-1:-2],
                      digits=2, big.mark='') %>% 
        merge_h(part = "header") %>%
        merge_v(part = "header") %>%
        merge_v(j=1,part = "body") %>%
        theme_booktabs() %>%
        flextable::align(i=1:2, align='center', part='header') %>%
        flextable::align(j = -1:-2, align='center', part='body') %>%
        flextable::align(j = 1:2, align='left', part='body') %>%
        valign(j=1, valign='top') %>%
        border_inner_v( border=fp_border(color="black")) %>%
        border_outer( border=fp_border(color="black", width=2)) %>%
        hline(part='body', i=with(dat, match(unique(Subregion),Subregion))[-1]-1, border=fp_border(color='black')) %>%
        hline(part='body', j=-1, border=fp_border(color='black')) %>%
        fontsize(size = 8, part = "all") %>%
        padding(padding=0, part='all') %>%
        width(j=1:20, width=c(1,1,
                              rep(0.45,6), rep(0.45,6), rep(0.45,6))) %>%
        bg(j=c(4,6,10,12,16,18), bg=c1) %>% 
        fix_border_issues() %>%
        set_caption(paste("Table ",docx.tab.count,". Summary statistics for all data from direct water sampling at 20 Great Barrier Reef inshore lagoon sites from ", paste(what.cols, collapse = ", "), ".
N= number of sampling occasions. Data are in mg L⁻¹ for suspended solids (SS) and m for Secchi depth. All other parameters are in µgL⁻¹ (see main report for abbreviations). Long-term averages that exceed available water quality guidelines (DERM 2009, GBRMPA 2009) are shaded in red."))

    tab
    
}

