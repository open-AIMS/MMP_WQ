source("MMP_functions.R")

## if the calling application has landed on this script as the running
## script, then start initialisations
if (MMP_isParent()) {
    MMP_startMatter()
}

LOGGER_OUTPUT_PATH <- paste0(DATA_PATH, "/processed/loggers/")
OTHER_OUTPUT_PATH <- paste0(DATA_PATH, "/processed/other/")
MAXDATE=as.Date(paste0(reportYear,'-09-30'))
MINDATE=MAXDATE-years(1)+days(1)

START_DATE <<- as.Date("2014-09-30")
START_DATE <<- MAXDATE - years(5)

## ---- timeseries plots 
CURRENT_ITEM <- "timeseries"
mmp__change_status(stage = paste0("STAGE", CURRENT_STAGE), item = CURRENT_ITEM, status = "progress")
MMP_openning_banner()

if (alwaysExtract &
    all(file.exists(paste0(LOGGER_OUTPUT_PATH, 'flntu.all.daily.RData'),
                    paste0(OTHER_OUTPUT_PATH, 'bom.weather.RData')
                    ))
    )
{
      load(file=paste0(LOGGER_OUTPUT_PATH, 'flntu.all.daily.RData'))
      load(file=paste0(OTHER_OUTPUT_PATH, 'bom.weather.RData'))
      load(file=paste0(LOGGER_OUTPUT_PATH, 'waterTempWAll.RData'))
      load(file=paste0(OTHER_OUTPUT_PATH, 'tides.daily.RData'))
      load(file=paste0(OTHER_OUTPUT_PATH, 'discharge.RData'))
      load(file=paste0(DATA_PATH, '/primary/other/river.lookup.RData'))
      lookup <- read.csv(paste0(PARAMS_PATH, '/lookup.csv'), strip.white = TRUE) %>% suppressMessages()

      unlink(paste0(DATA_PATH, "/reports/STAGE",CURRENT_STAGE, "_", CURRENT_ITEM, "_.RData")) 
      MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                             SECTION = paste0("# ", mmp__get_name(stage = paste0("STAGE",CURRENT_STAGE),
                                                                  item = CURRENT_ITEM),"\n\n"),
                             TABSET = paste0("::: panel-tabset \n\n"),
                             TABSET_END = paste0("::: \n\n")
                             )

      walk(.x = unique(flntu.all.daily$MMP_SITE_NAME),
           .f = function(S) {
               ## print(S)
               data <- mmp__timeseries_prepare_data(flntu = flntu.all.daily,
                                                    tides = tides.daily,
                                                    waterTemp = waterTempWAll,
                                                    wind = bom.weather,
                                                    discharge = discharge,
                                                    Site = S,
                                                    START_DATE)
               if (length(data)>1) {

                   wch <- which(unique(flntu.all.daily$MMP_SITE_NAME) == S)
                   png(filename = paste0(OUTPUT_PATH, '/figures/processed/timeseries_', S, '.png'),
                       res=300, width=6.299, height=2.756,units='in', pointsize=6
                       )
                   mmp__timeseries_plot(flntu = data$flntu,
                                        tides = data$tides,
                                        temperature = data$waterTemp,
                                        weather = data$wind,
                                        discharge = data$discharge,
                                        GL.chl = data$GL.chl, GL.ntu = data$GL.ntu,
                                        subtitle = paste0(letters[wch], ") ", S))
                   dev.off()
               }
           }
           )

      walk(.x = unique(flntu.all.daily$MMP_SITE_NAME),
                 .f = function(S) {
                       SS <- str_replace_all(S, ' ','_')
                       MMP_add_to_report_list(CURRENT_STAGE, CURRENT_ITEM,
                                              !!!setNames(list(
                                                      structure(paste0("### ", S, "\n"),
                                                                parent = 'TABSET')),
                                                      paste0('SUBSECTION_timeseries_',S)
                                                      ), 
                                              !!!setNames(list(
                                                     structure(paste0("\n::: {#fig-sql-timeseries-",SS,"}\n"),
                                                               parent = paste0('SUBSECTION_timeseries_', S))),
                                                     paste0('FIG_REF_',S)
                                                     ),
                                              !!!setNames(list(
                                                     structure(paste0("![](",OUTPUT_PATH,"/figures/processed/timeseries_", S, ".png)\n"),
                                                               parent = paste0("FIG_REF_", S))),
                                                     paste0('FIG_', S)
                                                     ),
                                              !!!setNames(list(
                                                          structure(paste0("\nDaily river discharge (blue), turbidity (NTU: red), chlorophyll-a (green), wind speed, tidal range and water temperature for the ", S, " subregion. Horizontal dashed lines represent guideline values.\n"),
                                                                    parent = paste0('FIG_REF_',S))),
                                                          paste0('FIG_CAP_',SS)),
                                              !!!setNames(list(
                                                     structure(paste0("\n::: \n"),
                                                               parent = paste0('SUBSECTION_timeseries_',S))),
                                                     paste0('FIG_END_', S)
                                                     ) 
                                              )
                 }
                 )
      
      ## flntu <- flntu.all.daily %>%
      ##     filter(Date>=START_DATE) %>%
      ##     filter(MMP_SITE_NAME == 'Fitzroy West') %>%
      ##     droplevels() %>%
      ##     complete(Date = seq.Date(min(Date), max(Date), by = 'day')) %>%
      ##     mutate(CHL = scales::rescale(CHL_QA_AVG, to = c(0, 1)),
      ##            Panel = '1')
      ## flntu.rng <- flntu$CHL_QA_AVG %>% range(na.rm=TRUE)
      ## flntu.ntu <- flntu.all.daily %>%
      ##     filter(Date>=START_DATE) %>%
      ##     filter(MMP_SITE_NAME == 'Fitzroy West') %>%
      ##     droplevels() %>%
      ##     complete(Date = seq.Date(min(Date), max(Date), by = 'day')) %>%
      ##     mutate(NTU = scales::rescale(NTU_QA_AVG, to = c(0, 1)),
      ##            Panel = '2')
      ## flntu.NTU.rng <- flntu$NTU_QA_AVG %>% range(na.rm=TRUE)
 
      ## waterTemp <- waterTempWAll %>%
      ##     filter(Date>=START_DATE) %>%
      ##     filter(MMP_SITE_NAME == 'Fitzroy West') %>%
      ##     droplevels() %>%
      ##     complete(Date = seq.Date(min(Date), max(Date), by = 'week')) %>%
      ##     mutate(T = scales::rescale(Temp, to = c(0.5, 1)),
      ##            Panel = '1')
      ## waterTemp.rng <- waterTemp$Temp %>% pretty(na.rm=TRUE) %>% range()
          
      ## ggplot() +
      ##     geom_line(data = flntu,
      ##               aes(y = CHL, x = Date)) +
      ##     geom_line(data = flntu.ntu,
      ##               aes(y = NTU, x = Date)) +
      ##     geom_line(data = waterTemp,
      ##               aes(y = T, x = Date)) +
      ##     ## scale_x_date('',date_breaks = scales::date_breaks(width = '3 month'))
      ##     scale_y_continuous(str_wrap('Chlorophyl-a', 25),
      ##                      label = function(x) pretty(scales::rescale(x, to = flntu.rng)),
      ##                      guide = ggh4x::guide_axis_truncated(),
      ##                      sec.axis = sec_axis(~.*1,
      ##                                          name = str_wrap("Daily temperature", 25),
      ##                                          label = function(x) scales::rescale(x, to = waterTemp.rng),
      ##                                          breaks = scales::rescale(pretty(waterTemp.rng), to = c(0.5, 1)),
      ##                                          guide = ggh4x::guide_axis_truncated(trunc_lower = 0.5))
      ##                      ) +
      ##     scale_x_date('',date_breaks = '6 months', date_labels = "%b") +
      ##     facet_grid(Panel~year(Date), switch = 'x', space="free_x", scales="free_x") +
      ##     theme_classic() + 
      ##     theme(strip.placement = "outside",
      ##           strip.background = element_blank(),
      ##           panel.spacing=unit(0,"cm"),
      ##           axis.title.x = element_blank())
      
} else {
}

MMP_checkData(name = "disturbance.reef.RData",
              stage = paste0("STAGE", CURRENT_STAGE),
              item = CURRENT_ITEM,
              label = "Processed timeseries plots",
              PATH = OTHER_OUTPUT_PATH)
MMP_openning_banner()

## ----end

