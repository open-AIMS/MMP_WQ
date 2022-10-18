
mmp__logBox <- function(box.width, box.margins) {
    log <- system(paste0("tail -n 5 ", LOG_FILE), intern = TRUE)
    pos <- max(stringr::str_locate(log, "\\|[^| ]*:\ ")[,"end"])
    log <- cli::ansi_strwrap(log,width = 80, exdent = pos)
    log.text <- paste0(
        cli::ansi_align(
                 paste0(
                     "\u2551",
                     strrep(" ", box.margins),
                     log),
                 width = box.width + box.margins*2,
                 align = "left"),
        "\u2551\n")
    log.text <- c("",log.text,
                  paste0("\u255A", strrep("\u2550", box.width + 1), "\u255D\n")
                  )
   log.text 
}

mmp__outerBox.top <- function(outer.box.width, this.box.width) {
    top <- paste0("\u2554",
                  strrep("\u2550", this.box.width),
                  "\u2564",
                  strrep("\u2550", outer.box.width - this.box.width),
                  "\u2557",
                  "\n"
                  )
    top
}

mmp__outerBox.bottom <- function(outer.box.width, this.box.width) {
    bottom <- paste0("\u2560",
                     strrep("\u2550", this.box.width),
                     '\u2567',
                     strrep("\u2550", outer.box.width - this.box.width),
                     "\u2563",
                     "\n"
                     )
    bottom
}


mmp__settingsBox <- function(settings, box.width, box.nchar, box.margins, currentTime) {
    box.text <- NULL
    keys <- settings$names
    values <- sapply(settings$items, function(x) eval(parse(text = x)))
    status <- settings$status
    for (i in 1:length(keys)) {
        box.text <- c(box.text,
                           paste0("\u2551",
                                  strrep(" ", box.margins),
                                  switch(status[i],
                                         'pending' = crayon::white(cli::symbol$line),
                                         'success' = crayon::green(cli::symbol$tick),
                                         'failure' = crayon::red(cli::symbol$cross)
                                         ),
                                  " ", crayon::blue(keys[i]), ": ",
                                  crayon::white(values[i]),
                                  strrep(" ", box.width - (box.nchar[i])-box.margins*2 -1),
                                  "\u2502",
                                  strrep(" ", box.margins)
                                  )
                           )
    }
    box.text
}

mmp__mainBox <- function(settings.box.text, box.width, settings.box.width, box.margins) {
    main.box.text <- c("MMP Water Quality Report Analysis", "")
    ## format the title to be centered
    for (i in 1:length(main.box.text))
        main.box.text[i] <- cli::ansi_align(main.box.text[i],
                                            width = box.width - settings.box.width - 1,
                                            align = 'center')

    ## add the stages as left justified 
    for (j in 1:length(runStage)) {
            main.box.text <- c(main.box.text,
                               cli::ansi_align(STATUS[[paste0("STAGE",runStage[j])]]$title,
                                               width = box.width - settings.box.width - 1,
                                               align = 'left')
                               )
            for (i in 1:length(STATUS[[paste0("STAGE",runStage[j])]]$items)) {
                if (runStage[j] == CURRENT_STAGE | STATUS[[paste0("STAGE",runStage[j])]]$status[i] == 'failure') {
                    main.box.text <- c(main.box.text,
                                       cli::ansi_align(
                                                paste0(strrep(" ", box.margins),      
                                                       switch(STATUS[[paste0("STAGE",runStage[j])]]$status[i],
                                                              'pending' = crayon::white(cli::symbol$line),
                                                              'progress' = crayon::magenta("\u23F1"),
                                                              'success' = crayon::green(cli::symbol$tick),
                                                              'failure' = crayon::red(cli::symbol$cross)
                                                              ),
                                                       " ", crayon::blue(STATUS[[paste0("STAGE",runStage[j])]]$name[i])
                                                       ),
                                                width = box.width - settings.box.width - 1,
                                                align = 'left'
                                            )
                                       )
                }
                
        } 
    }
    main.box.nchar <- nchar(main.box.text)
    main.box.text
}

mmp__combinedBoxes <- function(top,settings.box.text, main.box.text, bottom, box.width, settings.box.width, box.margins) {
    combined.text <- NULL
    for (i in 1:max(length(settings.box.text), length(main.box.text))) {
        combined.text <- c(combined.text,
                           paste0(
                               ifelse(i>length(settings.box.text),
                                      paste0("\u2551",
                                             cli::ansi_align("", width = settings.box.width, align = 'center'),
                                             "\u2502",
                                             strrep(" ", box.margins)), 
                                      settings.box.text[i]),
                               ifelse(i>length(main.box.text),
                                      cli::ansi_align("", width = box.width - settings.box.width - 1, align = 'center'),
                                      main.box.text[i]),
                               "\u2551",
                               "\n"))
        }
    combined.text <- c(top,combined.text,bottom)
}
