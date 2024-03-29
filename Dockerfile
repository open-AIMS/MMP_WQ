## Get R version 4.2.1
FROM rocker/r-ver:4.2.1

## Install packages
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    lbzip2 \
    libnetcdf-dev \
    libsqlite3-dev \
    libssl-dev \
    libudunits2-dev \
    netcdf-bin \
    sqlite3 \
    imagemagick \
    pandoc \
    pandoc-citeproc \
    make \
    ghostscript \
    poppler-utils \
    zip \
    wget \
    fonts-dejavu-extra \
    curl \
    tk \
    xtide \
    openjdk-11-jre \
    gdebi-core \
  && rm -rf /var/lib/apt/lists/*

# NOTE: pandoc-citeproc no longer maintained - are packages above 'time-capsuled' in the same way as R and R packages below?

## Install quarto (version 1.2.269)
ARG QUARTO_VERSION="1.2.269"
RUN curl -o quarto-linux-amd64.deb -L https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb
RUN gdebi --non-interactive quarto-linux-amd64.deb

## Install R package versions from MRAN (based on a date - YYYY-MM-DD)
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2022-10-11/')); \
  install.packages('tidyverse'); \
  install.packages('sf'); \
  install.packages('sp'); \
  install.packages('foreach'); \
  install.packages('testthat'); \
  install.packages('tidybayes'); \
  install.packages('jsonlite'); \
  install.packages('rlang'); \
  install.packages('bookdown'); \
  install.packages('rnaturalearth'); \
  install.packages('rnaturalearthdata'); \
  install.packages('patchwork'); \
  install.packages('ggnewscale'); \
  install.packages('tidybayes'); \
  install.packages('rgeos'); \
  install.packages('sn'); \
  install.packages('tidybayes'); \
  install.packages('inlabru'); \
  install.packages('cli'); \
  install.packages('Hmisc'); \
  install.packages('ncmeta'); \
  install.packages('stars'); \
  install.packages('geojsonR'); \
  install.packages('geojsonsf'); \
  install.packages('s2'); \
  install.packages('R.utils'); \
  install.packages('quarto'); \
  install.packages('ggh4x'); \
  install.packages('furrr'); \
  install.packages(c('kableExtra')); \
"
#############################################################################
## NOTE: we could opt for installing specific versions of packages
## RUN R -e "install.packages('remotes');"
## RUN R -e "remotes::install_version('dplyr', '1.0.5');"
#############################################################################

## Install INLA
RUN  wget https://inla.r-inla-download.org/R/stable/src/contrib/INLA_21.02.23.tar.gz \
  && R CMD INSTALL --clean --no-multiarch --without-keep.source --byte-compile --resave-data --compact-docs --no-demo INLA_21.02.23.tar.gz \
  && rm INLA_21.02.23.tar.gz

## Install quarto
    
ARG QUARTO_VERSION="1.2.269"
RUN curl -o quarto-linux-amd64.deb -L https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb
RUN gdebi --non-interactive quarto-linux-amd64.deb

RUN R -e "options(repos = \
    list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-04/'));\
  install.packages('quarto'); \
"

RUN R -e "options(repos = \
    list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-04/'));\
   install.packages('furrr'); \
"

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2022-10-11/')); \
  install.packages(c('kableExtra'))"  

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2022-10-11/')); \
  install.packages(c('remotes')); \
  remotes::install_github('AIMS/reportcards')"  

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2022-10-11/')); \
  install.packages(c('emmeans')); \
  install.packages(c('openxlsx')); \
  install.packages(c('cowplot'))"  

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2022-10-11/')); \
  install.packages(c('gdtools')); \
  install.packages(c('officer')); \
  install.packages(c('mgcv')); \
  install.packages(c('flextable'))"  

## Create project directory in docker image
RUN mkdir ~/MMP

## Copy scripts and parameters (folders and contents) into docker image project directory
COPY scripts/ ~/MMP/scripts/
COPY scripts/ ~/MMP/parameters/
WORKDIR ~/MMP/scripts/






#############################################################################
## COPY tests/ ~/reefCloud/tests/
## COPY Makefile ~/reefCloud/
## COPY docs/resources ~/reefCloud/resources
## RUN mkdir -p docs/Reports
## ##RUN cp docs/Reports/*.editME ~/reefCloud/docs/Reports
## COPY docs/Reports/*.editMe ~/reefCloud/docs/Reports/
# WORKDIR ~/MMP
## COPY stats_wrapper.sh ~/reefCloud/stats_wrapper.sh
## ENTRYPOINT ["~/reefCloud/stats_wrapper.sh"]
#############################################################################
