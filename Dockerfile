## Get R version 4.2.1
FROM rocker/r-ver:4.2.1

## Get packages
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
  && rm -rf /var/lib/apt/lists/*

## Potential Issues
# pandoc-citeproc no longer maintained - are packages above 'time-capsuled' in the same way as R and R packages below?

## Install R package versions from MRAN (based on a date - YYYY-MM-DD)
RUN R -e "options(repos = \
    list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-05/'));\
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
"
## Issues:
# patchwork not available for this version of R. Install from remotes? Can we control for version/date using install_github?
# RUN R -e "install.packages('remotes');"
# RUN R -e "remotes::install_github('thomasp85/patchwork');"

#############################################################################
## Alternatively, we could opt for installing specific versions of packages
## RUN R -e "install.packages('remotes');"
## RUN R -e "remotes::install_version('dplyr', '1.0.5');"
##
## install INLA
# RUN  wget https://inla.r-inla-download.org/R/stable/src/contrib/INLA_21.02.23.tar.gz \
#   && R CMD INSTALL --clean --no-multiarch --without-keep.source --byte-compile --resave-data --compact-docs --no-demo INLA_21.02.23.tar.gz \
#   && rm INLA_21.02.23.tar.gz
#
# RUN apt-get update \
#   && apt-get install -y --no-install-recommends \
#     tk \
#   && rm -rf /var/lib/apt/lists/*
#
# Install AWS CLI
# curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
# unzip awscliv2.zip
# sudo ./aws/install
#
# RUN mkdir ~/reefCloud
# COPY scripts/ ~/reefCloud/scripts/
# COPY tests/ ~/reefCloud/tests/
# COPY Makefile ~/reefCloud/
# COPY docs/resources ~/reefCloud/resources
# RUN mkdir -p docs/Reports
# ##RUN cp docs/Reports/*.editME ~/reefCloud/docs/Reports
# COPY docs/Reports/*.editMe ~/reefCloud/docs/Reports/
# WORKDIR ~/reefCloud
# COPY stats_wrapper.sh ~/reefCloud/stats_wrapper.sh
# ENTRYPOINT ["~/reefCloud/stats_wrapper.sh"]
#############################################################################
