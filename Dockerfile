FROM rocker/r-ver:4.6.1

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
  make \
  ghostscript \
  poppler-utils \
  zip \
  wget \
  fonts-dejavu-extra \
  libfontconfig1-dev \
  curl \
  tk \
  openjdk-11-jre \
  gdebi-core \
  cmake \
  libcairo2-dev \
  libuv1-dev \
  && rm -rf /var/lib/apt/lists/*


# NOTE: pandoc-citeproc was removed upstream; citeproc is now built into pandoc (use `pandoc --citeproc`).

#################################### XTIDE ##############################
# NOTE: xtide (and its libtcd dependency) were removed from the Ubuntu archive
#       (absent from 24.04 "noble"), so we build the `tide` CLI from source.
#       The project shells out to `tide` in MMP_functions.R / MMP_23_waterQuality_load_other.R.
#       Build-only deps are installed and purged in the same layer to keep the image small.
ARG LIBTCD_VERSION=2.2.7-r3
ARG XTIDE_VERSION=2.15.5
RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
        build-essential \
        ca-certificates \
        libpng-dev; \
    cd /tmp; \
    # --- libtcd (tide constituent database library) ---
    wget -q "https://flaterco.com/files/xtide/libtcd-${LIBTCD_VERSION}.tar.xz"; \
    tar xf "libtcd-${LIBTCD_VERSION}.tar.xz"; \
    cd libtcd-*/; \
    ./configure --prefix=/usr/local; \
    make -j"$(nproc)"; \
    make install; \
    cd /tmp; \
    # --- xtide (provides the `tide` command-line program) ---
    wget -q "https://flaterco.com/files/xtide/xtide-${XTIDE_VERSION}.tar.xz"; \
    tar xf "xtide-${XTIDE_VERSION}.tar.xz"; \
    cd xtide-*/; \
    ./configure --prefix=/usr/local --without-x; \
    make -j"$(nproc)"; \
    make install; \
    ldconfig; \
    # --- cleanup (keep the C/C++ toolchain: needed at runtime to compile
    #     R packages and Stan models via Rcpp/cmdstanr/brms) ---
    cd /; \
    rm -rf /tmp/libtcd-* /tmp/xtide-*; \
    apt-get purge -y libpng-dev; \
    apt-get autoremove -y; \
    rm -rf /var/lib/apt/lists/*
# At runtime the scripts set `export HFILE_PATH=../parameters/harmonics-2004-06-14.tcd`
# to point `tide` at the bundled harmonics database.

## Install quarto (version 1.10.19)
ARG QUARTO_VERSION="1.10.19"
RUN curl -o quarto-linux-amd64.deb -L https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb
RUN gdebi --non-interactive quarto-linux-amd64.deb

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
      libfontconfig1-dev \
      libharfbuzz-dev \
      libfribidi-dev \
  && rm -rf /var/lib/apt/lists/*

## Install General Usage R packages
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2026-10-01/')); \
  install.packages('textshaping'); \
  install.packages('ragg'); \
  install.packages('tidyverse'); \
  install.packages('jsonlite'); \
  install.packages('rlang'); \
  install.packages('cli'); \
  install.packages('Hmisc'); \
  install.packages('R.utils'); \
  install.packages('DT');   \
"
#############################################################################
## NOTE: we could opt for installing specific versions of packages
## RUN R -e "install.packages('remotes');"
## RUN R -e "remotes::install_version('dplyr', '1.0.5');"
#############################################################################

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
      libgdal-dev \
          gdal-bin \
              proj-bin \
                && rm -rf /var/lib/apt/lists/*

## Install Spatial R packages
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2026-10-01/')); \
    install.packages('sf'); \
"

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2026-10-01/')); \
    install.packages('sp'); \
    install.packages('rnaturalearth'); \
    install.packages('rnaturalearthdata'); \
    install.packages('ggnewscale'); \
    install.packages('rgeos'); \
    install.packages('sn'); \
    install.packages('stars'); \
    install.packages('geojsonR'); \
    install.packages('geojsonsf'); \
    install.packages('s2'); \
"

## Install ncdf4 R package (for netCDF file handling)
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2026-10-01/')); \
    install.packages('ncmeta'); \
    install.packages(c('ncdf4')); \
"

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libcairo2-dev \
  && rm -rf /var/lib/apt/lists/*


## Install quarto related R packages
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2026-10-01/')); \
    install.packages('quarto'); \
    install.packages('bookdown'); \
    install.packages(c('kableExtra')); \
    install.packages(c('officer')); \
    install.packages(c('flextable')); \
    install.packages(c('gdtools')); \
"

## Install flow related R packages
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2026-10-01/')); \
    install.packages('furrr'); \
    install.packages('foreach'); \
"

## Install unit testing related R packages
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2026-10-01/')); \
    install.packages('testthat'); \
"

## Install Modelling related R packages
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2026-10-01/')); \
    install.packages('emmeans'); \
    install.packages('DHARMa'); \
    install.packages(c('car')); \
    install.packages(c('mgcv')); \
"

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2026-10-01/')); \
    install.packages('remotes'); \
    install.packages('rstan');  \
    remotes::install_github('paul-buerkner/brms'); \
    install.packages('tidybayes');  \
    install.packages('cmdstanr', repos = c('https://mc-stan.org/r-packages/', getOption('repos'))); \
    remotes::install_github('stan-dev/cmdstanr'); \
    library(cmdstanr); \
    check_cmdstan_toolchain(); \
    install_cmdstan(cores = 2); \
"

## Install Bayesian modelling testing related R packages
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2026-10-01/')); \
    install.packages('tidybayes'); \
    install.packages('posterior'); \
    install.packages('bayesplot');   \
    install.packages('HDInterval');   \
"

## Install ggplot related R packages
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2026-10-01/')); \
    install.packages('patchwork'); \
    install.packages('ggnewscale'); \
    install.packages('ggh4x'); \
    install.packages(c('cowplot')); \
"

# ## Install INLA related R packages
# RUN R -e "options(repos = \
#   list(CRAN = 'https://packagemanager.posit.co/cran/2026-10-01/')); \
#   install.packages('inlabru'); \
#     ##install.packages('INLA', repos=c(getOption('repos'), INLA='https
#     ://inla.r-inla-download.org/R/stable')); \
# "

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2026-10-01/')); \
    install.packages(c('remotes')); \
    remotes::install_github('AIMS/reportcards'); \
"

## Install project specific R packages
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2026-10-01/')); \
    install.packages('openxlsx'); \
"

## Install project specific R packages
RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2026-10-01/')); \
    install.packages('assertthat'); \
"

## Create project directory in docker image
RUN mkdir ~/MMP

## Copy scripts and parameters (folders and contents) into docker image project directory
COPY scripts/ ~/MMP/scripts/
COPY scripts/ ~/MMP/parameters/
WORKDIR ~/MMP/scripts/

## Add default values for USER_ID and GROUP_ID
#ARG USER_ID=1000
#ARG GROUP_ID=1000

## Add a user with a specific UID and GID
#RUN groupadd -g ${GROUP_ID} tempgroup && \
#    useradd -m -u ${USER_ID} -g tempgroup tempuser
#RUN echo "USER_ID=${USER_ID}, GROUP_ID=${GROUP_ID}"

## Set the default user
#USER tempuser






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


