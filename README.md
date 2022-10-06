MMP Water Quality Analyses
=============================

# Docker instructions

1. to build an image from Dockerfile
   > docker build  ..
2. to run image interactively
   > docker run -it -rm <name> Rscript MMP_00_main.R --reportYear=2022 --runStage=1
3. to execute a container
   > docker exec <name> Rscript MMP_00_main.R --reportYear=2022 --runStage=1
4. list images / containers
5. delete image
6. clear cache

# Stages of analysis

- Stage 1
  - prepare paths
- Stage 2 (extracting data)
  - water quality
  - tides
  - BOM data
  - river discharge
  - report
- Stage 3 (processing data)
  - water quality
    - process
    - summaries
    - index
    - gams
    - ...
  - tides
  - BOM data
  - report
- Stage 4
  - report

# News
- 06/10/2022:  Start project
