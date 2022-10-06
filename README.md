MMP Water Quality Analyses
=============================

# Docker instructions

Docker stores specific system settings and software versions to ensure the analysis runs smoothly. Ensure you have docker installed and running on your machine.

0. Navigate to the directory containing the Dockerfile (i.e. the MMP_WQ folder) from the command line.
    > dir         --> List current directory contents (Windows)
    > ls          --> List current directory contents (Linux/Mac)
    > cd <path>   --> Navigate to specific folder via given path
    > cd <child_directory_name>   --> Navigate to child folder (i.e. subfolder)
    E.g.  C:\> cd .\Users\user\Documents\Projects\MMP_WQ\
    Note: paths use either \ (Windows) or / (Linux and Mac)

1. Build docker image from Dockerfile.
    > docker build . --tag <image_name>
    E.g.  C:\Users\user\Documents\Projects\MMP_WQ> docker build . --tag mmp

2. Run main script from inside docker container and remove (--rm) the docker container when script finishes.
    > docker run --rm <image_name> Rscript MMP_00_main.R --reportYear=YYYY --runStage=X
    E.g.  C:\Users\user\Documents\Projects\MMP_WQ> docker run --rm mmp Rscript MMP_00_main.R --reportYear=2022 --runStage=1

3. Clean up: Remove all docker containers, images, and cache.

    - Check for any docker containers:
    > docker ps --all
    E.g.  C:\Users\user\Documents\Projects\MMP_WQ> docker ps --all
            CONTAINER ID  IMAGE COMMAND CREATED       STATUS             PORTS     NAMES
            dcf7f999a862  mmp   "R"     6 seconds ago Exited (0) 5 seconds ago     gracious_bhabha

    - Remove any docker containers:
    > docker rm <container_ID>
    E.g.  C:\Users\user\Documents\Projects\MMP_WQ> docker rm dcf
            dcf
    Note: you don't have to type out the full ID, just the first few characters

    - Check for any docker images:
    > docker images --all
    E.g.  C:\Users\user\Documents\Projects\MMP_WQ> docker images --all
            REPOSITORY  TAG    IMAGE ID     CREATED         SIZE
            mmp         latest bbef020a621a 40 minutes ago 2.79GB

    - Remove any docker images:
    > docker rmi <image_name>
    E.g.  C:\Users\bford\Documents\Projects\MMP_WQ> docker rmi mmp
            Untagged: mmp:latest
            Deleted: sha256:bbef020a621aca0601972f3a167facfd70a6fd3c0c20f2f4e8f332e7babe7111

    - Clear build cache
    > docker builder prune
    E.g.  C:\Users\bford\Documents\Projects\MMP_WQ> docker builder prune
            WARN1NG! This will remove all dangling build cache. Are you sure you want to continue? [y/N]
          y
            Deleted build cache objects:
            r6qf6617yeo608arlsrtn22dn
            ...
            Total reclaimed space: 1.905GB


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
