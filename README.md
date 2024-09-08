MMP Water Quality Analyses
=============================

# Docker instructions

**Docker** is software which allows us to create a sort of timecapsuled running environment which is seperate from your specific computer's settings. Essentially it standardises the operating system, system settings and software versions to ensure that code runs as intended. 

Docker is run in multiple parts: the **dockerfile** details the setup for a specific running environment, which is called an **image**. Once an image is created we create a **container** from which we run our code. Think of the image as the timecapsuled computer, and the container as turning on the computer. More information, including installation and user guides can be found here https://docs.docker.com/.

**0. Ensure you have Docker installed and running on your computer.**
```console
docker --version
```
```console
# Example:
C:\> docker --version
  Docker version 20.10.17, build 100c701

# Here we see that Docker is installed on our computer and ready to go
```

**1. Navigate to the directory containing the Dockerfile (i.e. the `MMP_WQ` folder) from the command line.**

```console
ls                         # List current directory contents (Windows Powershell/Mac/Linux)
dir                        # List current directory contents (Windows cmd)
cd <path>                  # Navigate to specific folder via given path
cd <child_directory_name>  # Navigate to child folder (i.e. subfolder)
cd ..                      # Navigate to parent folder
```
```
# Example:
[user:2]$ cd home/user/Documents/Projects/MMP_WQ  # Mac and Linux (forward slash only)
C:\> cd Users\user\Documents\Projects\MMP_WQ  # Windows (forward or back slashes accepted)
C:\Users\user\Documents\Projects\MMP_WQ> ls
Mode                 LastWriteTime         Length Name
----                 -------------         ------ ----
d-----        18/10/2022   3:06 PM                data
d-----        18/10/2022   3:06 PM                docs
<...>
-a----         6/10/2022   3:23 PM           3113 Dockerfile
```

**2. Build Docker image from Dockerfile and `--tag` it with a name.**
```console
docker build . --tag <image_name>
```
```
# Example:
C:\Users\user\Documents\Projects\MMP_WQ> docker build . --tag mmp  # Windows
  [+] Building 4125.9s (13/13) FINISHED
  <...>

# Here we can see that it took XXXX (about 1 hour 8 minutes) to build our Docker image
# If this happens much quicker it is likely that Docker has cached the image from a previous build
# See step 4 for how to clear the cache
```

**3. Create a docker container using the `mmp` image, run the `MMP_00_main.R` script inside the container with the desired `reportYear` and `runStage`, and remove (`--rm`) the container when the script finishes.**

* `reportYear`: 
* `runStage`:
* `alwaysExtract`: Optional (default = TRUE). 

```console
docker run --rm <image_name> Rscript MMP_00_main.R --reportYear=<YYYY> --runStage=<START:STOP> --alwaysExtract=<TRUE/FALSE>
```
```
# Example:
C:\Users\user\Documents\Projects\MMP_WQ> docker run --rm mmp Rscript MMP_00_main.R --reportYear=2022 --runStage=1:3 --alwaysExtract=TRUE
```

**4. Build a singularity image (if intending to run on HPC).**  This
step essentially involves saving the docker image from its local
repository and then using this archive to build a singularity
(apptainer) image that can be run on the HPC.

**4.1. Build the singularity image:**

```console
docker save mmp -o mmp.tar
singularity build mmp.sif docker-archive://mmp.tar
```

**4.2. Clone the code base on to the HPC:** Ensure that you are logged
onto the HPC and have navigated to the folder in which the code
repository should be created.

```console
git clone git@github.com:open-AIMS/MMP_WQ.git
```

**4.3. Transfer this image over to the HPC:**
Ensure you are in the root of the project.

```console
scp mmp.sif <user>@hpc-l001.aims.gov.au:~/<path to project>
```
```
# Example
scp mmp.sif mlogan@hpc-l001.aims.gov.au:~/Work/AIMS/MMP/WQ/2023
```

**4.4. Run the singularity image on the HPC:** Ensure that you are
logged into the HPC and have navigated to the `scripts` folder of the
project.

```console
module load singularity
singularity exec -B .:/home/Project ../mmp.sif Rscript MMP_00_main.R --reportYear=2024 --alwaysExtract=TRUE --runStage=1
```

As of 2024, we need to also mount a NFS of the logger data

```console
module load singularity
singularity exec -B .:/home/Project -B /net/cluster1-prod-hpcnfs.aims.gov.au/rwqpp-field-data:/home/logger_data ../mmp.sif Rscript MMP_00_main.R --reportYear=2024 --alwaysExtract=TRUE --runStage=1
```

**4.5. Run an interactive R session from singularity (local or HPC)**

- if HPC must start with `module load singularity`

- in emacs, open shell (`M-x shell`)
- navigate to `scripts` folder
- run the following
```console
singularity exec -B .:/home/Project ../mmp.sif R 
```
- `M-x ess-remote`
- open the R script and start sending code (if necessary switch process)

As of 2024, we need to also mount a NFS of the logger data

```console
singularity exec -B .:/home/Project -B /net/cluster1-prod-hpcnfs.aims.gov.au/rwqpp-field-data:/home/logger_data ../mmp.sif R 
```


**5. Clean up (remove all docker containers, images, and cache).**
As we saw in Step 1, it can take quite a while to build the docker image. This is because the image is quite large (i.e. uses a lot of memory). Therefore, when we are completely finished with Docker, we should restore this memory. 

**5.1. Check for any docker containers:**
    
```console
docker ps --all
```
```
# Example:
C:\Users\user\Documents\Projects\MMP_WQ> docker ps --all
  CONTAINER ID  IMAGE COMMAND CREATED       STATUS             PORTS     NAMES
  dcf7f999a862  mmp   "R"     6 seconds ago Exited (0) 5 seconds ago     gracious_bhabha
```

**5.2. Remove any docker containers:**
  
```console
docker rm <container_ID>  # Note: you don't have to type out the full ID, just the first few characters
```
```
# Example:
C:\Users\user\Documents\Projects\MMP_WQ> docker rm dcf
  dcf
```

**5.3. Check for any docker images:**

```console
docker images --all
```
```
# Example:
C:\Users\user\Documents\Projects\MMP_WQ> docker images --all
  REPOSITORY  TAG    IMAGE ID     CREATED         SIZE
  mmp         latest bbef020a621a 40 minutes ago 2.79GB
```

**5.4. Remove any docker images:**
```console
docker rmi <image_name>
```
```
# Example:
C:\Users\bford\Documents\Projects\MMP_WQ> docker rmi mmp
  Untagged: mmp:latest
  Deleted: sha256:bbef020a621aca0601972f3a167facfd70a6fd3c0c20f2f4e8f332e7babe7111
```

**5.5. Clear build cache:**

```console
docker builder prune
```
```
Example:
C:\Users\bford\Documents\Projects\MMP_WQ> docker builder prune
  WARNING! This will remove all dangling build cache. Are you sure you want to continue? [y/N]
y
  Deleted build cache objects:
  r6qf6617yeo608arlsrtn22dn
  <...>
  Total reclaimed space: 1.905GB

# Here we can see that by cleaning up we restored 1.9 gigabytes of memory
```

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
  - package data
- Stage 4
  - report

# News
- 06/10/2022:  Start project
