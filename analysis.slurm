#!/bin/bash
#SBATCH --job-name=mmp
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=60
#SBATCH --partition=mlogan
#SBATCH --mem=100GB
#SBATCH --output=analysis_%j.log
#SBATCH --mail-user=m.logan@aims.gov.au
#SBATCH --mail-type=all

module load singularity
singularity exec -B .:/home/Project mmp2.sif make run_R




