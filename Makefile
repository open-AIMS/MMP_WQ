# help:
# 	@echo "Usage: make -i <type>                        -> to make item"
# 	@echo "       make -i build                         -> to build the docker image"
# 	@echo "       make -i run_stage_1_9 reportYear=2026 -> to run stages 1 though to 9"
#   @echo "          if reportYear is omitted, current year is assumed"

reportYear ?= $(shell date +%Y)

.PHONY: build run code docs

build:
	docker build . --tag mmp2 --progress=plain 2>&1 | tee build.log

build_singularity2:
	docker save mmp2 -o mmp2.tar
	singularity build mmp2.sif docker-archive://mmp2.tar

build_singularity:
	docker save mmp -o mmp.tar
	singularity build mmp.sif docker-archive://mmp.tar

# Run interactive R session in docker container
R_container:
	docker run --rm -it -v "$(shell pwd)":/home/Project mmp R

code_container:
	docker run --rm -v "$(shell pwd)":/home/Project mmp $(MAKE) -f scripts/Makefile

docs_container:
	docker run --rm -v "$(shell pwd)":/home/Project mmp $(MAKE) -f docs/Makefile

# docker run --rm -v "$(pwd):/home/Project" mmp $(MAKE) -f docs/Makefile
code_singularity:
	$(MAKE) -f scripts/Makefile singularity

slurm_R:
	@echo "Compiling analyses"
	sbatch analysis.slurm

run_R:
	@echo "Running the script on the HPC"
	cd scripts; Rscript MMP_40_fit_gps_hpc.R --reportYear=2026 --runStage=4 --alwaysExtract=FALSE --clearDirs=FALSE --reportOnly=FALSE

code_local:
	$(MAKE) -f scripts/Makefile

docs_local:
	$(MAKE) -f docs/Makefile

singularity_stage_1:
	rm -f data/reports/*.*
	#module load singularity
	singularity exec -B .:/home/Project mmp2.sif Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=1 --alwaysExtract=FALSE --clearDirs=FALSE

singularity_stage_2:
	# rm -f data/reports/*.*
	#module load singularity
	singularity exec -B .:/home/Project mmp2.sif Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=2 --alwaysExtract=TRUE --clearDirs=FALSE

singularity_stage_3:
	# rm -f data/reports/*.*
	#module load singularity
	singularity exec -B .:/home/Project mmp2.sif Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=3 --alwaysExtract=TRUE --clearDirs=FALSE

singularity_stage_3_4:
	rm -f data/reports/*.*
	#module load singularity
	singularity exec -B .:/home/Project mmp2.sif Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=3:4 --alwaysExtract=TRUE --clearDirs=FALSE

singularity_stage_4:
	# rm -f data/reports/*.*
	#module load singularity
	singularity exec -B .:/home/Project mmp2.sif Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=4 --alwaysExtract=FALSE --clearDirs=FALSE

singularity_stage_5:
	rm -f data/reports/STAGE5.*
	#module load singularity
	singularity exec -B .:/home/Project mmp2.sif Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=5 --alwaysExtract=TRUE --clearDirs=FALSE

singularity_stage_3_5:
	rm -f data/reports/*.*
	#module load singularity
	singularity exec -B .:/home/Project mmp2.sif Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=3:5 --alwaysExtract=TRUE --clearDirs=FALSE

singularity_stage_6:
	# rm -f data/reports/*.*
	#module load singularity
	singularity exec -B .:/home/Project mmp2.sif Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=6 --alwaysExtract=TRUE --clearDirs=FALSE --reportOnly=FALSE

singularity_stage_3_6:
	rm -f data/reports/*.*
	#module load singularity
	singularity exec -B .:/home/Project mmp2.sif Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=3:6 --alwaysExtract=TRUE --clearDirs=FALSE

singularity_stage_7:
	# rm -f data/reports/*.*
	#module load singularity
	singularity exec -B .:/home/Project mmp2.sif Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=7 --alwaysExtract=TRUE --clearDirs=FALSE --reportOnly=FALSE

singularity_stage_8:
	# rm -f data/reports/*.*
	#module load singularity
	singularity exec -B .:/home/Project mmp2.sif Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=8 --alwaysExtract=TRUE --clearDirs=FALSE --reportOnly=FALSE

singularity_stage_9:
	# rm -f data/reports/*.*
	#module load singularity
	singularity exec -B .:/home/Project mmp2.sif Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=9 --alwaysExtract=TRUE --clearDirs=FALSE --reportOnly=FALSE

run_stage_2:
	rm -f data/reports/*.*
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=2 --alwaysExtract=TRUE --clearDirs=TRUE

run_stage_3:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=3 --alwaysExtract=TRUE

run_stage_4:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=4 --alwaysExtract=TRUE

run_stage_5:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=5 --alwaysExtract=TRUE

run_stage_6:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=6 --alwaysExtract=TRUE

run_stage_7:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=7 --alwaysExtract=TRUE

run_stage_8:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=8 --alwaysExtract=TRUE

run_stage_9:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=9 --alwaysExtract=TRUE

run_stage_1_6:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2026 --runStage=1:6 --alwaysExtract=TRUE

run_stage_1_9:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=$(reportYear) --runStage=1:9 --alwaysExtract=TRUE

clean:
	rm -f *.log *.aux *.md *.out texput.log
