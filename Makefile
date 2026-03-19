# help:
# 	@echo "Usage: make -i <type>                        -> to make item"
# 	@echo "       make -i build                         -> to build the docker image"
# 	@echo "       make -i run_stage_1_9 reportYear=2026 -> to run stages 1 though to 9"
#   @echo "          if reportYear is omitted, current year is assumed"

reportYear ?= $(shell date +%Y)

.PHONY: build run code docs

build:
	docker build . --tag mmp

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

code_local:
	$(MAKE) -f scripts/Makefile

docs_local:
	$(MAKE) -f docs/Makefile

run_stage_2:
	rm -f data/reports/*.*
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2025 --runStage=2 --alwaysExtract=TRUE

run_stage_3:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2025 --runStage=3 --alwaysExtract=TRUE

run_stage_4:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2025 --runStage=4 --alwaysExtract=TRUE

run_stage_5:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2025 --runStage=5 --alwaysExtract=TRUE

run_stage_6:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2025 --runStage=6 --alwaysExtract=TRUE

run_stage_7:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2025 --runStage=7 --alwaysExtract=TRUE

run_stage_8:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2025 --runStage=8 --alwaysExtract=TRUE

run_stage_9:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2025 --runStage=9 --alwaysExtract=TRUE

run_stage_1_6:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=2025 --runStage=1:6 --alwaysExtract=TRUE

run_stage_1_9:
	docker run --workdir /home/Project --rm -it -v .:/home/Project mmp Rscript scripts/MMP_00_main.R --reportYear=$(reportYear) --runStage=1:9 --alwaysExtract=TRUE

clean:
	rm -f *.log *.aux *.md *.out texput.log
