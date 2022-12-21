R_SRC_DIR  = $(addprefix , scripts)
ifdef FILE
R_FILE = $(R_SRC_DIR)/$(FILE)
else
R_FILE ?= $(foreach sdir, $(R_SRC_DIR), $(wildcard $(sdir)/*.R))
endif

$(info ************************************)
$(info R Source directory:     $(R_SRC_DIR))
$(info R Source files:         $(R_FILE))
$(info ************************************)

all: $(R_FILE)
	 @echo "Compiling analyses"
	 cd scripts; Rscript $(notdir $<)

singularity:
	@echo "Compiling analyses"
	cd scripts; srun -n 1 --nice=1 --mem=200GB --partition=cpuq --job-name=MMP --output=job.log --time=02:00:00 singularity exec -B .:/MMP ../mmp.sif Rscript MMP_00_main.R --reportYear=2022 --runStage=3 --alwaysExtract=FALSE

# singularity exec -B .:/MMP ../mmp.sif Rscript MMP_00_main.R --reportYear=2022 --runStage=3 --alwaysExtract=TRUE


# srun singularity exec -B .:/MMP ../mmp.sif Rscript MMP_00_main.R --reportYear=2022 --runStage=3 --alwaysExtract=TRUE
