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
