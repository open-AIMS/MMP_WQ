# help:
# 	@echo "Usage: make -i SRC=<path/file> -> to make a specific file"
# 	@echo "       make -i                 -> to make all altered files"

.PHONY: build run code docs

build:
	docker build . --tag mmp

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

clean:
	rm -f *.log *.aux *.md *.out texput.log
