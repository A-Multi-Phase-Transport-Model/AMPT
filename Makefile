F77 = gfortran -ffree-line-length-0

SRC_PATH   := ./src
SRCS       += $(wildcard $(SRC_PATH)/*.f90)

all: clean build

.PHONY:build
build:
	@$(F77) $(SRCS) -o ampt

.PHONY:clean
clean:
	@$(RM) ampt
