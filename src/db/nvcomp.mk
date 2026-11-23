# nvCOMP (GPU OASIS CBLOCK decompression) detection, shared by db/Makefile and src/Makefile.nvf.
#
#   NVCOMP=1 / NVCOMP=0   force on / off (default: on when nvcomp.h and cuda_runtime.h are found)
#   NVCOMP_HOME           nvCOMP install (include/nvcomp.h, lib/libnvcomp.so)
#   CUDA_HOME             CUDA toolkit used for headers (and libcudart for the C++-only db tools)
#
# Exports:
#   NVCOMP_CXXFLAGS   -DLAYOUTDB_USE_NVCOMP + include paths (empty when disabled)
#   NVCOMP_LDLIBS     -lnvcomp with rpath (all consumers)
#   NVCOMP_CUDART     -lcudart with rpath (only for g++-linked tools; nvfortran -cuda brings its own)
NVCOMP_HOME ?= /home/skoranne/OSS_DOWNLOADS/nvcomp-linux-sbsa-5.2.0.10_cuda13-archive
CUDA_HOME   ?= /usr/local/cuda

ifeq ($(origin NVCOMP),undefined)
  ifneq ($(wildcard $(NVCOMP_HOME)/include/nvcomp.h),)
    ifneq ($(wildcard $(CUDA_HOME)/include/cuda_runtime.h),)
      NVCOMP := 1
    endif
  endif
endif

ifeq ($(NVCOMP),1)
  NVCOMP_CXXFLAGS := -DLAYOUTDB_USE_NVCOMP -I$(NVCOMP_HOME)/include -I$(CUDA_HOME)/include
  NVCOMP_LDLIBS   := -L$(NVCOMP_HOME)/lib -Wl,-rpath,$(NVCOMP_HOME)/lib -lnvcomp
  NVCOMP_CUDART   := -L$(CUDA_HOME)/lib64 -Wl,-rpath,$(CUDA_HOME)/lib64 -lcudart
else
  NVCOMP_CXXFLAGS :=
  NVCOMP_LDLIBS   :=
  NVCOMP_CUDART   :=
endif
