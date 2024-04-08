# CMake project file for OPD_chirality_index

cmake_minimum_required(VERSION 2.6)
project (opd)
enable_language (Fortran CXX)
cmake_policy(SET CMP0015 NEW)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fopenmp -Wall -g -O0 -ffpe-summary=none -fcheck=bounds -Wno-compare-reals -Wno-conversion -fbacktrace -fcheck=all -Wextra -no-pie -march=native" )
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
   set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -g -O3")
endif()

#
# Compile
#

set(CMAKE_VERBOSE_MAKEFILE on)
set(EXECUTABLE_OUTPUT_PATH "bin")
set(CMAKE_Fortran_MODULE_DIRECTORY "modules")

file(GLOB_RECURSE sources src/mod_*.f90 src/osipov_omp.f90)

add_executable(opd ${sources})

target_link_libraries(opd)
