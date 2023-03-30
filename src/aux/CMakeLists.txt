cmake_minimum_required(VERSION 3.5)


set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)



add_library( array_alloc   STATIC array_alloc.f90)
add_library( aux_dds       STATIC aux_dds.f90)







