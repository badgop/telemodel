cmake_minimum_required(VERSION 3.5)

project(ttlib)
enable_language(Fortran)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(dialect "-ffree-form -std=f2008 -fimplicit-none")
    set(bounds "-fbounds-check")
    set(errors "-pedantic-errors ")
    set(Fortran_COMPILER_FLAGS "-w")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -Wextra -g -O -Wall -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -ffree-line-length-0 -fcheck=all -finit-real=nan -ffpe-trap=zero,invalid,overflow,underflow")
    set(CMAKE_BUILD_TYPE DEBUG)
endif()



# set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
# set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

add_subdirectory(aux)
add_subdirectory(classes)
add_subdirectory(tests)
add_subdirectory(general)



add_executable( test_sample_class  tests/test_sample_class.f90)
add_executable( test_delay tests/test_delay.f90)
add_executable( test_comb tests/test_comb.f90)
add_executable( test_integrator tests/test_integrator.f90)
# add_executable( test_counter test_counter.f90)
add_executable( test_read_write  tests/test_read_write.f90)
add_executable( test_sample_writer  tests/test_sample_writer.f90)
add_executable( test_sample_reader  tests/test_sample_reader.f90)
add_executable( test_dds  tests/test_dds.f90)

add_executable( test_cic_interpolator  tests/test_cic_interp.f90)
# add_executable( test_impulse_generator  test_impulse_generator.f90)


target_link_libraries( test_sample_class                sample)
target_link_libraries( test_delay         delay         sample)
target_link_libraries( test_comb          comb          sample)
target_link_libraries( test_integrator    integrator    sample)
# target_link_libraries( test_counter       counter       sample      )
target_link_libraries( test_read_write    module_read_write         )
target_link_libraries( test_sample_writer file_writer_class  module_read_write sample )
target_link_libraries( test_sample_reader file_reader_class  module_read_write sample )
target_link_libraries( test_dds           aux_dds    dds   file_writer_class     sample )

target_link_libraries( test_cic_interpolator    dds   aux_dds cic_interp  file_writer_class     sample )


# target_link_libraries( test_impulse_generator   impulse_generator         file_writer_class   counter   sample )

# set_target_properties ( test_sample_class PROPERTIES LINKER_LANGUAGE Fortran)