cmake_minimum_required(VERSION 3.5)


set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)


add_library(base_block        STATIC base_block_class.f90)
add_library(sample       SHARED sample.f90)
add_library(delay        SHARED delay.f90)
add_library(comb         SHARED comb.f90)
add_library(integrator   SHARED integrator.f90)

add_library(dds             SHARED dds.f90)

add_library(cic_interp   SHARED cic_interp.f90)



# add_library(counter      SHARED counter.f90)

add_library(aux_file_class     SHARED aux_file_class.f90)
add_library(file_writer_class SHARED file_writer_class.f90)
add_library(file_reader_class SHARED file_reader_class.f90)
# add_library( impulse_generator SHARED  impulse_generator.f90)


add_dependencies( base_block                      sample  )
add_dependencies( delay                      sample  )
add_dependencies( comb               delay   sample  )
add_dependencies( integrator         delay   sample )
add_dependencies( cic_interp         delay   sample comb integrator )


# add_dependencies( counter                     sample )

add_dependencies( dds               sample integrator)
add_dependencies( aux_file_class      array_alloc   )
add_dependencies( file_writer_class      aux_file_class   )
add_dependencies( file_reader_class      aux_file_class   )
# add_dependencies( impulse_generator     sample counter   )



target_link_libraries(base_block                  sample )
target_link_libraries(delay                  sample  base_block)

target_link_libraries(comb           delay   sample base_block)
target_link_libraries(integrator     delay   sample base_block)

target_link_libraries( dds           sample   integrator  base_block)
target_link_libraries(cic_interp     delay   sample comb integrator base_block)

# target_link_libraries( counter                     sample )

target_link_libraries(aux_file_class  array_alloc )
target_link_libraries(file_writer_class  aux_file_class )
target_link_libraries(file_reader_class  aux_file_class )
# target_link_libraries(impulse_generator     sample counter  )








