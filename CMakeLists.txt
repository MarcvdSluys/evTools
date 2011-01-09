##
##  CMakeFile for evTools
##  AF, 07/01/2011
##
##  To compile, from the directory that contains this file, do:
##    $ mkdir build; cd build
##    $ cmake ..
##    $ make
##


cmake_minimum_required(VERSION 2.6 FATAL_ERROR)

# Set build type. Do this *before* we set the project name:
if( NOT CMAKE_BUILD_TYPE )
  set( CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING
    "Choose the type of build, options are: None Debug Release RelWithDebInfo Profile."
    FORCE)
endif( NOT CMAKE_BUILD_TYPE )
set( CMAKE_CONFIGURATION_TYPES "${CMAKE_BUILD_TYPE}" CACHE INTERNAL "internal" )


# Project name and language:
project( evTools Fortran )


# Search in the CMake/ directory for CMake modules:
list( APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMake )


# Various compile/optimisation options that we may want to enable:
include( SetCompileOptions )


# Place the products in their directories:
get_filename_component( Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME )
#set( MODULE_DIRECTORY "${CMAKE_SOURCE_DIR}/usr/include/libTheSky/${Fortran_COMPILER_NAME}" )
#set( LIBRARY_OUTPUT_PATH "${CMAKE_SOURCE_DIR}/usr/lib" )
set( MODULE_DIRECTORY "${CMAKE_SOURCE_DIR}/mod" )
set( LIBRARY_OUTPUT_PATH "${CMAKE_SOURCE_DIR}/lib" )




# Find dependencies:
find_package( PGPLOT REQUIRED )
find_package( LibSUFR REQUIRED )

#include_directories( ${PGPLOT_INCLUDE_DIRS} )
#include_directories( ${LibSUFR_INCLUDE_DIRS} )
#set( Fortran_INCLUDE_OPTIONS "-I${LibSUFR_INCLUDES} -I${PGPLOT_INCLUDES}" )
#set( INCLUDE_FLAGS "-I${LibSUFR_INCLUDES} -I${PGPLOT_INCLUDES}" )  # transferred to CompilerFlags
set( INCLUDE_FLAGS "-I${LibSUFR_INCLUDES}" )  # transferred to CompilerFlags; no PGPlot modules used



# Set source files:
include( FileList )

# Set FORTRAN compiler flags:
include( CompilerFlags )



# Create executable targets that don't need PGPlot:
add_executable( dat2plt  src/dat2plt.f90 ${evTools_std_files} )
add_executable( findplt  src/findplt.f90 ${evTools_std_files} )
add_executable( getgrid  src/getgrid.f90 ${evTools_std_files} )
add_executable( grid  src/grid.f90 ${evTools_std_files} )
add_executable( listmdl  src/listmdl.f90 ${evTools_std_files} ${evTools_mdl_files} )
add_executable( listmod  src/listmod.f90 ${evTools_std_files} )
add_executable( listplt  src/listplt.f90 ${evTools_std_files} )
add_executable( makerun  src/makerun.f90 ${evTools_std_files} )
add_executable( mergeplt  src/mergeplt.f90 ${evTools_std_files} )
add_executable( plt2ce  src/plt2ce.f90 ${evTools_std_files} )
add_executable( plt2dat  src/plt2dat.f90 ${evTools_std_files} )
add_executable( plt2obs  src/plt2obs.f90 ${evTools_std_files} )
add_executable( selplt  src/selplt.f90 ${evTools_std_files} ${evTools_plt_files} )

# Set module directory:
set_target_properties( dat2plt findplt getgrid grid listmdl listmod listplt makerun mergeplt plt2ce plt2dat plt2obs selplt  PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY} )



# Create executable targets that need PGPlot:
add_executable( plotmdl  src/plotmdl.f90 ${evTools_std_files} ${evTools_mdl_files} ${evTools_plot_files} )
add_executable( plotmdln  src/plotmdln.f90 ${evTools_std_files} )
add_executable( plotmod  src/plotmod.f90 ${evTools_std_files} )
add_executable( plotplt  src/plotplt.f90 ${evTools_std_files} ${evTools_plt_files} ${evTools_plot_files} )
add_executable( plotpltn  src/plotpltn.f90 ${evTools_std_files} ${evTools_plt_files} ${evTools_plot_files} )
add_executable( plotpltp  src/plotpltp.f90 ${evTools_std_files} )

# Set module directory:
set_target_properties( plotmdl plotmdln plotmod plotplt plotpltn plotpltp  PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY} )

# Link against PGPlot:
target_link_libraries( plotmdl ${PGPLOT_LIBRARIES} )
target_link_libraries( plotmdln ${PGPLOT_LIBRARIES} )
target_link_libraries( plotmod ${PGPLOT_LIBRARIES} )
target_link_libraries( plotplt ${PGPLOT_LIBRARIES} )
target_link_libraries( plotpltn ${PGPLOT_LIBRARIES} )
target_link_libraries( plotpltp ${PGPLOT_LIBRARIES} )



#add_executable( map  src/map.f90 )
#target_link_libraries( map ${LibSUFR_LIBRARIES} ${PGPLOT_LIBRARIES} "libTheSky_shared" "libTheSkyMap_shared" )
#set_target_properties( map PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY} )




# Install to destination:
#   use DIRECTORY rather than TARGETS to allow include/<fortrancompiler>/
#install( DIRECTORY usr/
#  DESTINATION ${CMAKE_INSTALL_PREFIX})


