
set(evTools_std_files
  src/code_version.f90
  src/functions.f90
  )

# Get compiler name, add a file for g95:
get_filename_component( Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME )
if( Fortran_COMPILER_NAME MATCHES "g95" )
  set( evTools_std_files ${evTools_std_files} src/nagfor.f90 )  # Needed to compile with nagfor, g95, and F2003 standard - won't run!
endif( Fortran_COMPILER_NAME MATCHES "g95" )


set(evTools_plot_files
  src/plotfunctions.f90
  )

set(evTools_plt_files
  src/plt_functions.f90
  )

set(evTools_mdl_files
  src/mdl_functions.f90
  )

