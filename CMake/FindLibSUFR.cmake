## Adapted from: FindLibSUFR.cmake
## by AF on 27/12/2010


# - Check for the presence of the LibSUFR library
#
# Defines the following variables:
#  HAVE_LibSUFR        = 
#  LibSUFR_INCLUDES    = Path to the LibSUFR header files
#  LibSUFR_LIBRARIES   = Path to all parts of the LibSUFR library
#  LibSUFR_LIBRARY_DIR = Path to the directory containing the LibSUFR libraries

## -----------------------------------------------------------------------------
## Standard locations where to look for required components

include (CMakeSettings)

## -----------------------------------------------------------------------------
## Check for the header files

FIND_PATH (LibSUFR_INCLUDES sufr_constants.mod
  PATHS ${include_locations} ${include_locations}/libSUFR ${include_locations}/libSUFR/${Fortran_COMPILER_NAME} ${lib_locations}
  PATH_SUFFIXES libSUFR
  )

## -----------------------------------------------------------------------------
## Check for the library

set (LibSUFR_LIBRARIES "")


find_library (LibSUFR_LIBRARY
  NAMES SUFR SUFR_${Fortran_COMPILER_NAME}
  PATHS ${lib_locations} ${lib_locations}/libSUFR ${lib_locations}/libSUFR_${Fortran_COMPILER_NAME}
  PATH_SUFFIXES libSUFR
  NO_DEFAULT_PATH
  )

if (LibSUFR_LIBRARY)
  list (APPEND LibSUFR_LIBRARIES ${LibSUFR_LIBRARY})
  get_filename_component(LibSUFR_LIBRARY_DIR ${LibSUFR_LIBRARY} PATH)
else (LibSUFR_LIBRARY)
  message (STATUS "Warning: Unable to locate LibSUFR!")
endif (LibSUFR_LIBRARY)


IF (LibSUFR_INCLUDES AND LibSUFR_LIBRARIES)
  SET (HAVE_LibSUFR TRUE)
ELSE (LibSUFR_INCLUDES AND LibSUFR_LIBRARIES)
  SET (HAVE_LibSUFR FALSE)
  IF (NOT LibSUFR_FIND_QUIETLY)
    IF (NOT LibSUFR_INCLUDES)
      MESSAGE (STATUS "Unable to find LibSUFR header files!")
    ENDIF (NOT LibSUFR_INCLUDES)
    IF (NOT LibSUFR_LIBRARIES)
      MESSAGE (STATUS "Unable to find LibSUFR library files!")
    ENDIF (NOT LibSUFR_LIBRARIES)
  ENDIF (NOT LibSUFR_FIND_QUIETLY)
ENDIF (LibSUFR_INCLUDES AND LibSUFR_LIBRARIES)

if (HAVE_LibSUFR)
  if (NOT LibSUFR_FIND_QUIETLY)
    message (STATUS "Found components for LibSUFR:")
    message (STATUS "LibSUFR_INCLUDES  = ${LibSUFR_INCLUDES}")
    message (STATUS "LibSUFR_LIBRARIES = ${LibSUFR_LIBRARIES}")
    #message ("")
  endif (NOT LibSUFR_FIND_QUIETLY)
else (HAVE_LibSUFR)
  if (LibSUFR_FIND_REQUIRED)
    message (FATAL_ERROR "Could not find LibSUFR!")
  endif (LibSUFR_FIND_REQUIRED)
endif (HAVE_LibSUFR)

## -----------------------------------------------------------------------------
## Mark as advanced ...

mark_as_advanced (
  LibSUFR_INCLUDES
  LibSUFR_LIBRARIES
  LibSUFR_LIBRARY
  LibSUFR_LIBRARY_DIR
  )
