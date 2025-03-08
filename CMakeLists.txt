cmake_minimum_required(VERSION 3.22)
if(POLICY CMP0135)
  cmake_policy(SET CMP0135 NEW)
endif()

project(
  PIC
  LANGUAGES Fortran
  VERSION 1.0
  DESCRIPTION "Jorge's cool stuff")

# Add the source directory where the actual Fortran sources are
set(core_lib pic)
add_library(${core_lib} STATIC)

# Set a default build type if not specified
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

# Enable position-independent code for shared-library linking support
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Make sure Fortran modules are handled properly
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)
message(STATUS "the big variable is ${CMAKE_Fortran_MODULE_DIRECTORY}")
# Ensure the target property is explicitly set
set_target_properties(${core_lib} PROPERTIES Fortran_MODULE_DIRECTORY
                                             ${CMAKE_Fortran_MODULE_DIRECTORY})

# Check if -DPIC_ENABLE_INT8 is passed and add compile definition SUBNORMAL
if(PIC_ENABLE_INT8)
  target_compile_definitions(${core_lib} PRIVATE SUBNORMAL)
endif()

# Print summary info
message(STATUS "Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "PIC_ENABLE_INT8: ${PIC_ENABLE_INT8}")

set(SKIP_DOC_GEN
    TRUE
    CACHE BOOL "Option to skip documentation generation")
include(ExternalProject)
set(json_fortran_version 9.0.1)
ExternalProject_Add(
  json-fortran
  URL https://github.com/jacobwilliams/json-fortran/archive/refs/tags/${json_fortran_version}.tar.gz
  PREFIX ${PROJECT_SOURCE_DIR}/external/json-fortran
  CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${PROJECT_SOURCE_DIR}/lib/
             -DSKIP_DOC_GEN=TRUE -DCMAKE_VERBOSE_MAKEFILE=OFF
  LOG_BUILD ON # Enable logging to capture build output
)
# testing framework, supported by the Fortran-lang github orgo
set(test-drive_version 0.4.0)
ExternalProject_Add(
  test-drive
  URL https://github.com/fortran-lang/test-drive/archive/refs/tags/v${test-drive_version}.tar.gz
  PREFIX ${PROJECT_SOURCE_DIR}/external/test-drive
  CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${PROJECT_SOURCE_DIR}/lib/test-drive)

add_subdirectory(src)

string(TOLOWER "${CMAKE_Fortran_COMPILER_ID}" FORTRAN_COMPILER_ID_LOWER)
target_link_libraries(
  ${core_lib}
  PRIVATE
    ${PROJECT_SOURCE_DIR}/lib/jsonfortran-${FORTRAN_COMPILER_ID_LOWER}-${json_fortran_version}/lib/libjsonfortran.a
    ${PROJECT_SOURCE_DIR}/lib/test-drive/lib/libtest-drive.a)
target_include_directories(
  ${core_lib}
  PRIVATE
    ${CMAKE_Fortran_MODULE_DIRECTORY}
    ${PROJECT_SOURCE_DIR}/lib/jsonfortran-${FORTRAN_COMPILER_ID_LOWER}-${json_fortran_version}/lib/
    ${PROJECT_SOURCE_DIR}/lib/test-drive/include/test-drive/${CMAKE_Fortran_COMPILER_ID}-${CMAKE_Fortran_COMPILER_VERSION}
)

add_dependencies(${core_lib} json-fortran test-drive)
# Install targets
install(
  TARGETS ${core_lib}
  LIBRARY DESTINATION lib
  RUNTIME DESTINATION bin)
# Install Fortran module files explicitly
install(
  DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/
  DESTINATION include
  FILES_MATCHING
  PATTERN "*.mod")

# Install json-fortran headers
install(
  DIRECTORY
    ${PROJECT_SOURCE_DIR}/lib/jsonfortran-${FORTRAN_COMPILER_ID_LOWER}-${json_fortran_version}/lib/
  DESTINATION include
  FILES_MATCHING
  PATTERN "*.mod")

# Install test-drive headers
install(
  DIRECTORY
    ${PROJECT_SOURCE_DIR}/lib/test-drive/include/test-drive/${CMAKE_Fortran_COMPILER_ID}-${CMAKE_Fortran_COMPILER_VERSION}
  DESTINATION include)

set(exe_name app)
add_executable(${exe_name} ${PROJECT_SOURCE_DIR}/app/main.f90)
target_include_directories(${exe_name} PRIVATE "${CMAKE_BINARY_DIR}/modules")

target_link_libraries(${exe_name} PRIVATE ${core_lib})
