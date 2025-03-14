cmake_minimum_required(VERSION 3.22)

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

# Check if -DPIC_ENABLE_INT8 is passed and add compile definition SUBNORMAL
if(PIC_ENABLE_INT8)
  target_compile_definitions(${core_lib} PRIVATE SUBNORMAL)
endif()

# Print summary info
message(STATUS "Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "PIC_ENABLE_INT8: ${PIC_ENABLE_INT8}")

add_subdirectory(src)

# Install targets
install(
  TARGETS ${core_lib}
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  RUNTIME DESTINATION bin)

# Install Fortran module files (assuming they are in the build directory)
install(
  DIRECTORY ${CMAKE_BINARY_DIR}/
  DESTINATION include
  FILES_MATCHING
  PATTERN "*.mod")
set(exe_name app)
add_executable(${exe_name} ${PROJECT_SOURCE_DIR}/app/main.f90)
target_include_directories(${exe_name} PRIVATE "${CMAKE_BINARY_DIR}/modules")

target_link_libraries(${exe_name} PRIVATE ${core_lib})
