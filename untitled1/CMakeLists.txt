cmake_minimum_required(VERSION 3.10)
project(proj)

set(CMAKE_C_STANDARD 17)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Include directories
include_directories(
        ${PROJECT_SOURCE_DIR}/include/
)

# Find all source files
file(GLOB_RECURSE SOURCES
        "${PROJECT_SOURCE_DIR}/src/*.c"
)

# Add executable
add_executable(proj  ${SOURCES})

# If you have any external libraries, add them here
# For example:
# find_package(SomeLibrary REQUIRED)
# target_link_libraries(vm PRIVATE SomeLibrary)

# Add compile options if needed
# For example, to enable all warnings:
# target_compile_options(vm PRIVATE -Wall -Wextra)

# Print include directories (for debugging)
get_property(dirs DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY INCLUDE_DIRECTORIES)
foreach(dir ${dirs})
    message(STATUS "Include dir='${dir}'")
endforeach()
