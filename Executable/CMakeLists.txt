cmake_minimum_required(VERSION 3.29)
project(Executable)

file(GLOB_RECURSE SOURCE_FILES
        "${PROJECT_SOURCE_DIR}/src/*.c")

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_STANDARD 17)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror -Wall -Wextra -Wconversion -Wsign-conversion")

include_directories(include)

add_executable(${PROJECT_NAME} ${SOURCE_FILES})
