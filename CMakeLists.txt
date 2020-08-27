cmake_minimum_required(VERSION 2.6)
project(ir)
#find_package (Threads)

option(ENABLE_PROGRAMS "Include build targets for programs from the /programs directory." ON)

include_directories(src)

if (UNIX)
    add_definitions(--std=c++11 -fno-rtti -fno-exceptions -Wall -Wextra -pedantic -Wconversion -Wno-c++14-binary-literal -Wno-unused-const-variable)
endif()


set (IR_SRCS
    src/macho.cpp
    src/coff.cpp
    src/x64.cpp
    src/linker_object.cpp 
)


add_library(lib_objects OBJECT ${IR_SRCS})

add_executable(ir src/main.cpp)
target_link_libraries(ir lib_objects ${CMAKE_THREAD_LIBS_INIT})

if (ENABLE_PROGRAMS)
    add_executable(bf programs/bf.cpp)
    target_link_libraries(bf lib_objects ${CMAKE_THREAD_LIBS_INIT})

    add_executable(minij programs/minij.cpp)
    target_link_libraries(minij lib_objects ${CMAKE_THREAD_LIBS_INIT})
endif()
