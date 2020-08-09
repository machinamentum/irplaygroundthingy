cmake_minimum_required(VERSION 2.6)
project(ir)
#find_package (Threads)

include_directories(src)

if (UNIX)
    add_definitions(--std=c++11 -fno-rtti -fno-exceptions -Wall -Wextra -pedantic -Wconversion -Wno-c++14-binary-literal -Wno-unused-const-variable)
endif()


set (IR_SRCS
    src/main.cpp
    src/macho.cpp
    src/coff.cpp
    src/x64.cpp
    src/linker_object.cpp 
)

add_executable(ir ${IR_SRCS})
target_link_libraries(ir ${CMAKE_THREAD_LIBS_INIT})
