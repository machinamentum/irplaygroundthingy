cmake_minimum_required(VERSION 2.6)
project(ir)
#find_package (Threads)

#include_directories()

if (UNIX)
    add_definitions(--std=c++11 -fno-rtti)
endif()


set (IR_SRCS
    src/main.cpp
)

add_executable(ir ${IR_SRCS})
target_link_libraries(ir ${CMAKE_THREAD_LIBS_INIT})
