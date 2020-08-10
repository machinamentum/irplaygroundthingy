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

add_executable(ir src/main.cpp ${IR_SRCS})
target_link_libraries(ir ${CMAKE_THREAD_LIBS_INIT})

if (ENABLE_PROGRAMS)
    add_executable(bf programs/bf.cpp ${IR_SRCS})
    target_link_libraries(bf ${CMAKE_THREAD_LIBS_INIT})
endif()
