cmake_minimum_required(VERSION 2.6)
project(ir)
#find_package (Threads)

set(CMAKE_CXX_STANDARD 17)

option(ENABLE_PROGRAMS "Include build targets for programs from the /programs directory." ON)

include_directories(src)

if (UNIX)
    add_definitions(-fno-rtti -fno-exceptions -Wall -Wextra -pedantic -Wconversion -Wno-unused-const-variable)
endif()


set (IR_SRCS
    src/macho.cpp
    src/coff.cpp
    src/x64.cpp
    src/linker_object.cpp
    src/print_ir.cpp
)


add_library(lib_objects OBJECT ${IR_SRCS})
add_library(irpgt STATIC ${IR_SRCS})
set_target_properties(irpgt PROPERTIES PUBLIC_HEADER "src/ir.h;src/linker_object.h;src/general.h")
install(TARGETS irpgt EXPORT irpgtConfig
        LIBRARY DESTINATION lib
        PUBLIC_HEADER DESTINATION include/irpgt
)
install(EXPORT irpgtConfig DESTINATION cmake)

if (ENABLE_PROGRAMS)
    add_executable(bf programs/bf.cpp)
    target_link_libraries(bf lib_objects ${CMAKE_THREAD_LIBS_INIT})

    add_executable(minij programs/minij.cpp)
    target_link_libraries(minij lib_objects ${CMAKE_THREAD_LIBS_INIT})

    add_executable(old_test programs/old_test.cpp)
    target_link_libraries(old_test lib_objects ${CMAKE_THREAD_LIBS_INIT})
endif()
