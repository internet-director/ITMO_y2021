cmake_minimum_required(VERSION 3.25)
project(asm23_print_internet_director C)
set(CMAKE_C_COMPILER "i686-w64-mingw32-gcc")
set(CMAKE_C_STANDARD 11)

include_directories(.)
enable_language(ASM_NASM)
set(CMAKE_ASM_NASM_OBJECT_FORMAT win32)
set(CMAKE_ASM_NASM_COMPILE_OBJECT "<CMAKE_ASM_NASM_COMPILER> <INCLUDES> \
    <FLAGS> -f ${CMAKE_ASM_NASM_OBJECT_FORMAT} -o <OBJECT> <SOURCE>")

set_source_files_properties(tmain.c PROPERTIES COMPILE_FLAGS "-m32 -no-pie -fno-pie")

add_link_options(-m32 -no-pie -fno-pie)


add_executable(asm23_print_internet_director tmain.c print.asm)
