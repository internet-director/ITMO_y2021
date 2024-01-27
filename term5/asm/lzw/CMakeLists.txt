cmake_minimum_required(VERSION 3.25)
project(asm23-lzw-internet-director C)
set(CMAKE_C_COMPILER "i686-w64-mingw32-gcc")
set(CMAKE_C_STANDARD 11)

include_directories(.)
enable_language(ASM_NASM)
set(CMAKE_ASM_NASM_OBJECT_FORMAT win32)

set(CMAKE_ASM_NASM_COMPILE_OBJECT "<CMAKE_ASM_NASM_COMPILER> <INCLUDES> <FLAGS> \
    -fwin32 -o <OBJECT> <SOURCE>")


#set_source_files_properties(lzw.asm PROPERTIES COMPILE_FLAGS "-gcv8")
set_source_files_properties(tmain.c PROPERTIES COMPILE_FLAGS "-m32 -no-pie -fno-pie")
#set_source_files_properties(tmain.c PROPERTIES COMPILE_FLAGS "-no-pie -fno-pie")

add_link_options(-g -m32 -no-pie -fno-pie)
#add_link_options(-no-pie -fno-pie -g)

add_executable(asm23-lzw-internet-director tmain.c lzw.asm)

