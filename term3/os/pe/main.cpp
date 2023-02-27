#include <iostream>
#include <fstream>
#include <memory>
#include <cstring>
#include "Header.h"
//#include <Windows.h>

#define RVATOVA(base, offset) ( (size_t)base + (size_t)offset )
#define VATORAW(section, offset) ( (size_t)section->PointerToRawData + (size_t)offset - (size_t)section->VirtualAddress )
#define ALIGN_DOWN(x, align)  (x & ~(align-1))
#define CAST(data) (reinterpret_cast<char*>(data))
#define ALIGN_UP(x, align)    ((x & (align-1))?ALIGN_DOWN(x,align)+align:x)

bool dump_print(std::ifstream &file) {
    while (int c = file.get()) {
        if (c == -1) return false;
        std::cout << (char) c;
    }
    std::cout << std::endl;
    return true;
}

bool is_pe(std::ifstream &file) {
    std::uint32_t data;
    file.seekg(0x3C);
    if (!file.read(CAST(&data), 4)) return false;
    file.seekg(data);
    if (!file.read(CAST(&data), 4)) return false;
    return data == IMAGE_NT_SIGNATURE;
}

template<bool is_export>
bool print_import_export(std::ifstream &file) {
    size_t DIRECTORY_FLAG = IMAGE_DIRECTORY_ENTRY_IMPORT;
    if constexpr (is_export) {
        DIRECTORY_FLAG = IMAGE_DIRECTORY_ENTRY_EXPORT;
    }

    std::uint32_t e_lfanew;
    WORD number_of_section, sz_optional_header;
    IMAGE_SECTION_HEADER section;
    IMAGE_DATA_DIRECTORY directory;

    file.seekg(0x3C);
    if (!file.read(CAST(&e_lfanew), 4)) return false;
    file.seekg(e_lfanew + 0x6);
    if (!file.read(CAST(&number_of_section), 2)) return false;
    file.seekg(e_lfanew + 0x14);
    if (!file.read(CAST(&sz_optional_header), 2)) return false;
    file.seekg(e_lfanew + 0x88 + DIRECTORY_FLAG * sizeof(IMAGE_DATA_DIRECTORY));
    if (!file.read(CAST(&directory), sizeof(IMAGE_DATA_DIRECTORY))) return false;
    file.seekg(e_lfanew + FIELD_OFFSET(IMAGE_NT_HEADERS64, OptionalHeader) + sz_optional_header);
    if (!file.read(CAST(&section), sizeof(IMAGE_SECTION_HEADER))) return false;


    PIMAGE_SECTION_HEADER import_section = nullptr;
    for (int i = 0; i < number_of_section; i++) {
        if (directory.VirtualAddress >= section.VirtualAddress &&
            (directory.VirtualAddress < (section.VirtualAddress + section.Misc.VirtualSize))) {
            import_section = &section;
            break;
        }
        if (!file.read(CAST(&section), sizeof(IMAGE_SECTION_HEADER))) return false;
    }

    if (!import_section) {
        std::cerr << "Not find correct section" << std::endl;
        return false;
    }

    if constexpr (is_export) {
        IMAGE_EXPORT_DIRECTORY exp;
        file.seekg(VATORAW(import_section, directory.VirtualAddress));
        if (!file.read(CAST(&exp), sizeof(IMAGE_EXPORT_DIRECTORY))) return false;

        DWORD name;
        for (int i = 0; i < exp.NumberOfNames; i++) {
            file.seekg(VATORAW(import_section, exp.AddressOfNames) + 4 * i);
            if (!file.read(CAST(&name), 4)) return false;
            file.seekg(import_section->PointerToRawData + name - import_section->VirtualAddress);
            if (!dump_print(file)) return false;
        }
    } else {
        size_t index = 0;
        IMAGE_THUNK_DATA64 thunk;
        IMAGE_IMPORT_BY_NAME name;
        IMAGE_IMPORT_DESCRIPTOR imp;
        do {
            file.seekg(VATORAW(import_section, directory.VirtualAddress) + sizeof(IMAGE_IMPORT_DESCRIPTOR) * index++);
            if (!file.read(CAST(&imp), sizeof(IMAGE_IMPORT_DESCRIPTOR))) return false;
            if (!imp.Name) return false;

            file.seekg(VATORAW(import_section, imp.Name));
            if (!dump_print(file)) return false;

            size_t index2 = 0;
            while (true) {
                file.seekg(VATORAW(import_section, imp.DUMMYUNIONNAME.OriginalFirstThunk) +
                           sizeof(IMAGE_THUNK_DATA64) * index2++);
                if (!file.read(CAST(&thunk), sizeof(IMAGE_THUNK_DATA64))) return false;
                if (thunk.u1.AddressOfData == 0) break;
                file.seekg(VATORAW(import_section, thunk.u1.AddressOfData + 0x2));
                std::cout << "    ";
                if (!dump_print(file)) return false;
            }
        } while (true);
    }
    return true;
}


int main(int argc, char **argv) {
    if (argc != 3) {
        std::cerr << "Invalid arguments" << std::endl;
        return 1;
    }

    int code = 0;

    std::ifstream file(argv[2], std::ios::binary | std::ios::ate | std::ios::in);

    if (!file.is_open()) {
        std::cerr << "cant open file" << std::endl;
        return 1;
    }

    if (!std::strcmp(argv[1], "is-pe")) {
        bool check = is_pe(file);
        if (!check) {
            code = 1;
        }
        std::cout << (check ? "PE" : "Not PE") << std::endl;
    } else if (!std::strcmp(argv[1], "import-functions")) {
        print_import_export<false>(file);
    } else if (!std::strcmp(argv[1], "export-functions")) {
        print_import_export<true>(file);
    } else {
        std::cerr << "Invalid arguments" << std::endl;
        return 1;
    }

    return code;
}