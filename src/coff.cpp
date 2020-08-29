#include "general.h"
#include "linker_object.h"

#include <stdio.h> // fopen and whatnot

struct PE_Coff_Header {
    u16 Machine;
    u16 NumberOfSections;
    u32 TimeDateStamp;
    u32 PointerToSymbolTable;
    u32 NumberOfSymbols;
    u16 SizeOfOptionalHeader;
    u16 Characteristics;
};

struct PE_Coff_Section_Header {
    char Name[8];
    u32 VirtualSize;
    u32 VirtualAddress;
    u32 SizeOfRawData;
    u32 PointerToRawData;
    u32 PointerToRelocations;
    u32 PointerToLinenumbers;
    u16 NumberOfRelocations;
    u16 NumberOfLinenumbers;
    u32 Characteristics;
};

const u64 PE_COFF_RELOCATION_SIZE = 10; // sizeof(PE_Coff_Relocation) is slightly larger than 10 bytes due to padding.
struct PE_Coff_Relocation {
    u32 VirtualAddress;
    u32 SymbolTableIndex;
    u16 Type;
};

const u64 PE_COFF_SYMBOL_SIZE = 18; // sizeof(PE_Coff_Symbol) is slightly larger than 18 bytes due to padding.
struct PE_Coff_Symbol {
    union {
        char ShortName[8];
        struct {
            u32 Zeroes;
            u32 Offset;
        } LongName;
    } Name;

    u32 Value;
    u16 SectionNumber;
    u16 Type;
    u8 StorageClass;
    u8 NumberOfAuxSymbols;
};

const u16 IMAGE_FILE_MACHINE_UNKNOWN = 0x0;
const u16 IMAGE_FILE_MACHINE_AMD64   = 0x8664;
const u16 IMAGE_FILE_MACHINE_ARM64   = 0xAA64;


const u32 IMAGE_SCN_CNT_CODE         = 0x00000020;
const u32 IMAGE_SCN_ALIGN_16BYTES    = 0x00500000;
const u32 IMAGE_SCN_LNK_NRELOC_OVFL  = 0x01000000;
const u32 IMAGE_SCN_MEM_EXECUTE      = 0x20000000;
const u32 IMAGE_SCN_MEM_READ         = 0x40000000;
const u32 IMAGE_SCN_MEM_WRITE        = 0x80000000;

const u16 IMAGE_REL_AMD64_ADDR64     = 0x0001;
const u16 IMAGE_REL_AMD64_REL32      = 0x0004;
const u16 IMAGE_REL_AMD64_REL32_1    = 0x0005;
const u16 IMAGE_REL_AMD64_REL32_2    = 0x0006;
const u16 IMAGE_REL_AMD64_REL32_3    = 0x0007;
const u16 IMAGE_REL_AMD64_REL32_4    = 0x0008;
const u16 IMAGE_REL_AMD64_REL32_5    = 0x0009;

const u16 IMAGE_REL_AMD64_SECTION    = 0x000A;
const u16 IMAGE_REL_AMD64_SECREL     = 0x000B;


const u8 IMAGE_SYM_DTYPE_FUNCTION = 0x20;

void emit_coff_file(Linker_Object *object) {
    Data_Buffer buffer;

    PE_Coff_Header *header = buffer.allocate_unaligned<PE_Coff_Header>();
    header->Machine              = IMAGE_FILE_MACHINE_AMD64;

    header->NumberOfSections     = static_cast<u16>(object->sections.count);
    header->TimeDateStamp        = 0; // @TODO
    // header->PointerToSymbolTable = ;
    header->NumberOfSymbols      = object->symbol_table.count;
    header->SizeOfOptionalHeader = 0;
    header->Characteristics      = 0;


    for (auto &sect : object->sections) {
        PE_Coff_Section_Header  *section = buffer.allocate_unaligned<PE_Coff_Section_Header>();
        memset(section, 0, sizeof(PE_Coff_Section_Header));

        sect.mach_section = section;

        memcpy(section->Name, sect.name.data, sect.name.length);

        u32 data_size = sect.data.size();
        section->VirtualSize    = data_size;
        section->VirtualAddress = 0;
        section->SizeOfRawData  = data_size;
        // section->PointerToRawData = ; 
        // section->PointerToRelocations = ;
        section->PointerToLinenumbers = 0;

        u32 Characteristics = IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_16BYTES;

        if (sect.is_writable)          Characteristics |= IMAGE_SCN_MEM_WRITE;
        if (sect.is_pure_instructions) Characteristics |= (IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE);

        if (sect.relocations.count >= 0xFFFF) {
            section->NumberOfRelocations = 0xFFFF;
            Characteristics |= IMAGE_SCN_LNK_NRELOC_OVFL;
        } else {
            section->NumberOfRelocations = static_cast<u16>(sect.relocations.count);
        }

        section->NumberOfLinenumbers = 0;
        section->Characteristics = Characteristics;
    }

    for (auto &sect : object->sections) {
        PE_Coff_Section_Header *section = (PE_Coff_Section_Header *)sect.mach_section;

        section->PointerToRawData = buffer.size();

        buffer.append(&sect.data);

        section->PointerToRelocations = buffer.size();

        if (section->Characteristics & IMAGE_SCN_LNK_NRELOC_OVFL) {
            PE_Coff_Relocation *info = (PE_Coff_Relocation *)buffer.allocate_bytes_unaligned(PE_COFF_RELOCATION_SIZE);

            info->VirtualAddress   = sect.relocations.count;
            info->SymbolTableIndex = 0;
            info->Type             = 0;
        }

        for (auto &reloc : sect.relocations) {
            PE_Coff_Relocation *info = (PE_Coff_Relocation *)buffer.allocate_bytes_unaligned(PE_COFF_RELOCATION_SIZE);
            info->VirtualAddress   = reloc.offset;
            info->SymbolTableIndex = reloc.symbol_index;

            u16 type = IMAGE_REL_AMD64_ADDR64;
            if (reloc.is_for_rip_call || reloc.is_rip_relative) type = IMAGE_REL_AMD64_REL32;

            if (type == IMAGE_REL_AMD64_ADDR64) assert(reloc.size == 8);
            if (type == IMAGE_REL_AMD64_REL32)  assert(reloc.size == 4);
            info->Type = type;
        }
    }

    Data_Buffer string_buffer;
    string_buffer.append_byte(0);

    header->PointerToSymbolTable = buffer.size();

    for (auto &symbol : object->symbol_table) {
        PE_Coff_Symbol *sym = (PE_Coff_Symbol *)buffer.allocate_bytes_unaligned(PE_COFF_SYMBOL_SIZE);

        if (symbol.linkage_name.length <= 8) {
            memset(sym->Name.ShortName, 0, 8);
            memcpy(sym->Name.ShortName, symbol.linkage_name.data, symbol.linkage_name.length);
        } else {
            sym->Name.LongName.Zeroes = 0;
            sym->Name.LongName.Offset = string_buffer.size() + 4; // +4 to include the string_table_size field itself.

            assert(symbol.linkage_name.length <= U32_MAX);
            string_buffer.append(symbol.linkage_name.data, static_cast<u32>(symbol.linkage_name.length));
            string_buffer.append_byte(0);
        }

        sym->SectionNumber = symbol.section_number;
        sym->Value = symbol.section_offset;
     
        if (symbol.is_function) {
            sym->Type = IMAGE_SYM_DTYPE_FUNCTION;
        } else {
            sym->Type = 0;
        }

        sym->StorageClass       = 2;
        sym->NumberOfAuxSymbols = 0;
    }

    assert(string_buffer.size() <= (U32_MAX-4));
    u32 *string_table_size = buffer.allocate_unaligned<u32>();
    *string_table_size = string_buffer.size() + 4; // +4 to include the string_table_size field itself.

    buffer.append(&string_buffer);

    FILE *file = fopen("test.obj", "wb");
    for (auto &c : buffer.chunks) {
        size_t amount = c.count;
        do {
            size_t written = fwrite(c.data, 1, amount, file);
            amount -= written;
        } while (amount > 0);
    }
    fclose(file);
}
