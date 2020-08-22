#include "general.h"
#include "linker_object.h"

#include <stdio.h> // fopen and such

struct mach_header_64 {
    u32 magic;
    u32 cputype;
    u32 cpusubtype;
    u32 filetype;
    u32 ncmds;
    u32 sizeofcmds;
    u32 flags;
    u32 reserved;
};

struct load_command {
    u32 cmd;
    u32 cmdsize;
};


struct symtab_command : load_command {
    u32 symoff;
    u32 nsyms;
    u32 stroff;
    u32 strsize;
};

struct segment_command_64 : load_command {
    char segname[16];
    u64 vmaddr;
    u64 vmsize;
    u64 fileoff;
    u64 filesize;
    u32 maxprot;
    u32 initprot;
    u32 nsects;
    u32 flags;
};

struct section_64 {
    char sectname[16];
    char segname[16];
    u64 addr;
    u64 size;
    u32 offset;
    u32 align;
    u32 reloff;
    u32 nreloc;
    u32 flags;
    u32 reserved1;
    u32 reserved2;
    u32 reserved3;
};

struct relocation_info {
    u32 r_address;
    u32 r_info;
};

struct nlist_64 {
    u32 n_strx;
    u8  n_type;
    u8  n_sect;
    u16 n_desc;
    u64 n_value;
};

#define RELOC_LEN1 0
#define RELOC_LEN2 1
#define RELOC_LEN4 2
#define RELOC_LEN8 3

#define R_INFO(symbol_index, pcrel, len, ext, type) ((symbol_index & 0xFFFFFF) | (pcrel << 24) | (len << 25) | (ext << 27) | (type << 28))

const u32 MH_MAGIC_64 = 0xFEEDFACF;
const u32 MH_CIGAM_64 = 0xCFFAEDFE;

const u32 CPU_TYPE_x86_64  = 0x1000007;
const u32 CPU_TYPE_AArch64 = 0x100000B;

const u32 MH_OBJECT   = 1;
const u32 MH_EXECUTE  = 2;
const u32 MH_FVMLIB   = 3;
const u32 MH_CORE     = 4;
const u32 MH_PRELOAD  = 5;
const u32 MH_DYLIB    = 6;
const u32 MH_DYLINKER = 7;
const u32 MH_BUNDLE   = 8;

const u32 LC_SEGMENT         = 0x1;
const u32 LC_SYMTAB          = 0x2;
const u32 LC_SYMSEG          = 0x3;
const u32 LC_THREAD          = 0x4;
const u32 LC_UNIXTHREAD      = 0x5;
const u32 LC_LOADFVMLIB      = 0x6;
const u32 LC_IDFVMLIB        = 0x7;
const u32 LC_IDENT           = 0x8;
const u32 LC_FVMFILE         = 0x9;
const u32 LC_PREPAGE         = 0xA;
const u32 LC_DYSYMTAB        = 0xB;
const u32 LC_LOAD_DYLIB      = 0xC;
const u32 LC_ID_DYLIB        = 0xD;
const u32 LC_LOAD_DYLINKER   = 0xE;
const u32 LC_ID_DYLINKER     = 0xF;
const u32 LC_PREBOUND_DYLIB  = 0x10;
const u32 LC_SEGMENT_64      = 0x19;

const u32 X86_64_RELOC_UNSIGNED   = 0;
const u32 X86_64_RELOC_SIGNED     = 1;
const u32 X86_64_RELOC_BRANCH     = 2;
const u32 X86_64_RELOC_GOT_LOAD   = 3;
const u32 X86_64_RELOC_GOT        = 4;
const u32 X86_64_RELOC_SUBTRACTOR = 5;

const u8 N_EXT  = 0x01;
const u8 N_SECT = 0x0E;

const u8 NO_SECT = 0;

const u32 S_ATTR_PURE_INSTRUCTIONS = 0x80000000;

void emit_macho_file(Linker_Object *object) {
    Data_Buffer buffer;

    mach_header_64 *header = buffer.allocate_unaligned<mach_header_64>();
    header->magic = MH_MAGIC_64;
    header->cputype = CPU_TYPE_x86_64;
    header->cpusubtype = 3;
    header->filetype = MH_OBJECT;
    header->ncmds = 2;
    // header->sizeofcmds =;
    header->flags    = 0x2000;
    header->reserved = 0;

    auto load_cmds_start = buffer.size();

    auto symtab_cmd = buffer.allocate_unaligned<symtab_command>();
    symtab_cmd->cmd     = LC_SYMTAB;
    symtab_cmd->cmdsize = sizeof(symtab_command);
    // symtab_cmd->symoff = ;
    symtab_cmd->nsyms = object->symbol_table.count;
    // symtab_cmd->stroff  = ;
    // symtab_cmd->strsize = ;

    auto segment_cmd = buffer.allocate_unaligned<segment_command_64>();
    segment_cmd->cmd     = LC_SEGMENT_64;
    segment_cmd->cmdsize = sizeof(segment_command_64) + sizeof(section_64)*object->sections.count;
    memset(segment_cmd->segname, 0, 16);
    // memcpy(segment_cmd->segname, "__TEXT", 6);

    segment_cmd->vmaddr = 0;
    segment_cmd->maxprot  = 0;
    segment_cmd->initprot = 0;
    segment_cmd->nsects   = object->sections.count;
    segment_cmd->flags = 0;

    for (auto &sect : object->sections) {
        section_64  *section = buffer.allocate_unaligned<section_64>();
        memset(section, 0, sizeof(section_64));

        sect.mach_section = section;

        memcpy(section->sectname, sect.name.data, sect.name.length);
        memcpy(section->segname,  sect.segment.data, sect.segment.length);
        section->size = sect.data.size();
        section->align = 0; // @TOOD
        section->nreloc = sect.relocations.count;
        section->flags = 0;

        if (sect.is_pure_instructions) section->flags |= S_ATTR_PURE_INSTRUCTIONS | 0x400;

        section->reserved1 = 0;
        section->reserved2 = 0;
        section->reserved3 = 0;
    }

    header->sizeofcmds = buffer.size()-load_cmds_start;

    segment_cmd->fileoff = buffer.size();

    u64 addr = 0;

    for (auto &sect : object->sections) {
        section_64 *section = (section_64 *)sect.mach_section;

        section->addr = addr;
        section->offset = buffer.size();

        buffer.append(&sect.data);

        addr += section->size;

        section->reloff = buffer.size();
        section->nreloc = sect.relocations.count;

        for (auto &reloc : sect.relocations) {
            relocation_info *info = buffer.allocate_unaligned<relocation_info>();
            info->r_address = reloc.offset;

            u32 size = 0;
            if      (reloc.size == 1) size = RELOC_LEN1;
            else if (reloc.size == 2) size = RELOC_LEN2;
            else if (reloc.size == 4) size = RELOC_LEN4;
            else if (reloc.size == 8) size = RELOC_LEN8;
            else assert(false);

            u32 pcrel = 0;
            if (reloc.is_for_rip_call || reloc.is_rip_relative) pcrel = 1;

            u32 type = X86_64_RELOC_UNSIGNED;
            if      (reloc.is_for_rip_call) type = X86_64_RELOC_BRANCH;
            else if (reloc.is_rip_relative) type = X86_64_RELOC_SIGNED;

            info->r_info = R_INFO(reloc.symbol_index, pcrel, size, 1, type);
        }
    }

    segment_cmd->filesize =  buffer.size() - segment_cmd->fileoff;
    segment_cmd->vmsize = segment_cmd->filesize; // @TODO

    Data_Buffer string_buffer;
    string_buffer.append_byte(0);

    symtab_cmd->symoff = buffer.size();

    for (auto &symbol : object->symbol_table) {
        auto sym = buffer.allocate_unaligned<nlist_64>();

        if (symbol.linkage_name.length) {
            sym->n_strx = string_buffer.size();
            if (!symbol.is_section) string_buffer.append_byte('_');

            assert(symbol.linkage_name.length <= U32_MAX);
            string_buffer.append(symbol.linkage_name.data, static_cast<u32>(symbol.linkage_name.length));
            string_buffer.append_byte(0);
        } else {
            sym->n_strx = 0;
        }

        if (symbol.is_externally_defined) {
            sym->n_type = N_EXT;
            sym->n_sect = NO_SECT;
        } else {
            sym->n_type = 0;
            if (symbol.section_number)        sym->n_type |= N_SECT;
            if (symbol.is_externally_visible) sym->n_type |= N_EXT;
            sym->n_sect = symbol.section_number;
        }

        sym->n_desc = 0;

        u64 section_addr = 0;
        if (symbol.section_number) section_addr = ((section_64 *)object->sections[symbol.section_number-1].mach_section)->addr;
        sym->n_value = symbol.section_offset + section_addr;
    }

    symtab_cmd->stroff = buffer.size();
    symtab_cmd->strsize = string_buffer.size();

    buffer.append(&string_buffer);

    FILE *file = fopen("test.o", "wb");
    for (auto &c : buffer.chunks) {
        size_t amount = c.count;
        do {
            size_t written = fwrite(c.data, 1, amount, file);
            amount -= written;
        } while (amount > 0);
    }
    fclose(file);
}