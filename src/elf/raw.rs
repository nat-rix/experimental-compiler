#![allow(non_camel_case_types)]

pub type Elf64_Half = u16;
pub type Elf64_Word = u32;
pub type Elf64_Xword = u64;
pub type Elf64_Addr = u64;
pub type Elf64_Off = u64;

pub const CLASS64: u8 = 2;
pub const DATA2LSB: u8 = 1;
pub const EV_CURRENT: u8 = 1;
pub const OSABI_NONE: u8 = 0;
pub const OSABIVER_UNSPEC: u8 = 0;

pub const ET_EXEC: Elf64_Half = 2;

pub const EM_X86_64: Elf64_Half = 62;

pub const PT_LOAD: Elf64_Word = 1;

pub const PF_X: Elf64_Word = 1 << 0;
pub const PF_W: Elf64_Word = 1 << 1;
pub const PF_R: Elf64_Word = 1 << 2;

pub const SHT_PROGBITS: Elf64_Word = 1;
pub const SHT_STRTAB: Elf64_Word = 3;

pub const SHF_ALLOC: Elf64_Xword = 1 << 1;
pub const SHF_EXECINSTR: Elf64_Xword = 1 << 2;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct ElfIdent {
    pub magic: [u8; 4],
    pub class: u8,
    pub data: u8,
    pub version: u8,
    pub osabi: u8,
    pub abiver: u8,
    pub padding: [u8; 7],
}

impl ElfIdent {
    pub const fn default_x86_64_unknown() -> Self {
        Self {
            magic: *b"\x7fELF",
            class: CLASS64,
            data: DATA2LSB,
            version: EV_CURRENT,
            osabi: OSABI_NONE,
            abiver: OSABIVER_UNSPEC,
            padding: [0; 7],
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Elf64_Ehdr {
    pub ident: ElfIdent,
    pub e_type: Elf64_Half,
    pub e_machine: Elf64_Half,
    pub e_version: Elf64_Word,
    pub e_entry: Elf64_Addr,
    pub e_phoff: Elf64_Off,
    pub e_shoff: Elf64_Off,
    pub e_flags: Elf64_Word,
    pub e_ehsize: Elf64_Half,
    pub e_phentsize: Elf64_Half,
    pub e_phnum: Elf64_Half,
    pub e_shentsize: Elf64_Half,
    pub e_shnum: Elf64_Half,
    pub e_shstrndx: Elf64_Half,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Elf64_Phdr {
    pub p_type: Elf64_Word,
    pub p_flags: Elf64_Word,
    pub p_offset: Elf64_Off,
    pub p_vaddr: Elf64_Addr,
    pub p_paddr: Elf64_Addr,
    pub p_filesz: Elf64_Xword,
    pub p_memsz: Elf64_Xword,
    pub p_align: Elf64_Xword,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Elf64_Shdr {
    pub sh_name: Elf64_Word,
    pub sh_type: Elf64_Word,
    pub sh_flags: Elf64_Xword,
    pub sh_addr: Elf64_Addr,
    pub sh_offset: Elf64_Off,
    pub sh_size: Elf64_Xword,
    pub sh_link: Elf64_Word,
    pub sh_info: Elf64_Word,
    pub sh_addralign: Elf64_Xword,
    pub sh_entsize: Elf64_Xword,
}
