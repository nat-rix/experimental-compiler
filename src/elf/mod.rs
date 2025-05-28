pub mod pod;
pub mod raw;

use pod::AsBytes;

use std::{fs::File, io::Write, path::Path};

use crate::error::InternalError;

const ELF_MODE: u32 = 0o755;
const PAGE_SIZE: u64 = 0x1_000;

pub struct ElfFile<'a> {
    file: File,
    path: &'a Path,
}

impl<'a> ElfFile<'a> {
    pub fn create(path: &'a impl AsRef<Path>) -> Result<Self, InternalError> {
        let path = path.as_ref();
        let mut options = std::fs::OpenOptions::new();
        options.write(true).create(true).truncate(true);
        #[cfg(unix)]
        {
            use std::os::unix::fs::OpenOptionsExt;
            options.mode(ELF_MODE);
        }
        let file = options
            .open(path)
            .map_err(|err| InternalError::FileCreate(path.to_owned(), err.kind()))?;
        Ok(Self { file, path })
    }

    fn write(&mut self, data: &[u8]) -> Result<(), InternalError> {
        self.file
            .write_all(data)
            .map_err(|err| InternalError::FileWrite(self.path.to_owned(), err.kind()))
    }
}

const PHDR_COUNT: usize = 1;
const SHDR_COUNT: usize = 3;
// first used virtual address (first page is skipped)
pub(crate) const PROG_ADDR: u64 = 0x401000;
// virtual address of the entry point
const ENTRY_ADDR: u64 = PROG_ADDR;
#[derive(Debug, Clone, Copy)]
#[repr(C)]
struct SectionNames {
    pub null: [u8; 1],
    pub shrtrtab: [u8; 10],
    pub text: [u8; 6],
}
impl Default for SectionNames {
    fn default() -> Self {
        Self {
            null: *b"\0",
            shrtrtab: *b".shrtrtab\0",
            text: *b".text\0",
        }
    }
}
#[derive(Debug, Clone, Copy)]
#[repr(C)]
struct CustomElfHeaders {
    pub ehdr: raw::Elf64_Ehdr,
    pub phdrs: [raw::Elf64_Phdr; PHDR_COUNT],
    pub shdrs: [raw::Elf64_Shdr; SHDR_COUNT],
    pub names: SectionNames,
}

unsafe impl AsBytes for CustomElfHeaders {}

pub fn write_code(file: &mut ElfFile, code: &[u8]) -> Result<(), InternalError> {
    let prog_offset = PAGE_SIZE;
    let prog_size = code.len();
    let hdrs = CustomElfHeaders {
        ehdr: raw::Elf64_Ehdr {
            ident: raw::ElfIdent::default_x86_64_unknown(),
            e_type: raw::ET_EXEC,
            e_machine: raw::EM_X86_64,
            e_version: raw::EV_CURRENT.into(),
            e_entry: ENTRY_ADDR,
            e_phoff: core::mem::offset_of!(CustomElfHeaders, phdrs) as _,
            e_shoff: core::mem::offset_of!(CustomElfHeaders, shdrs) as _,
            e_flags: 0,
            e_ehsize: core::mem::size_of::<raw::Elf64_Ehdr>() as _,
            e_phentsize: core::mem::size_of::<raw::Elf64_Phdr>() as _,
            e_phnum: PHDR_COUNT as _,
            e_shentsize: core::mem::size_of::<raw::Elf64_Shdr>() as _,
            e_shnum: SHDR_COUNT as _,
            e_shstrndx: 1,
        },
        phdrs: [raw::Elf64_Phdr {
            p_type: raw::PT_LOAD,
            p_flags: raw::PF_R | raw::PF_X,
            p_offset: prog_offset,
            p_vaddr: PROG_ADDR,
            p_paddr: PROG_ADDR,
            p_filesz: prog_size as _,
            p_memsz: prog_size as _,
            p_align: PAGE_SIZE,
        }],
        shdrs: [
            // the first section header must be zeroed out
            raw::Elf64_Shdr::default(),
            // .shrtrtab
            raw::Elf64_Shdr {
                sh_name: core::mem::offset_of!(SectionNames, shrtrtab) as _,
                sh_type: raw::SHT_STRTAB,
                sh_flags: 0,
                sh_addr: 0,
                sh_offset: core::mem::offset_of!(CustomElfHeaders, names) as _,
                sh_size: core::mem::size_of::<SectionNames>() as _,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
            // .text
            raw::Elf64_Shdr {
                sh_name: core::mem::offset_of!(SectionNames, text) as _,
                sh_type: raw::SHT_PROGBITS,
                sh_flags: raw::SHF_ALLOC | raw::SHF_EXECINSTR,
                sh_addr: PROG_ADDR,
                sh_offset: prog_offset,
                sh_size: prog_size as _,
                sh_link: 0,
                sh_info: 0,
                sh_addralign: 1,
                sh_entsize: 0,
            },
        ],
        names: Default::default(),
    };
    file.write(hdrs.as_bytes())?;
    let rest = prog_offset as usize - core::mem::size_of::<CustomElfHeaders>();
    file.write(&vec![0; rest])?;
    file.write(code)?;
    Ok(())
}
