/// Copyright: Copyright (c) 2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Resources:
/// - https://www.muppetlabs.com/~breadbox/software/ELF.txt
/// - ELF-64 Object File Format: Version 1.5 Draft 2, May 27, 1998
/// - http://www.skyfree.org/linux/references/ELF_Format.pdf
/// - https://github.com/aspieln3r/spergland/wiki/ELF-File-:-short-summary
/// - https://refspecs.linuxbase.org/elf/gabi4+/ch4.eheader.html

/// An ELF object file consists of the following parts:
/// * File header, which must appear at the beginning of the file.
/// * Section table, required for relocatable files, and optional for loadable files.
/// * Program header table, required for loadable files, and optional for
///   relocatable files. This table describes the loadable segments and other
///   data structures required for loading a program or dynamically-linked
///   library in preparation for execution.
/// * Contents of the sections or segments, including loadable data, relocations,
///   and string and symbol tables.
module vox.be.elf64;

/// The file header is located at the beginning of the file, and is used to locate the other parts of the file.
struct Elf64Header {
	char[4] magic = "\x7FELF";
	ElfObjectFileClass file_class = ElfObjectFileClass.ELFCLASS64;
	ElfDataEncoding data_encoding = ElfDataEncoding.ELFDATA2LSB;
	ubyte file_version = 1; // Object file format version, always 1
	ElfOsAbi os_abi; // Identifies the operating system and ABI for which the object is prepared
	// Specifies version of the ABI in os_abi field
	// For applications conforming to the System V ABI, third edition, this field should contain 0.
	ubyte abi_version = 0;
	ubyte[7] pad;

	ElfObjectFileType file_type; // Object file type
	ElfMachineType machine;   // Machine type. These values are defined in the processor-specific supplements.
	uint      _version = 1; // Object file format version
	// Contains the virtual address of the program entry point. If there is no entry point, this field contains zero.
	ulong     entry;     // Entry point address
	ulong     phoff = 64; // Program header file offset in bytes
	ulong     shoff;     // Section header file offset in bytes
	uint      flags;     // Processor-specific flags
	ushort    ehsize = 64; // ELF header size in bytes
	ushort    phentsize = 56; // Program header table single entry size in bytes.
	ushort    phnum;     // Program header table number of entries.
	ushort    shentsize = 64; // Section header table single entry size in bytes.
	ushort    shnum;     // Section header table number of entries.
	// Contains the section header table index of the section containing
	// the section name string table. If there is no section name string
	// table, this field has the value SHN_UNDEF (0)
	ushort    shstrndx;  // String table index
}
static assert(Elf64Header.sizeof == 64);

// Object File Classes, identifies the class of the object file, or its capacity.
enum ElfObjectFileClass : ubyte {
	ELFCLASSNONE = 0, /// Invalid class
	ELFCLASS32 = 1, /// 32-bit objects
	ELFCLASS64 = 2, /// 64-bit objects
}

// Data Encodings
enum ElfDataEncoding : ubyte {
	ELFDATA2LSB = 1, /// Object file data structures are little-endian
	ELFDATA2MSB = 2, /// Object file data structures are big-endian
}

// Operating System and ABI Identifiers,
enum ElfOsAbi : ubyte {
	ELFOSABI_SYSV = 0, /// System V ABI
	ELFOSABI_HPUX = 1, /// HP-UX operating system
	ELFOSABI_STANDALONE = 255, /// Standalone (embedded) application
}

// Object File Types
enum ElfObjectFileType : ushort {
	ET_NONE = 0, /// No file type
	ET_REL  = 1, /// Relocatable object file
	ET_EXEC = 2, /// Executable file
	ET_DYN  = 3, /// Shared object file
	ET_CORE = 4, /// Core file
	ET_LOOS = 0xFE00, /// Environment-specific use
	ET_HIOS = 0xFEFF, ///
	ET_LOPROC = 0xFF00, /// Processor-specific use
	ET_HIPROC = 0xFFFF, ///
}

enum ElfMachineType : ushort {
	x86 = 0x03,    /// x86
	x86_64 = 0x3E, /// amd64
	arm = 0x28,
	arm64 = 0xB7,  /// aarch64
	riscv = 0xF3,  /// RISC-V
}

// Section index 0, and indices in the range 0xFF00–0xFFFF are reserved for special purposes.
// The first entry in the section header table (with an index of 0) is reserved, and must contain all zeroes.
enum ElfSectionIndicies : ushort {
	SHN_UNDEF = 0, /// Used to mark an undefined or meaningless section reference
	SHN_LOPROC = 0xFF00, /// Processor-specific use
	SHN_HIPROC = 0xFF1F,
	SHN_LOOS = 0xFF20, /// Environment-specific use
	SHN_HIOS = 0xFF3F,
	SHN_ABS = 0xFFF1, /// Indicates that the corresponding reference is an absolute value
	SHN_COMMON = 0xFFF2, /// Indicates a symbol that has been declared as a common block (Fortran COMMON or C tentative declaration)
}

struct Elf64SectionHeader {
	uint  sh_name;      /// Section name
	uint  sh_type;      /// Section type
	ulong sh_flags;     /// Section attributes
	ulong sh_addr;      /// Virtual address in memory
	ulong sh_offset;    /// Offset in file
	ulong sh_size;      /// Size of section
	uint  sh_link;      /// Link to other section
	uint  sh_info;      /// Miscellaneous information
	ulong sh_addralign; /// Address alignment boundary
	ulong sh_entsize;   /// Size of entries, if section has table
}

struct Elf64ProgramHeader {
	Elf64SegmentType type; /// Type of segment
	uint  flags;  /// Segment attributes (set of Elf64SegmentAttributes)
	ulong offset; /// Offset in file
	ulong vaddr;  /// Virtual address in memory
	ulong paddr;  /// Reserved
	ulong filesz; /// Size of segment in file
	ulong memsz;  /// Size of segment in memory
	ulong alignment; /// Alignment of segment
}

enum Elf64SegmentType : uint {
	PT_NULL = 0, /// Unused entry
	PT_LOAD = 1, /// Loadable segment
	PT_DYNAMIC = 2, /// Dynamic linking tables
	/// If PT_INTERP segment is added, then PT_DYNAMIC must be present too
	/// Otherwise it is invalid executable
	PT_INTERP = 3, /// Program interpreter path name
	PT_NOTE = 4, /// Note sections
	PT_SHLIB = 5, /// Reserved
	PT_PHDR = 6, /// Program header table
	PT_TLS = 7, /// Thread Local Storage http://www.sco.com/developers/gabi/latest/ch5.pheader.html#tls
	PT_LOOS = 0x6000_0000, /// Environment-specific use
	PT_HIOS = 0x6FFF_FFFF,
	PT_LOPROC = 0x7000_0000, /// Processor-specific use
	PT_HIPROC = 0X7FFF_FFFF,
	/// The array element specifies the location and size of the
	/// exception handling information as defined by the .eh_frame_hdr section.
	PT_GNU_EH_FRAME = 0X6474E550,
	/// The p_flags member specifies the permissions on the segment containing the stack and
	/// is used to indicate wether the stack should be executable. The absense of this header
	/// indicates that the stack will be executable.
	PT_GNU_STACK = 0X6474E551,
	/// The array element specifies the location and size of a segment which may be made
	/// read-only after relocation shave been processed.
	PT_GNU_RELRO = 0X6474E552,
}

enum Elf64SegmentAttributes : uint {
	EXECUTE = 0x1, /// Execute permission
	WRITE = 0x2, /// Write permission
	READ = 0x4, /// Read permission
	PF_MASKOS = 0x00FF_0000, /// These flag bits are reserved for environment-specific use
	PF_MASKPROC = 0xFF00_0000, /// These flag bits are reserved for processor-specific use
}
