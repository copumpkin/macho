{-# LANGUAGE ImplicitParams #-}
module Data.Macho.Types where

import Data.Int
import Data.Word
import Data.Bits
import Data.Binary.Get
import Data.Binary.Put
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Control.Monad
import Control.Applicative
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as C
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy     as L

data MH_MAGIC
    = MH_MAGIC32
    | MH_MAGIC64
    | MH_CIGAM32
    | MH_CIGAM64
  deriving (Ord, Eq, Show, Enum)
  
magic :: Bimap Word32 MH_MAGIC
magic = Bimap.fromList
 [ (0xfeedface, MH_MAGIC32)
 , (0xfeedfacf, MH_MAGIC64)
 , (0xcefaedfe, MH_CIGAM32)
 , (0xcffaedfe, MH_CIGAM64)
 ]

macho_to_magic = (magic Bimap.!)
macho_from_magic = (magic Bimap.!>)

bitfield_le off sz word = (word `shiftL` (32 - off - sz)) `shiftR` (32 - sz)
bitfield_be off sz word = (word `shiftL` off) `shiftR` (32 - sz)

newtype Decoder a = Decoder { runDecoder :: MachoBinary -> Get a }

decode ds bs = do
  b <- binary
  return $ runGet (runDecoder ds b) bs

getWord :: Decoder Word64
getWord = do
  is64 <- is64bit  
  if is64 then getWord64 else fromIntegral <$> getWord32

binary :: Decoder MachoBinary
binary = Decoder pure

lift :: Get a -> Decoder a
lift g = Decoder (\_ -> g)

instance Functor Decoder where
  fmap = liftM

instance Applicative Decoder where
  pure = return
  (<*>) = ap

instance Monad Decoder where
  return x = Decoder (\_ -> return x)
  Decoder f >>= g = Decoder $ \h -> do x <- f h;  runDecoder (g x) h
  
class MonadDecoder m where
  is64bit   :: m Bool
  getWord16 :: m Word16
  getWord32 :: m Word32
  getWord64 :: m Word64
  bitfield  :: Int -> Int -> Word32 -> m Word32
  
instance MonadDecoder Decoder where
  is64bit = Decoder (pure . _is64bit)
  getWord16 = Decoder _getWord16
  getWord32 = Decoder _getWord32
  getWord64 = Decoder _getWord64
  bitfield i j x = Decoder (\h -> pure $ _bitfield h i j x)

data MachoBinary = MachoBinary
    { _is64bit   :: Bool
    , _getWord16 :: Get Word16
    , _getWord32 :: Get Word32
    , _getWord64 :: Get Word64
    , _putWord16 :: Word16 -> Put
    , _putWord32 :: Word32 -> Put
    , _putWord64 :: Word64 -> Put
    , _bitfield  :: Int -> Int -> Word32 -> Word32
    }
        
le_binary = MachoBinary 
  { _is64bit = False
  , _getWord16 = getWord16le
  , _getWord32 = getWord32le
  , _getWord64 = getWord64le
  , _putWord16 = putWord16le
  , _putWord32 = putWord32le
  , _putWord64 = putWord64le
  , _bitfield = bitfield_le
  }
  
be_binary = MachoBinary
  { _is64bit = False
  , _getWord16 = getWord16be
  , _getWord32 = getWord32be
  , _getWord64 = getWord64be
  , _putWord16 = putWord16be
  , _putWord32 = putWord32be
  , _putWord64 = putWord64be
  , _bitfield = bitfield_be
  }
  
macho_binary MH_MAGIC32 = le_binary
macho_binary MH_MAGIC64 = le_binary { _is64bit = True }
macho_binary MH_CIGAM32 = be_binary
macho_binary MH_CIGAM64 = be_binary { _is64bit = True }

data CPU_TYPE
    = CPU_TYPE_X86
    | CPU_TYPE_X86_64
    | CPU_TYPE_ARM
    | CPU_TYPE_POWERPC
    | CPU_TYPE_POWERPC64
    deriving (Ord, Show, Eq, Enum)

cputype :: Bimap Word32 CPU_TYPE
cputype = Bimap.fromList
 [ (0x00000007, CPU_TYPE_X86)
 , (0x01000007, CPU_TYPE_X86_64)
 , (0x0000000c, CPU_TYPE_ARM)
 , (0x00000012, CPU_TYPE_POWERPC)
 , (0x01000012, CPU_TYPE_POWERPC64)
 ]
 
mach_to_cputype = (cputype Bimap.!)
mach_from_cputype = (cputype Bimap.!>)

data CPU_SUBTYPE
    = CPU_SUBTYPE_INTEL
    | CPU_SUBTYPE_I386_ALL
    | CPU_SUBTYPE_386
    | CPU_SUBTYPE_486
    | CPU_SUBTYPE_486SX
    | CPU_SUBTYPE_PENT
    | CPU_SUBTYPE_PENTPRO
    | CPU_SUBTYPE_PENTII_M3
    | CPU_SUBTYPE_PENTII_M5
    | CPU_SUBTYPE_CELERON
    | CPU_SUBTYPE_CELERON_MOBILE
    | CPU_SUBTYPE_PENTIUM_3
    | CPU_SUBTYPE_PENTIUM_3_M
    | CPU_SUBTYPE_PENTIUM_3_XEON
    | CPU_SUBTYPE_PENTIUM_M
    | CPU_SUBTYPE_PENTIUM_4
    | CPU_SUBTYPE_PENTIUM_4_M
    | CPU_SUBTYPE_ITANIUM
    | CPU_SUBTYPE_ITANIUM_2
    | CPU_SUBTYPE_XEON
    | CPU_SUBTYPE_XEON_MP
    | CPU_SUBTYPE_INTEL_FAMILY
    | CPU_SUBTYPE_INTEL_FAMILY_MAX
    | CPU_SUBTYPE_INTEL_MODEL
    | CPU_SUBTYPE_INTEL_MODEL_ALL
    | CPU_SUBTYPE_X86_ALL
    | CPU_SUBTYPE_X86_64_ALL
    | CPU_SUBTYPE_X86_ARCH1
    | CPU_SUBTYPE_POWERPC_ALL
    | CPU_SUBTYPE_POWERPC_601
    | CPU_SUBTYPE_POWERPC_602
    | CPU_SUBTYPE_POWERPC_603
    | CPU_SUBTYPE_POWERPC_603e
    | CPU_SUBTYPE_POWERPC_603ev
    | CPU_SUBTYPE_POWERPC_604
    | CPU_SUBTYPE_POWERPC_604e
    | CPU_SUBTYPE_POWERPC_620
    | CPU_SUBTYPE_POWERPC_750
    | CPU_SUBTYPE_POWERPC_7400
    | CPU_SUBTYPE_POWERPC_7450
    | CPU_SUBTYPE_POWERPC_970
    | CPU_SUBTYPE_ARM_ALL
    | CPU_SUBTYPE_ARM_V4T
    | CPU_SUBTYPE_ARM_V6
    deriving (Ord, Show, Eq, Enum)
    
cpusubtype :: Bimap (CPU_TYPE, Word32) CPU_SUBTYPE
cpusubtype = Bimap.fromList
  [ ((CPU_TYPE_X86, 132)      , CPU_SUBTYPE_486SX)
  , ((CPU_TYPE_X86, 5)        , CPU_SUBTYPE_PENT)
  , ((CPU_TYPE_X86, 22)       , CPU_SUBTYPE_PENTPRO)
  , ((CPU_TYPE_X86, 54)       , CPU_SUBTYPE_PENTII_M3)
  , ((CPU_TYPE_X86, 86)       , CPU_SUBTYPE_PENTII_M5)
  , ((CPU_TYPE_X86, 103)      , CPU_SUBTYPE_CELERON)
  , ((CPU_TYPE_X86, 119)      , CPU_SUBTYPE_CELERON_MOBILE)
  , ((CPU_TYPE_X86, 8)        , CPU_SUBTYPE_PENTIUM_3)
  , ((CPU_TYPE_X86, 24)       , CPU_SUBTYPE_PENTIUM_3_M)
  , ((CPU_TYPE_X86, 40)       , CPU_SUBTYPE_PENTIUM_3_XEON)
  , ((CPU_TYPE_X86, 9)        , CPU_SUBTYPE_PENTIUM_M)
  , ((CPU_TYPE_X86, 10)       , CPU_SUBTYPE_PENTIUM_4)
  , ((CPU_TYPE_X86, 26)       , CPU_SUBTYPE_PENTIUM_4_M)
  , ((CPU_TYPE_X86, 11)       , CPU_SUBTYPE_ITANIUM)
  , ((CPU_TYPE_X86, 27)       , CPU_SUBTYPE_ITANIUM_2)
  , ((CPU_TYPE_X86, 12)       , CPU_SUBTYPE_XEON)
  , ((CPU_TYPE_X86, 28)       , CPU_SUBTYPE_XEON_MP)
  , ((CPU_TYPE_X86, 3)        , CPU_SUBTYPE_X86_ALL)
  , ((CPU_TYPE_X86, 4)        , CPU_SUBTYPE_X86_ARCH1)
  , ((CPU_TYPE_X86_64, 3)     , CPU_SUBTYPE_X86_64_ALL)
  , ((CPU_TYPE_POWERPC, 0)    , CPU_SUBTYPE_POWERPC_ALL)
  , ((CPU_TYPE_POWERPC, 1)    , CPU_SUBTYPE_POWERPC_601)
  , ((CPU_TYPE_POWERPC, 2)    , CPU_SUBTYPE_POWERPC_602)
  , ((CPU_TYPE_POWERPC, 3)    , CPU_SUBTYPE_POWERPC_603)
  , ((CPU_TYPE_POWERPC, 4)    , CPU_SUBTYPE_POWERPC_603e)
  , ((CPU_TYPE_POWERPC, 5)    , CPU_SUBTYPE_POWERPC_603ev)
  , ((CPU_TYPE_POWERPC, 6)    , CPU_SUBTYPE_POWERPC_604)
  , ((CPU_TYPE_POWERPC, 7)    , CPU_SUBTYPE_POWERPC_604e)
  , ((CPU_TYPE_POWERPC, 8)    , CPU_SUBTYPE_POWERPC_620)
  , ((CPU_TYPE_POWERPC, 9)    , CPU_SUBTYPE_POWERPC_750)
  , ((CPU_TYPE_POWERPC, 10)   , CPU_SUBTYPE_POWERPC_7400)
  , ((CPU_TYPE_POWERPC, 11)   , CPU_SUBTYPE_POWERPC_7450)
  , ((CPU_TYPE_POWERPC, 100)  , CPU_SUBTYPE_POWERPC_970)
  , ((CPU_TYPE_POWERPC64, 0)  , CPU_SUBTYPE_POWERPC_ALL)
  , ((CPU_TYPE_POWERPC64, 1)  , CPU_SUBTYPE_POWERPC_601)
  , ((CPU_TYPE_POWERPC64, 2)  , CPU_SUBTYPE_POWERPC_602)
  , ((CPU_TYPE_POWERPC64, 3)  , CPU_SUBTYPE_POWERPC_603)
  , ((CPU_TYPE_POWERPC64, 4)  , CPU_SUBTYPE_POWERPC_603e)
  , ((CPU_TYPE_POWERPC64, 5)  , CPU_SUBTYPE_POWERPC_603ev)
  , ((CPU_TYPE_POWERPC64, 6)  , CPU_SUBTYPE_POWERPC_604)
  , ((CPU_TYPE_POWERPC64, 7)  , CPU_SUBTYPE_POWERPC_604e)
  , ((CPU_TYPE_POWERPC64, 8)  , CPU_SUBTYPE_POWERPC_620)
  , ((CPU_TYPE_POWERPC64, 9)  , CPU_SUBTYPE_POWERPC_750)
  , ((CPU_TYPE_POWERPC64, 10) , CPU_SUBTYPE_POWERPC_7400)
  , ((CPU_TYPE_POWERPC64, 11) , CPU_SUBTYPE_POWERPC_7450)
  , ((CPU_TYPE_POWERPC64, 100), CPU_SUBTYPE_POWERPC_970)
  , ((CPU_TYPE_ARM, 0)        , CPU_SUBTYPE_ARM_ALL)
  , ((CPU_TYPE_ARM, 5)        , CPU_SUBTYPE_ARM_V4T)
  , ((CPU_TYPE_ARM, 6)        , CPU_SUBTYPE_ARM_V6)
  ]

mach_to_cpusubtype = curry (cpusubtype Bimap.!)
mach_from_cpusubtype = (cpusubtype Bimap.!>)

data MachoHeader = MachoHeader
    { mh_cputype    :: CPU_TYPE    -- ^ CPU family the Mach-O executes on.
    , mh_cpusubtype :: CPU_SUBTYPE -- ^ Specific CPU type the Mach-O executes on.
    , mh_filetype   :: MH_FILETYPE -- ^ Type of Mach-o file.
    , mh_flags      :: [MH_FLAGS]  -- ^ Flags.
    } deriving (Show, Eq)
    
data MH_FILETYPE
    = MH_OBJECT                   -- ^ relocatable object file
    | MH_EXECUTE                  -- ^ demand paged executable file
    | MH_CORE                     -- ^ core file
    | MH_PRELOAD                  -- ^ preloaded executable file
    | MH_DYLIB                    -- ^ dynamically bound shared library
    | MH_DYLINKER                 -- ^ dynamic link editor
    | MH_BUNDLE                   -- ^ dynamically bound bundle file
    | MH_DYLIB_STUB               -- ^ shared library stub for static. linking only, no section contents
    | MH_DSYM                     -- ^ companion file with only debug. sections
    | MH_KEXT_BUNDLE
    deriving (Ord, Show, Eq, Enum)
    
mach_filetype 0x1 = MH_OBJECT
mach_filetype 0x2 = MH_EXECUTE
mach_filetype 0x4 = MH_CORE
mach_filetype 0x5 = MH_PRELOAD
mach_filetype 0x6 = MH_DYLIB
mach_filetype 0x7 = MH_DYLINKER
mach_filetype 0x8 = MH_BUNDLE
mach_filetype 0x9 = MH_DYLIB_STUB
mach_filetype 0xa = MH_DSYM
mach_filetype 0xb = MH_KEXT_BUNDLE

data MH_FLAGS
    = MH_NOUNDEFS                -- ^ the object file has no undefined references
    | MH_INCRLINK                -- ^ the object file is the output of an incremental link against a base file and can't be link edited again
    | MH_DYLDLINK                -- ^ the object file is input for the dynamic linker and can't be staticly link edited again
    | MH_BINDATLOAD              -- ^ the object file's undefined references are bound by the dynamic linker when loaded.
    | MH_PREBOUND                -- ^ the file has its dynamic undefined references prebound.
    | MH_SPLIT_SEGS              -- ^ the file has its read-only and read-write segments split
    | MH_LAZY_INIT
    | MH_TWOLEVEL                -- ^ the image is using two-level name space bindings
    | MH_FORCE_FLAT              -- ^ the executable is forcing all images to use flat name space bindings
    | MH_NOMULTIDEFS             -- ^ this umbrella guarantees no multiple defintions of symbols in its sub-images so the two-level namespace hints can always be used.
    | MH_NOFIXPREBINDING         -- ^ do not have dyld notify the prebinding agent about this executable
    | MH_PREBINDABLE             -- ^ the binary is not prebound but can have its prebinding redone. only used when MH_PREBOUND is not set.
    | MH_ALLMODSBOUND            -- ^ indicates that this binary binds to all two-level namespace modules of its dependent libraries. only used when MH_PREBINDABLE and MH_TWOLEVEL are both set.
    | MH_SUBSECTIONS_VIA_SYMBOLS -- ^ safe to divide up the sections into sub-sections via symbols for dead code stripping
    | MH_CANONICAL               -- ^ the binary has been canonicalized via the unprebind operation
    | MH_WEAK_DEFINES            -- ^ the final linked image contains external weak symbols
    | MH_BINDS_TO_WEAK           -- ^ the final linked image uses weak symbols
    | MH_ALLOW_STACK_EXECUTION   -- ^ When this bit is set, all stacks  in the task will be given stack execution privilege.  Only used in MH_EXECUTE filetypes.
    | MH_DEAD_STRIPPABLE_DYLIB
    | MH_ROOT_SAFE               -- ^ When this bit is set, the binary  declares it is safe for use in processes with uid zero
    | MH_SETUID_SAFE             -- ^ When this bit is set, the binary  declares it is safe for use in processes when issetugid() is true
    | MH_NO_REEXPORTED_DYLIBS    -- ^ When this bit is set on a dylib,  the static linker does not need to examine dependent dylibs to see if any are re-exported
    | MH_PIE                     -- ^ When this bit is set, the OS will load the main executable at a random address.  Only used in MH_EXECUTE filetypes.
    deriving (Ord, Show, Eq, Enum)
    
data LC_COMMAND
    = LC_SEGMENT MachoSegment                        -- ^ segment of this file to be mapped
    | LC_SYMTAB [MachoSymbol] B.ByteString           -- ^ static link-edit symbol table and stab info
    | LC_SYMSEG
    | LC_THREAD [(Word32, [Word32])]                 -- ^ thread state information (list of (flavor, [long]) pairs)
    | LC_UNIXTHREAD [(Word32, [Word32])]             -- ^ unix thread state information (includes a stack) (list of (flavor, [long] pairs)
    | LC_LOADFVMLIB
    | LC_IDFVMLIB
    | LC_IDENT
    | LC_FVMFILE
    | LC_PREPAGE
    | LC_DYSYMTAB MachoDynamicSymbolTable            -- ^ dynamic link-edit symbol table info
    | LC_LOAD_DYLIB String Word32 Word32 Word32      -- ^ load a dynamically linked shared library (name, timestamp, current version, compatibility version)
    | LC_ID_DYLIB String Word32 Word32 Word32        -- ^ dynamically linked shared lib ident (name, timestamp, current version, compatibility version)
    | LC_LOAD_DYLINKER String                        -- ^ load a dynamic linker (name of dynamic linker)
    | LC_ID_DYLINKER String                          -- ^ dynamic linker identification (name of dynamic linker)
    | LC_PREBOUND_DYLIB String [Word8]               -- ^ modules prebound for a dynamically linked shared library (name, list of module indices)
    | LC_ROUTINES Word32 Word32                      -- ^ image routines (virtual address of initialization routine, module index where it resides)
    | LC_SUB_FRAMEWORK String                        -- ^ sub framework (name)
    | LC_SUB_UMBRELLA String                         -- ^ sub umbrella (name)
    | LC_SUB_CLIENT String                           -- ^ sub client (name)
    | LC_SUB_LIBRARY String                          -- ^ sub library (name)
    | LC_TWOLEVEL_HINTS [(Word32, Word32)]           -- ^ two-level namespace lookup hints (list of (subimage index, symbol table index) pairs
    | LC_PREBIND_CKSUM Word32                        -- ^ prebind checksum (checksum)

    | LC_LOAD_WEAK_DYLIB String Word32 Word32 Word32 -- ^ load a dynamically linked shared library that is allowed to be missing (symbols are weak imported) (name, timestamp, current version, compatibility version)
    | LC_SEGMENT_64 MachoSegment                     -- ^ 64-bit segment of this file to mapped
    | LC_ROUTINES_64 Word64 Word64                   -- ^ 64-bit image routines (virtual address of initialization routine, module index where it resides)
    | LC_UUID [Word8]                                -- ^ the uuid for an image or its corresponding dsym file (8 element list of bytes)
    | LC_RPATH String                                -- ^ runpath additions (path)
    | LC_CODE_SIGNATURE Word32 Word32                -- ^ local of code signature
    | LC_SEGMENT_SPLIT_INFO Word32 Word32            -- ^ local of info to split segments
    | LC_REEXPORT_DYLIB
    | LC_LAZY_LOAD_DYLIB
    | LC_ENCRYPTION_INFO Word32 B.ByteString
    | LC_DYLD_INFO
    | LC_DYLD_INFO_ONLY
    deriving (Show, Eq)

data VM_PROT
    = VM_PROT_READ    -- ^ read permission
    | VM_PROT_WRITE   -- ^ write permission
    | VM_PROT_EXECUTE -- ^ execute permission
    deriving (Ord, Show, Eq, Enum)
    
data MachoSegment = MachoSegment
    { seg_segname  :: String         -- ^ segment name
    , seg_vmaddr   :: Word64         -- ^ virtual address where the segment is loaded
    , seg_vmsize   :: Word64         -- ^ size of segment at runtime
    , seg_fileoff  :: Word64         -- ^ file offset of the segment
    , seg_filesize :: Word64         -- ^ size of segment in file
    , seg_maxprot  :: [VM_PROT]      -- ^ maximum virtual memory protection
    , seg_initprot :: [VM_PROT]      -- ^ initial virtual memory protection
    , seg_flags    :: [SG_FLAG]      -- ^ segment flags
    , seg_sections :: [MachoSection] -- ^ sections owned by this segment
    } deriving (Show, Eq)

data Macho = Macho
    { m_header   :: MachoHeader  -- ^ Header information.
    , m_commands :: [LC_COMMAND] -- ^ List of load commands describing Mach-O contents.
    } deriving (Show, Eq)

data SG_FLAG
    = SG_HIGHVM  -- ^ The file contents for this segment is for the high part of the VM space, the low part is zero filled (for stacks in core files).
    | SG_NORELOC -- ^ This segment has nothing that was relocated in it and nothing relocated to it, that is it may be safely replaced without relocation.
    deriving (Show, Eq)
  
data MachoSection = MachoSection
    { sec_sectname    :: String        -- ^ name of section
    , sec_segname     :: String        -- ^ name of segment that should own this section
    , sec_addr        :: Word64        -- ^ virtual memoy address for section
    , sec_size        :: Word64        -- ^ size of section
    , sec_align       :: Int           -- ^ alignment required by section (literal form, not power of two, e.g. 8 not 3)
    , sec_relocs      :: [Relocation]  -- ^ relocations for this section
    , sec_type        :: S_TYPE        -- ^ type of section
    , sec_user_attrs  :: [S_USER_ATTR] -- ^ user attributes of section
    , sec_sys_attrs   :: [S_SYS_ATTR]  -- ^ system attibutes of section
    } deriving (Show, Eq)

data S_TYPE
    = S_REGULAR                    -- ^ regular section
    | S_ZEROFILL                   -- ^ zero fill on demand section
    | S_CSTRING_LITERALS           -- ^ section with only literal C strings
    | S_4BYTE_LITERALS             -- ^ section with only 4 byte literals
    | S_8BYTE_LITERALS             -- ^ section with only 8 byte literals
    | S_LITERAL_POINTERS           -- ^ section with only pointers to literals
    | S_NON_LAZY_SYMBOL_POINTERS   -- ^ section with only non-lazy symbol pointers
    | S_LAZY_SYMBOL_POINTERS       -- ^ section with only lazy symbol pointers
    | S_SYMBOL_STUBS               -- ^ section with only symbol stubs, bte size of stub in the reserved2 field
    | S_MOD_INIT_FUNC_POINTERS     -- ^ section with only function pointers for initialization
    | S_MOD_TERM_FUNC_POINTERS     -- ^ section with only function pointers for termination
    | S_COALESCED                  -- ^ section contains symbols that are to be coalesced
    | S_GB_ZEROFILL                -- ^ zero fill on demand section (that can be larger than 4 gigabytes)
    | S_INTERPOSING                -- ^ section with only pairs of function pointers for interposing
    | S_16BYTE_LITERALS            -- ^ section with only 16 byte literals
    | S_DTRACE_DOF                 -- ^ section contains DTrace Object Format
    | S_LAZY_DYLIB_SYMBOL_POINTERS -- ^ section with only lazy symbol pointers to lazy loaded dylibs
    deriving (Show, Eq)
    
sectionType flags = case flags .&. 0x000000ff of
    0x00 -> S_REGULAR
    0x01 -> S_ZEROFILL
    0x02 -> S_CSTRING_LITERALS
    0x03 -> S_4BYTE_LITERALS
    0x04 -> S_8BYTE_LITERALS
    0x05 -> S_LITERAL_POINTERS
    0x06 -> S_NON_LAZY_SYMBOL_POINTERS
    0x07 -> S_LAZY_SYMBOL_POINTERS
    0x08 -> S_SYMBOL_STUBS
    0x09 -> S_MOD_INIT_FUNC_POINTERS
    0x0a -> S_MOD_TERM_FUNC_POINTERS
    0x0b -> S_COALESCED
    0x0c -> S_GB_ZEROFILL
    0x0d -> S_INTERPOSING
    0x0e -> S_16BYTE_LITERALS
    0x0f -> S_DTRACE_DOF
    0x10 -> S_LAZY_DYLIB_SYMBOL_POINTERS

data S_USER_ATTR
    = S_ATTR_PURE_INSTRUCTIONS   -- ^ section contains only true machine instructions
    | S_ATTR_NO_TOC              -- ^ setion contains coalesced symbols that are not to be in a ranlib table of contents
    | S_ATTR_STRIP_STATIC_SYMS   -- ^ ok to strip static symbols in this section in files with the MH_DYLDLINK flag
    | S_ATTR_NO_DEAD_STRIP       -- ^ no dead stripping
    | S_ATTR_LIVE_SUPPORT        -- ^ blocks are live if they reference live blocks
    | S_ATTR_SELF_MODIFYING_CODE -- ^ used with i386 code stubs written on by dyld
    | S_ATTR_DEBUG               -- ^ a debug section
    deriving (Show, Eq)
    
data S_SYS_ATTR
    = S_ATTR_SOME_INSTRUCTIONS -- ^ section contains soem machine instructions
    | S_ATTR_EXT_RELOC         -- ^ section has external relocation entries
    | S_ATTR_LOC_RELOC         -- ^ section has local relocation entries
    deriving (Show, Eq)
    
data N_TYPE
    = N_UNDF       -- ^ undefined symbol, n_sect is 0
    | N_ABS        -- ^ absolute symbol, does not need relocation, n_sect is 0
    | N_SECT       -- ^ symbol is defined in section n_sect
    | N_PBUD       -- ^ symbol is undefined and the image is using a prebound value for the symbol, n_sect is 0
    | N_INDR       -- ^ symbol is defined to be the same as another symbol. n_value is a string table offset indicating the name of that symbol
    | N_GSYM       -- ^ stab global symbol: name,,0,type,0
    | N_FNAME      -- ^ stab procedure name (f77 kludge): name,,0,0,0
    | N_FUN        -- ^ stab procedure: name,,n_sect,linenumber,address
    | N_STSYM      -- ^ stab static symbol: name,,n_sect,type,address
    | N_LCSYM      -- ^ stab .lcomm symbol: name,,n_sect,type,address
    | N_BNSYM      -- ^ stab begin nsect sym: 0,,n_sect,0,address
    | N_OPT        -- ^ stab emitted with gcc2_compiled and in gcc source
    | N_RSYM       -- ^ stab register sym: name,,0,type,register
    | N_SLINE      -- ^ stab src line: 0,,n_sect,linenumber,address
    | N_ENSYM      -- ^ stab end nsect sym: 0,,n_sect,0,address
    | N_SSYM       -- ^ stab structure elt: name,,0,type,struct_offset
    | N_SO         -- ^ stab source file name: name,,n_sect,0,address
    | N_OSO        -- ^ stab object file name: name,,0,0,st_mtime
    | N_LSYM       -- ^ stab local sym: name,,0,type,offset
    | N_BINCL      -- ^ stab include file beginning: name,,0,0,sum
    | N_SOL        -- ^ stab #included file name: name,,n_sect,0,address
    | N_PARAMS     -- ^ stab compiler parameters: name,,0,0,0
    | N_VERSION    -- ^ stab compiler version: name,,0,0,0
    | N_OLEVEL     -- ^ stab compiler -O level: name,,0,0,0
    | N_PSYM       -- ^ stab parameter: name,,0,type,offset
    | N_EINCL      -- ^ stab include file end: name,,0,0,0
    | N_ENTRY      -- ^ stab alternate entry: name,,n_sect,linenumber,address
    | N_LBRAC      -- ^ stab left bracket: 0,,0,nesting level,address
    | N_EXCL       -- ^ stab deleted include file: name,,0,0,sum
    | N_RBRAC      -- ^ stab right bracket: 0,,0,nesting level,address
    | N_BCOMM      -- ^ stab begin common: name,,0,0,0
    | N_ECOMM      -- ^ stab end common: name,,n_sect,0,0
    | N_ECOML      -- ^ stab end common (local name): 0,,n_sect,0,address
    | N_LENG       -- ^ stab second stab entry with length information
    | N_PC         -- ^ stab global pascal symbol: name,,0,subtype,line
    deriving (Show, Eq)

n_type 0x00 = N_UNDF
n_type 0x01 = N_ABS
n_type 0x07 = N_SECT
n_type 0x06 = N_PBUD
n_type 0x05 = N_INDR
n_type 0x20 = N_GSYM
n_type 0x22 = N_FNAME
n_type 0x24 = N_FUN
n_type 0x26 = N_STSYM
n_type 0x28 = N_LCSYM
n_type 0x2e = N_BNSYM
n_type 0x3c = N_OPT
n_type 0x40 = N_RSYM
n_type 0x44 = N_SLINE
n_type 0x4e = N_ENSYM
n_type 0x60 = N_SSYM
n_type 0x64 = N_SO
n_type 0x66 = N_OSO
n_type 0x80 = N_LSYM
n_type 0x82 = N_BINCL
n_type 0x84 = N_SOL
n_type 0x86 = N_PARAMS
n_type 0x88 = N_VERSION
n_type 0x8A = N_OLEVEL
n_type 0xa0 = N_PSYM
n_type 0xa2 = N_EINCL
n_type 0xa4 = N_ENTRY
n_type 0xc0 = N_LBRAC
n_type 0xc2 = N_EXCL
n_type 0xe0 = N_RBRAC
n_type 0xe2 = N_BCOMM
n_type 0xe4 = N_ECOMM
n_type 0xe8 = N_ECOML
n_type 0xfe = N_LENG
n_type 0x30 = N_PC

data REFERENCE_FLAG
    = REFERENCE_FLAG_UNDEFINED_NON_LAZY          -- ^ reference to an external non-lazy symbol
    | REFERENCE_FLAG_UNDEFINED_LAZY              -- ^ reference to an external lazy symbol
    | REFERENCE_FLAG_DEFINED                     -- ^ symbol is defined in this module
    | REFERENCE_FLAG_PRIVATE_DEFINED             -- ^ symbol is defined in this module and visible only to modules within this shared library
    | REFERENCE_FLAG_PRIVATE_UNDEFINED_NON_LAZY  -- ^ reference to an external non-lazy symbol and visible only to modules within this shared library
    | REFERENCE_FLAG_PRIVATE_UNDEFINED_LAZY      -- ^ reference to an external lazy symbol and visible only to modules within this shared library
    | REFERENCED_DYNAMICALLY                     -- ^ set for all symbols referenced by dynamic loader APIs
    | N_WEAK_REF                                 -- ^ indicates the symbol is a weak reference, set to 0 if definition cannot be found
    | N_WEAK_DEF                                 -- ^ indicates the symbol is a weak definition, will be overridden by a strong definition at link-time
    | LIBRARY_ORDINAL Word16                     -- ^ for two-level mach-o objects, specifies the index of the library in which this symbol is defined. zero specifies current image.
    deriving (Show, Eq)
reference_flag_lo16 0 = REFERENCE_FLAG_UNDEFINED_NON_LAZY
reference_flag_lo16 1 = REFERENCE_FLAG_UNDEFINED_LAZY
reference_flag_lo16 2 = REFERENCE_FLAG_DEFINED
reference_flag_lo16 3 = REFERENCE_FLAG_PRIVATE_DEFINED
reference_flag_lo16 4 = REFERENCE_FLAG_PRIVATE_UNDEFINED_NON_LAZY
reference_flag_lo16 5 = REFERENCE_FLAG_PRIVATE_UNDEFINED_LAZY
reference_flag_hi16 word = reference_flag_hi16_ 16 word
    where reference_flag_hi16_ 0 word = []
          reference_flag_hi16_ 1 word | testBit word 0 = REFERENCED_DYNAMICALLY : reference_flag_hi16 0
          reference_flag_hi16_ 3 word | testBit word 2 = N_WEAK_REF             : reference_flag_hi16 1
          reference_flag_hi16_ 4 word | testBit word 3 = N_WEAK_DEF             : reference_flag_hi16 1
          reference_flag_hi16_ n word = reference_flag_hi16_ (n-1) word

data MachoSymbol = MachoSymbol
    { sym_name  :: String                         -- ^ symbol name
    , sym_type  :: N_TYPE                         -- ^ symbol type
    , sym_pext  :: Bool                           -- ^ true if limited global scope
    , sym_ext   :: Bool                           -- ^ true if external symbol
    , sym_sect  :: Word8                          -- ^ section index where the symbol can be found
    , sym_flags :: Either Word16 [REFERENCE_FLAG] -- ^ for stab entries, Left Word16 is the uninterpreted flags field, otherwise Right [REFERENCE_FLAG] are the symbol flags
    , sym_value :: Word64                         -- ^ symbol value, 32-bit symbol values are promoted to 64-bit for simpliciy
    } deriving (Show, Eq)

data DylibModule = DylibModule
    { dylib_module_name_offset    :: Word32           -- ^ module name string table offset
    , dylib_ext_def_sym           :: (Word32, Word32) -- ^ (initial, count) pair of symbol table indices for externally defined symbols
    , dylib_ref_sym               :: (Word32, Word32) -- ^ (initial, count) pair of symbol table indices for referenced symbols
    , dylib_local_sym             :: (Word32, Word32) -- ^ (initial, count) pair of symbol table indices for local symbols
    , dylib_ext_rel               :: (Word32, Word32) -- ^ (initial, count) pair of symbol table indices for externally referenced symbols
    , dylib_init                  :: (Word32, Word32) -- ^ (initial, count) pair of symbol table indices for the index of the module init section and the number of init pointers
    , dylib_term                  :: (Word32, Word32) -- ^ (initial, count) pair of symbol table indices for the index of the module term section and the number of term pointers
    , dylib_objc_module_info_addr :: Word64           -- ^ statically linked address of the start of the data for this module in the __module_info section in the __OBJC segment
    , dylib_objc_module_info_size :: Word32           -- ^ number of bytes of data for this module that are used in the __module_info section in the __OBJC segment
    } deriving (Show, Eq)

-- | Platform-specific relocation types.
data R_TYPE
    = GENERIC_RELOC_VANILLA
    | GENERIC_RELOC_PAIR
    | GENERIC_RELOC_SECTDIFF
    | GENERIC_RELOC_LOCAL_SECTDIFF
    | GENERIC_RELOC_PB_LA_PTR
    | ARM_RELOC_VANILLA
    | ARM_RELOC_PAIR
    | ARM_RELOC_SECTDIFF
    | ARM_RELOC_LOCAL_SECTDIFF
    | ARM_RELOC_PB_LA_PTR
    | ARM_RELOC_BR24
    | ARM_THUMB_RELOC_BR22
    | X86_64_RELOC_BRANCH
    | X86_64_RELOC_GOT_LOAD
    | X86_64_RELOC_GOT
    | X86_64_RELOC_SIGNED
    | X86_64_RELOC_UNSIGNED
    | X86_64_RELOC_SUBTRACTOR
    | X86_64_RELOC_SIGNED_1
    | X86_64_RELOC_SIGNED_2
    | X86_64_RELOC_SIGNED_4
    | PPC_RELOC_VANILLA
    | PPC_RELOC_PAIR
    | PPC_RELOC_BR14
    | PPC_RELOC_BR24
    | PPC_RELOC_HI16
    | PPC_RELOC_LO16
    | PPC_RELOC_HA16
    | PPC_RELOC_LO14
    | PPC_RELOC_SECTDIFF
    | PPC_RELOC_LOCAL_SECTDIFF
    | PPC_RELOC_PB_LA_PTR
    | PPC_RELOC_HI16_SECTDIFF
    | PPC_RELOC_LO16_SECTDIFF
    | PPC_RELOC_HA16_SECTDIFF
    | PPC_RELOC_JBSR
    | PPC_RELOC_LO14_SECTDIFF
    deriving (Ord, Show, Eq, Enum)


r_type 0 CPU_TYPE_X86        = GENERIC_RELOC_VANILLA
r_type 1 CPU_TYPE_X86        = GENERIC_RELOC_PAIR
r_type 2 CPU_TYPE_X86        = GENERIC_RELOC_SECTDIFF
r_type 3 CPU_TYPE_X86        = GENERIC_RELOC_LOCAL_SECTDIFF
r_type 4 CPU_TYPE_X86        = GENERIC_RELOC_PB_LA_PTR
r_type 0 CPU_TYPE_ARM        = ARM_RELOC_VANILLA
r_type 1 CPU_TYPE_ARM        = ARM_RELOC_PAIR
r_type 2 CPU_TYPE_ARM        = ARM_RELOC_SECTDIFF
r_type 3 CPU_TYPE_ARM        = ARM_RELOC_LOCAL_SECTDIFF
r_type 4 CPU_TYPE_ARM        = ARM_RELOC_PB_LA_PTR
r_type 5 CPU_TYPE_ARM        = ARM_RELOC_BR24
r_type 6 CPU_TYPE_ARM        = ARM_THUMB_RELOC_BR22
r_type 0 CPU_TYPE_X86_64     = X86_64_RELOC_UNSIGNED
r_type 1 CPU_TYPE_X86_64     = X86_64_RELOC_SIGNED
r_type 2 CPU_TYPE_X86_64     = X86_64_RELOC_BRANCH
r_type 3 CPU_TYPE_X86_64     = X86_64_RELOC_GOT_LOAD
r_type 4 CPU_TYPE_X86_64     = X86_64_RELOC_GOT
r_type 5 CPU_TYPE_X86_64     = X86_64_RELOC_SUBTRACTOR
r_type 6 CPU_TYPE_X86_64     = X86_64_RELOC_SIGNED_1
r_type 7 CPU_TYPE_X86_64     = X86_64_RELOC_SIGNED_2
r_type 8 CPU_TYPE_X86_64     = X86_64_RELOC_SIGNED_4
r_type 0 CPU_TYPE_POWERPC    = PPC_RELOC_VANILLA
r_type 1 CPU_TYPE_POWERPC    = PPC_RELOC_PAIR
r_type 2 CPU_TYPE_POWERPC    = PPC_RELOC_BR14
r_type 3 CPU_TYPE_POWERPC    = PPC_RELOC_BR24
r_type 4 CPU_TYPE_POWERPC    = PPC_RELOC_HI16
r_type 5 CPU_TYPE_POWERPC    = PPC_RELOC_LO16
r_type 6 CPU_TYPE_POWERPC    = PPC_RELOC_HA16
r_type 7 CPU_TYPE_POWERPC    = PPC_RELOC_LO14
r_type 8 CPU_TYPE_POWERPC    = PPC_RELOC_SECTDIFF
r_type 9 CPU_TYPE_POWERPC    = PPC_RELOC_PB_LA_PTR
r_type 10 CPU_TYPE_POWERPC   = PPC_RELOC_HI16_SECTDIFF
r_type 11 CPU_TYPE_POWERPC   = PPC_RELOC_LO16_SECTDIFF
r_type 12 CPU_TYPE_POWERPC   = PPC_RELOC_HA16_SECTDIFF
r_type 13 CPU_TYPE_POWERPC   = PPC_RELOC_JBSR
r_type 14 CPU_TYPE_POWERPC   = PPC_RELOC_LO14_SECTDIFF
r_type 15 CPU_TYPE_POWERPC   = PPC_RELOC_LOCAL_SECTDIFF
r_type 0 CPU_TYPE_POWERPC64  = PPC_RELOC_VANILLA
r_type 1 CPU_TYPE_POWERPC64  = PPC_RELOC_PAIR
r_type 2 CPU_TYPE_POWERPC64  = PPC_RELOC_BR14
r_type 3 CPU_TYPE_POWERPC64  = PPC_RELOC_BR24
r_type 4 CPU_TYPE_POWERPC64  = PPC_RELOC_HI16
r_type 5 CPU_TYPE_POWERPC64  = PPC_RELOC_LO16
r_type 6 CPU_TYPE_POWERPC64  = PPC_RELOC_HA16
r_type 7 CPU_TYPE_POWERPC64  = PPC_RELOC_LO14
r_type 8 CPU_TYPE_POWERPC64  = PPC_RELOC_SECTDIFF
r_type 9 CPU_TYPE_POWERPC64  = PPC_RELOC_PB_LA_PTR
r_type 10 CPU_TYPE_POWERPC64 = PPC_RELOC_HI16_SECTDIFF
r_type 11 CPU_TYPE_POWERPC64 = PPC_RELOC_LO16_SECTDIFF
r_type 12 CPU_TYPE_POWERPC64 = PPC_RELOC_HA16_SECTDIFF
r_type 13 CPU_TYPE_POWERPC64 = PPC_RELOC_JBSR
r_type 14 CPU_TYPE_POWERPC64 = PPC_RELOC_LO14_SECTDIFF
r_type 15 CPU_TYPE_POWERPC64 = PPC_RELOC_LOCAL_SECTDIFF

data Relocation
    = RelocationInfo
        { ri_address   :: Int32  -- ^ offset from start of section to place to be relocated
        , ri_symbolnum :: Word32 -- ^ index into symbol or section table
        , ri_pcrel     :: Bool   -- ^ indicates if the item to be relocated is part of an instruction containing PC-relative addressing
        , ri_length    :: Word32 -- ^ length of item containing address to be relocated (literal form (4) instead of power of two (2))
        , ri_extern    :: Bool   -- ^ indicates whether symbolnum is an index into the symbol table (True) or section table (False)
        , ri_type      :: R_TYPE -- ^ relocation type
        }
    | ScatteredRelocationInfo
        { rs_pcrel   :: Bool   -- ^ indicates if the item to be relocated is part of an instruction containing PC-relative addressing
        , rs_length  :: Word32 -- ^ length of item containing address to be relocated (literal form (4) instead of power of two (2))
        , rs_type    :: R_TYPE -- ^ relocation type
        , rs_address :: Word32 -- ^ offset from start of section to place to be relocated
        , rs_value   :: Int32  -- ^ address of the relocatable expression for the item in the file that needs to be updated if the address is changed
        }
    deriving (Show, Eq)
    
data MachoDynamicSymbolTable = MachoDynamicSymbolTable
    { localSyms    :: (Word32, Word32)   -- ^ symbol table index and count for local symbols
    , extDefSyms   :: (Word32, Word32)   -- ^ symbol table index and count for externally defined symbols
    , undefSyms    :: (Word32, Word32)   -- ^ symbol table index and count for undefined symbols
    , tocEntries   :: [(Word32, Word32)] -- ^ list of symbol index and module index pairs
    , modules      :: [DylibModule]      -- ^ modules
    , extRefSyms   :: [Word32]           -- ^ list of external reference symbol indices
    , indirectSyms :: [Word32]           -- ^ list of indirect symbol indices
    , extRels      :: [Relocation]       -- ^ external locations
    , locRels      :: [Relocation]       -- ^ local relocations
    } deriving (Show, Eq)
