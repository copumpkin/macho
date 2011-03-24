{-# OPTIONS_GHC -F -pgmF she #-}
{-# LANGUAGE RecordWildCards #-}
-- | Data.Macho is a module for parsing a ByteString of a Mach-O file into a Macho record.
module Data.Macho ( parseMacho
                  , Macho(..)
                  , MachoHeader(..)
                  , LC_COMMAND(..)
                  , CPU_TYPE(..)
                  , CPU_SUBTYPE(..)
                  , MH_FLAGS(..)
                  , VM_PROT(..)
                  , MachoSegment(..)
                  , SG_FLAG(..)
                  , MachoSection(..)
                  , S_TYPE(..)
                  , S_USER_ATTR(..)
                  , S_SYS_ATTR(..)
                  , N_TYPE(..)
                  , REFERENCE_FLAG(..)
                  , MachoSymbol(..)
                  , DylibModule(..)
                  , R_TYPE(..)
                  , Relocation(..)
                  , MachoDynamicSymbolTable(..)
                  , MH_FILETYPE(..)) where

import Data.Binary hiding (decode)
import Data.Binary.Get
import Data.Bits
import Data.Word
import Data.Int
import Numeric
import Control.Monad
import Control.Applicative
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as C
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy     as L

import Data.Macho.Types

getMachoHeader :: Get (MachoBinary, Int, Int, MachoHeader)
getMachoHeader = do
  magic      <- (| macho_to_magic getWord32le |)
  binary     <- (| (macho_binary magic) |)
  cputype    <- (| mach_to_cputype (_getWord32 binary) |)
  cpusubtype <- (| (mach_to_cpusubtype cputype) (_getWord32 binary) |)
  filetype   <- (| mach_filetype (_getWord32 binary) |)
  ncmds      <- (| fromIntegral (_getWord32 binary) |)
  sizeofcmds <- (| fromIntegral (_getWord32 binary) |)
  flags      <- runDecoder getMachHeaderFlags binary
  reserved   <- case magic of
      MH_MAGIC64 -> _getWord32 binary
      MH_CIGAM64 -> _getWord32 binary
      _          -> return 0
  headerSize <- (| fromIntegral bytesRead |)
  return (binary, sizeofcmds, headerSize, MachoHeader cputype cpusubtype filetype flags)

getLoadCommands fl mh = do
    empty <- lift isEmpty
    if empty then
        return []
     else do
        cmd     <- (| fromIntegral getWord32 |)
        cmdsize <- (| fromIntegral getWord32 |)
        lcdata  <- lift $ getByteString (cmdsize - 8)
        lc      <- decode (getLoadCommand cmd lcdata fl mh) (L.fromChunks [lcdata])
        rest    <- getLoadCommands fl mh
        return $ lc : rest

-- | Parse a ByteString of a Mach-O object into a Macho record.
parseMacho :: B.ByteString -> Macho
parseMacho b =
    let (mr, sizeofcmds, hdrSize, header) = runGet getMachoHeader $ L.fromChunks [b]
        commands = runGet (runDecoder (getLoadCommands b header) mr) $ L.fromChunks [B.take sizeofcmds $ B.drop hdrSize b]
    in Macho header commands


getMachHeaderFlags :: Decoder [MH_FLAGS]
getMachHeaderFlags = (| (getMachHeaderFlags_ 31) getWord32 |)
  where 
  getMachHeaderFlags_ :: Int -> Word32 -> [MH_FLAGS]
  getMachHeaderFlags_  0 word = []
  getMachHeaderFlags_  1 word | testBit word  0 = MH_NOUNDEFS                : getMachHeaderFlags_  0 word
  getMachHeaderFlags_  2 word | testBit word  1 = MH_INCRLINK                : getMachHeaderFlags_  1 word
  getMachHeaderFlags_  3 word | testBit word  2 = MH_DYLDLINK                : getMachHeaderFlags_  2 word
  getMachHeaderFlags_  4 word | testBit word  3 = MH_BINDATLOAD              : getMachHeaderFlags_  3 word
  getMachHeaderFlags_  5 word | testBit word  4 = MH_PREBOUND                : getMachHeaderFlags_  4 word
  getMachHeaderFlags_  6 word | testBit word  5 = MH_SPLIT_SEGS              : getMachHeaderFlags_  5 word
  getMachHeaderFlags_  8 word | testBit word  7 = MH_TWOLEVEL                : getMachHeaderFlags_  7 word
  getMachHeaderFlags_  9 word | testBit word  8 = MH_FORCE_FLAT              : getMachHeaderFlags_  8 word
  getMachHeaderFlags_ 10 word | testBit word  9 = MH_NOMULTIDEFS             : getMachHeaderFlags_  9 word
  getMachHeaderFlags_ 11 word | testBit word 10 = MH_NOFIXPREBINDING         : getMachHeaderFlags_ 10 word
  getMachHeaderFlags_ 12 word | testBit word 11 = MH_PREBINDABLE             : getMachHeaderFlags_ 11 word
  getMachHeaderFlags_ 13 word | testBit word 12 = MH_ALLMODSBOUND            : getMachHeaderFlags_ 12 word
  getMachHeaderFlags_ 14 word | testBit word 13 = MH_SUBSECTIONS_VIA_SYMBOLS : getMachHeaderFlags_ 13 word
  getMachHeaderFlags_ 15 word | testBit word 14 = MH_CANONICAL               : getMachHeaderFlags_ 14 word
  getMachHeaderFlags_ 16 word | testBit word 15 = MH_WEAK_DEFINES            : getMachHeaderFlags_ 15 word
  getMachHeaderFlags_ 17 word | testBit word 16 = MH_BINDS_TO_WEAK           : getMachHeaderFlags_ 16 word
  getMachHeaderFlags_ 18 word | testBit word 17 = MH_ALLOW_STACK_EXECUTION   : getMachHeaderFlags_ 17 word
  getMachHeaderFlags_ 19 word | testBit word 18 = MH_ROOT_SAFE               : getMachHeaderFlags_ 18 word
  getMachHeaderFlags_ 20 word | testBit word 19 = MH_SETUID_SAFE             : getMachHeaderFlags_ 19 word
  getMachHeaderFlags_ 21 word | testBit word 20 = MH_NO_REEXPORTED_DYLIBS    : getMachHeaderFlags_ 20 word
  getMachHeaderFlags_ 22 word | testBit word 21 = MH_PIE                     : getMachHeaderFlags_ 21 word
  getMachHeaderFlags_  n word = getMachHeaderFlags_ (n-1) word

getLoadCommand :: Word32 -> B.ByteString -> B.ByteString -> MachoHeader -> Decoder LC_COMMAND
getLoadCommand 0x00000001 lc fl mh = getSegmentCommand64 fl mh
getLoadCommand 0x00000002 lc fl mh = getSymTabCommand fl mh
getLoadCommand 0x00000004 lc fl mh = getThreadCommand LC_THREAD
getLoadCommand 0x00000005 lc fl mh = getThreadCommand LC_UNIXTHREAD
getLoadCommand 0x0000000b lc fl mh = getDySymTabCommand fl mh
getLoadCommand 0x0000000c lc fl mh = getDylibCommand lc LC_LOAD_DYLIB
getLoadCommand 0x0000000d lc fl mh = getDylibCommand lc LC_ID_DYLIB
getLoadCommand 0x0000000e lc fl mh = getDylinkerCommand lc LC_LOAD_DYLINKER
getLoadCommand 0x0000000f lc fl mh = getDylinkerCommand lc LC_ID_DYLINKER
getLoadCommand 0x00000010 lc fl mh = getPreboundDylibCommand lc
getLoadCommand 0x00000011 lc fl mh = getRoutinesCommand32
getLoadCommand 0x00000012 lc fl mh = getSubFrameworkCommand lc
getLoadCommand 0x00000013 lc fl mh = getSubUmbrellaCommand lc
getLoadCommand 0x00000014 lc fl mh = getSubClientCommand lc
getLoadCommand 0x00000015 lc fl mh = getSubLibraryCommand lc
getLoadCommand 0x00000016 lc fl mh = getTwoLevelHintsCommand fl
getLoadCommand 0x00000017 lc fl mh = getPrebindCkSumCommand
getLoadCommand 0x80000018 lc fl mh = getDylibCommand lc LC_LOAD_WEAK_DYLIB
getLoadCommand 0x00000019 lc fl mh = getSegmentCommand64 fl mh
getLoadCommand 0x0000001a lc fl mh = getRoutinesCommand64
getLoadCommand 0x0000001b lc fl mh = getUUIDCommand
getLoadCommand 0x8000001c lc fl mh = getRPathCommand lc
getLoadCommand 0x0000001d lc fl mh = getLinkEditCommand LC_CODE_SIGNATURE
getLoadCommand 0x0000001e lc fl mh = getLinkEditCommand LC_SEGMENT_SPLIT_INFO


getVM_PROT = (| (getVM_PROT_ 31) getWord32 |)
    where getVM_PROT_ 0 word = []
          getVM_PROT_ 1 word | testBit word 0 = VM_PROT_READ    : getVM_PROT_ 0 word
          getVM_PROT_ 2 word | testBit word 1 = VM_PROT_WRITE   : getVM_PROT_ 1 word
          getVM_PROT_ 3 word | testBit word 2 = VM_PROT_EXECUTE : getVM_PROT_ 2 word
          getVM_PROT_ n word = getVM_PROT_ (n-1) word


getSegmentCommand32 = getSegmentCommand LC_SEGMENT
getSegmentCommand64 = getSegmentCommand LC_SEGMENT_64

getSegmentCommand con fl mh = do
    segname  <- (| (takeWhile (/= '\0') . C.unpack) (lift $ getByteString 16) |)
    vmaddr   <- getWord
    vmsize   <- getWord
    fileoff  <- getWord
    filesize <- getWord
    maxprot  <- getVM_PROT
    initprot <- getVM_PROT
    nsects   <- (| fromIntegral getWord32 |)
    flags    <- getSG_FLAG
    sects    <- replicateM nsects (getSection fl mh)
    return $ con MachoSegment
                            { seg_segname = segname
                            , seg_vmaddr  = vmaddr
                            , seg_vmsize  = vmsize
                            , seg_fileoff = fileoff
                            , seg_filesize = filesize
                            , seg_maxprot  = maxprot
                            , seg_initprot = initprot
                            , seg_flags    = flags
                            , seg_sections = sects }

getSG_FLAG = (| (getSG_FLAG_ 31) getWord32 |)
    where getSG_FLAG_ 0 word = []
          getSG_FLAG_ 1 word | testBit word 0 = SG_HIGHVM  : getSG_FLAG_ 0 word
          getSG_FLAG_ 3 word | testBit word 2 = SG_NORELOC : getSG_FLAG_ 2 word
          getSG_FLAG_ n word = getSG_FLAG_ (n-1) word
          
getSection fl mh = do
  sectname  <- liftM (takeWhile (/= '\0') . C.unpack) $ lift (getByteString 16)
  segname   <- liftM (takeWhile (/= '\0') . C.unpack) $ lift (getByteString 16)
  addr      <- getWord
  size      <- getWord
  offset    <- getWord32
  align     <- liftM (2 ^) $ getWord32
  reloff    <- liftM fromIntegral $ getWord32
  nreloc    <- liftM fromIntegral $ getWord32
  relocs    <- decode (replicateM nreloc (getRel mh)) (L.fromChunks [B.drop reloff fl])
  flags     <- getWord32
  reserved1 <- getWord32
  reserved2 <- getWord
  sectype   <- return $ sectionType flags
  userattrs <- return $ sectionUserAttribute flags
  sysattrs  <- return $ sectionSystemAttribute flags
  return MachoSection { sec_sectname   = sectname
                      , sec_segname    = segname
                      , sec_addr       = addr
                      , sec_size       = size
                      , sec_align      = align
                      , sec_relocs     = relocs
                      , sec_type       = sectype
                      , sec_user_attrs = userattrs
                      , sec_sys_attrs  = sysattrs }

sectionUserAttribute flags = sectionUserAttribute_ 31 (flags .&. 0xff000000)
    where sectionUserAttribute_  0 flags = []
          sectionUserAttribute_ 31 flags | testBit flags 30 = S_ATTR_PURE_INSTRUCTIONS   : sectionUserAttribute_ 30 flags
          sectionUserAttribute_ 30 flags | testBit flags 29 = S_ATTR_NO_TOC              : sectionUserAttribute_ 29 flags
          sectionUserAttribute_ 29 flags | testBit flags 28 = S_ATTR_STRIP_STATIC_SYMS   : sectionUserAttribute_ 28 flags
          sectionUserAttribute_ 28 flags | testBit flags 27 = S_ATTR_NO_DEAD_STRIP       : sectionUserAttribute_ 27 flags
          sectionUserAttribute_ 27 flags | testBit flags 26 = S_ATTR_LIVE_SUPPORT        : sectionUserAttribute_ 26 flags
          sectionUserAttribute_ 26 flags | testBit flags 25 = S_ATTR_SELF_MODIFYING_CODE : sectionUserAttribute_ 25 flags
          sectionUserAttribute_  n flags = sectionUserAttribute_ (n-1) flags

sectionSystemAttribute flags = sectionSystemAttribute_ 31 (flags .&. 0x00ffff00)
    where sectionSystemAttribute_  0 flags = []
          sectionSystemAttribute_  8 flags | testBit flags 7 = S_ATTR_LOC_RELOC         : sectionSystemAttribute_  7 flags
          sectionSystemAttribute_  9 flags | testBit flags 8 = S_ATTR_EXT_RELOC         : sectionSystemAttribute_  8 flags
          sectionSystemAttribute_ 10 flags | testBit flags 9 = S_ATTR_SOME_INSTRUCTIONS : sectionSystemAttribute_  9 flags
          sectionSystemAttribute_  n flags = sectionSystemAttribute_ (n-1) flags

nullStringAt offset      = B.takeWhile ((/=) 0) . B.drop offset
substringAt  offset size = B.take size . B.drop offset
getLC_STR lc = do
    offset <- liftM fromIntegral $ getWord32
    return $ C.unpack $ nullStringAt offset lc

getDylibCommand :: B.ByteString -> (String -> Word32 -> Word32 -> Word32 -> LC_COMMAND) -> Decoder LC_COMMAND
getDylibCommand lc con = do
    name                  <- getLC_STR lc
    timestamp             <- getWord32 
    current_version       <- getWord32 
    compatibility_version <- getWord32 
    return $ con name timestamp current_version compatibility_version

getSubFrameworkCommand lc     = return LC_SUB_FRAMEWORK <*> getLC_STR lc
getSubClientCommand    lc     = return LC_SUB_CLIENT    <*> getLC_STR lc
getSubUmbrellaCommand  lc     = return LC_SUB_UMBRELLA  <*> getLC_STR lc
getSubLibraryCommand   lc     = return LC_SUB_LIBRARY   <*> getLC_STR lc
getDylinkerCommand     lc con = return con              <*> getLC_STR lc

getPreboundDylibCommand lc = do
    name           <- getLC_STR lc
    nmodules       <- liftM fromIntegral $ getWord32
    modules_offset <- liftM fromIntegral $ getWord32
    modules        <- return $ B.unpack $ B.take ((nmodules `div` 8) + (nmodules `mod` 8)) $ B.drop modules_offset lc
    return $ LC_PREBOUND_DYLIB name modules

getThreadCommand con = do
    let getThreadCommand_ = do
          empty <- lift isEmpty
          if empty then
              return []
           else do
              flavor <- getWord32
              count  <- liftM fromIntegral $ getWord32
              state  <- replicateM count getWord32 
              rest   <- getThreadCommand_
              return ((flavor, state) : rest)
    flavours <- getThreadCommand_
    return $ con flavours

getRoutinesCommand32 = do
    init_address <- getWord32 
    init_module  <- getWord32 
    replicateM_ 6 getWord32
    return $ LC_ROUTINES init_address init_module

getRoutinesCommand64 = do
    init_address <- getWord64
    init_module  <- getWord64
    replicateM_ 6 getWord64
    return $ LC_ROUTINES_64 init_address init_module



reference_flags word mh =
    if MH_TWOLEVEL `elem` mh_flags mh then
        [reference_flag_lo16 (word .&. 0xf), LIBRARY_ORDINAL ((word .&. 0xf0) `shiftR` 16)]
    else
        reference_flag_lo16 (word .&. 0xf) : reference_flag_hi16 word

n_types n = if n .&. 0xe0 == 0 then
               let npext = n .&. 0x10 /= 0
                   ntype = n_type ((n .&. 0x0e) `shiftR` 1)
                   next  = n .&. 0x01 /= 0
               in (False, npext, ntype, next)
           else
               (True, False, n_type n, False)


getSymbolName strsect = do
    offset <- liftM fromIntegral $ getWord32
    return $ C.unpack $ C.takeWhile (/= '\0') $ B.drop offset strsect

getNList :: B.ByteString -> MachoHeader -> Decoder MachoSymbol
getNList strsect mh = do
  n_name  <- getSymbolName strsect
  n_type  <- lift getWord8
  let (stabs, npext, ntype, next) = n_types n_type
  n_sect  <- lift getWord8
  n_desc  <- getWord16
  let ref_flags = if stabs then
                      Left n_desc
                  else
                      Right $ reference_flags n_desc mh
  n_value <- getWord
  return $ MachoSymbol n_name ntype npext next n_sect ref_flags n_value

getSymTabCommand :: B.ByteString -> MachoHeader -> Decoder LC_COMMAND
getSymTabCommand fl mh = do
  symoff  <- (| fromIntegral getWord32 |)
  nsyms   <- (| fromIntegral getWord32 |)
  stroff  <- (| fromIntegral getWord32 |)
  strsize <- (| fromIntegral getWord32 |)
  strsect <- return $ B.take strsize $ B.drop stroff fl
  symbols <- decode (replicateM nsyms (getNList strsect mh)) (L.fromChunks [B.drop symoff fl])
  return $ LC_SYMTAB symbols strsect

getTOC = do
  symbol_index <- getWord32
  module_index <- getWord32
  return (symbol_index, module_index)

getModule = do
    module_name           <- getWord32
    iextdefsym            <- getWord32
    nextdefsym            <- getWord32
    irefsym               <- getWord32
    nrefsym               <- getWord32
    ilocalsym             <- getWord32
    nlocalsym             <- getWord32
    iextrel               <- getWord32
    nextrel               <- getWord32
    iinit_iterm           <- getWord32
    iinit                 <- return (iinit_iterm .&. 0x0000ffff)
    iterm                 <- return $ (iinit_iterm .&. 0xffff0000) `shiftR` 16
    ninit_nterm           <- getWord32
    ninit                 <- return (ninit_nterm .&. 0x0000ffff)
    nterm                 <- return $ (ninit_nterm .&. 0xffff0000) `shiftR` 16
    objc_module_info_addr <- getWord32 -- TODO: is this correct?
    objc_module_info_size <- getWord
    return DylibModule
        { dylib_module_name_offset    = module_name
        , dylib_ext_def_sym           = (iextdefsym, nextdefsym)
        , dylib_ref_sym               = (irefsym, nrefsym)
        , dylib_local_sym             = (ilocalsym, nlocalsym)
        , dylib_ext_rel               = (iextrel, nextrel)
        , dylib_init                  = (iinit, ninit)
        , dylib_term                  = (iterm, nterm)
        , dylib_objc_module_info_addr = objc_module_info_addr
        , dylib_objc_module_info_size = objc_module_info_size
        }

getRel mh = do
    r_address <- getWord32
    r_value   <- getWord32
    if (r_address .&. 0x80000000) /= 0 then do
        rs_pcrel   <- (1 ==) <$> bitfield 1 1 r_address 
        rs_length  <- (2 ^) <$> bitfield 2 2 r_address
        rs_type    <- flip r_type (mh_cputype mh) <$> bitfield 4 4 r_address
        rs_address <- bitfield 8 24 r_address
        rs_value   <- return $ fromIntegral r_value
        return $ ScatteredRelocationInfo rs_pcrel rs_length rs_type rs_address rs_value
     else do
        ri_address   <- return $ fromIntegral r_address
        ri_symbolnum <- bitfield 0 24 r_value
        ri_pcrel     <- (1 ==) <$> bitfield 24 1 r_value
        ri_length    <- (2 ^) <$> bitfield 25 2 r_value
        ri_extern    <- (1 ==) <$> bitfield 27 1 r_value
        ri_type      <- flip r_type (mh_cputype mh) <$> bitfield 28 4 r_value
        return $ RelocationInfo ri_address ri_symbolnum ri_pcrel ri_length ri_extern ri_type


getDySymTabCommand fl mh = do
    ilocalsym      <- getWord32
    nlocalsym      <- getWord32
    iextdefsym     <- getWord32
    nextdefsym     <- getWord32
    iundefsym      <- getWord32
    nundefsym      <- getWord32
    tocoff         <- liftM fromIntegral $ getWord32
    ntoc           <- liftM fromIntegral $ getWord32
    toc            <- decode (replicateM ntoc getTOC) (L.fromChunks [B.drop tocoff fl])
    modtaboff      <- liftM fromIntegral $ getWord32
    nmodtab        <- liftM fromIntegral $ getWord32
    is64           <- is64bit
    modtab         <- decode (replicateM nmodtab (if is64 then getModule else getModule)) (L.fromChunks [B.drop modtaboff fl])
    extrefsymoff   <- liftM fromIntegral $ getWord32
    nextrefsyms    <- liftM fromIntegral $ getWord32
    extrefsyms     <- decode (replicateM nextrefsyms getWord32) (L.fromChunks [B.drop extrefsymoff fl])
    indirectsymoff <- liftM fromIntegral $ getWord32
    nindirectsyms  <- liftM fromIntegral $ getWord32
    indirectsyms   <- decode (replicateM nindirectsyms getWord32) (L.fromChunks [B.drop indirectsymoff fl])
    extreloff      <- liftM fromIntegral $ getWord32
    nextrel        <- liftM fromIntegral $ getWord32
    extrels        <- decode (replicateM nextrel (getRel mh)) (L.fromChunks [B.drop extreloff fl])
    locreloff      <- liftM fromIntegral $ getWord32
    nlocrel        <- liftM fromIntegral $ getWord32
    locrels        <- decode (replicateM nlocrel (getRel mh)) (L.fromChunks [B.drop locreloff fl])
    return $ LC_DYSYMTAB MachoDynamicSymbolTable
        { localSyms    = (ilocalsym, nlocalsym)
        , extDefSyms   = (iextdefsym, nextdefsym)
        , undefSyms    = (iundefsym, nundefsym)
        , tocEntries   = toc
        , modules      = modtab
        , extRefSyms   = extrefsyms
        , indirectSyms = indirectsyms
        , extRels      = extrels
        , locRels      = locrels
        }

getTwoLevelHint = do
    word <- getWord32
    isub_image <- bitfield 0 8 word
    itoc       <- bitfield 8 24 word
    return (isub_image, itoc)

getTwoLevelHintsCommand fl = do
    offset  <- liftM fromIntegral $ getWord32
    nhints  <- liftM fromIntegral $ getWord32
    LC_TWOLEVEL_HINTS <$> decode (replicateM nhints getTwoLevelHint) (L.fromChunks [B.drop offset fl])

getPrebindCkSumCommand = (| LC_PREBIND_CKSUM getWord32 |)

getUUIDCommand = (| LC_UUID (replicateM 8 (lift getWord8)) |)

getRPathCommand lc = do
    name_offset           <- liftM fromIntegral $ getWord32
    name                  <- return $ C.unpack $ nullStringAt name_offset lc
    return $ LC_RPATH name

getLinkEditCommand :: (Word32 -> Word32 -> LC_COMMAND) -> Decoder LC_COMMAND
getLinkEditCommand con = (| con getWord32 getWord32 |)
