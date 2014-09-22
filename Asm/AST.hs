module SFOT.Asm.AST where

import Prelude hiding (and)

import Data.Word
import Data.Bits hiding (bit)
import Data.Int
import Numeric

type ByteAddr = Word8
type Immediate = Word8
type WordAddr = Word16

data ShortJump =  ShortLabel String | RelAddr Int8 deriving (Show, Eq)

data LongJump = LongLabel String | AbsAddr WordAddr deriving (Show, Eq)

type Program = [Operation]

data Operation = ADC Adc
               | AND And
               | ASL Asl
               | BCC ShortJump
               | BCS ShortJump
               | BEQ ShortJump
               | BIT Bit
               | BMI ShortJump
               | BNE ShortJump
               | BPL ShortJump
               | BRK
               | BVC ShortJump
               | BVS ShortJump
               | CLC
               | CLD
               | CLI
               | CLV
               | CMP Cmp
               | CPX Cpx
               | CPY Cpy
               | DEC Dec
               | DEX
               | DEY
               | EOR Eor
               | INC Inc
               | INX
               | INY
               | JMP Jmp
               | JSR LongJump
               | Label String
               | LDA Lda
               | LDX Ldx
               | LDY Ldy
               | LSR Lsr
               | NOP
               | ORA Ora
               | PHA
               | PHP
               | PLA
               | PLP
               | ROL Rol
               | ROR Ror
               | RTI
               | RTS
               | SBC Sbc
               | SEC
               | SED
               | SEI
               | STA Sta
               | STX Stx
               | STY Sty
               | TAX
               | TAY
               | TSX
               | TXA
               | TXS
               | TYA
                 deriving (Show, Eq)


data Adc = AdcI  Immediate
         | AdcZ  ByteAddr
         | AdcZX ByteAddr
         | AdcA  WordAddr
         | AdcAX WordAddr
         | AdcAY WordAddr
         | AdcIX ByteAddr
         | AdcIY ByteAddr
           deriving (Show, Eq)

data And = AndI  Immediate
         | AndZ  ByteAddr
         | AndZX ByteAddr
         | AndA  WordAddr
         | AndAX WordAddr
         | AndAY WordAddr
         | AndIX ByteAddr
         | AndIY ByteAddr
           deriving (Show, Eq)

data Asl = AslAc
         | AslZ  ByteAddr
         | AslZX ByteAddr
         | AslA  WordAddr
         | AslAX WordAddr
           deriving (Show, Eq)

data Bit = BitZ  ByteAddr
         | BitA  WordAddr
           deriving (Show, Eq)

data Cmp = CmpI  Immediate
         | CmpZ  ByteAddr
         | CmpZX ByteAddr
         | CmpA  WordAddr
         | CmpAX WordAddr
         | CmpAY WordAddr
         | CmpIX ByteAddr
         | CmpIY ByteAddr
           deriving (Show, Eq)

data Cpx = CpxI  Immediate
         | CpxZ  ByteAddr
         | CpxA  WordAddr
           deriving (Show, Eq)

data Cpy = CpyI  Immediate
         | CpyZ  ByteAddr
         | CpyA  WordAddr
           deriving (Show, Eq)

data Dec = DecZ  ByteAddr
         | DecZX ByteAddr
         | DecA  WordAddr
         | DecAX WordAddr
           deriving (Show, Eq)

data Eor = EorI  Immediate
         | EorZ  ByteAddr
         | EorZX ByteAddr
         | EorA  WordAddr
         | EorAX WordAddr
         | EorAY WordAddr
         | EorIX ByteAddr
         | EorIY ByteAddr
           deriving (Show, Eq)

data Inc = IncZ  ByteAddr
         | IncZX ByteAddr
         | IncA  WordAddr
         | IncAX WordAddr
           deriving (Show, Eq)

data Jmp = JmpA LongJump
         | JmpI LongJump
           deriving (Show, Eq)

data Lda = LdaI Immediate
         | LdaZ ByteAddr
         | LdaZX ByteAddr
         | LdaA WordAddr
         | LdaAX WordAddr
         | LdaAY WordAddr
         | LdaIX ByteAddr
         | LdaIY ByteAddr
           deriving (Show, Eq)

data Ldx = LdxI Immediate
         | LdxZ ByteAddr
         | LdxZY ByteAddr
         | LdxA WordAddr
         | LdxAY WordAddr
           deriving (Show, Eq)

data Ldy = LdyI Immediate
         | LdyZ ByteAddr
         | LdyZX ByteAddr
         | LdyA WordAddr
         | LdyAX WordAddr
           deriving (Show, Eq)

data Lsr = LsrAc
         | LsrZ ByteAddr
         | LsrZX ByteAddr
         | LsrA WordAddr
         | LsrAX WordAddr
           deriving (Show, Eq)

data Ora = OraI Immediate
         | OraZ ByteAddr
         | OraZX ByteAddr
         | OraA WordAddr
         | OraAX WordAddr
         | OraAY WordAddr
         | OraIX ByteAddr
         | OraIY ByteAddr
           deriving (Show, Eq)

data Rol = RolAc
         | RolZ ByteAddr
         | RolZX ByteAddr
         | RolA WordAddr
         | RolAX WordAddr
           deriving (Show, Eq)

data Ror = RorAc
         | RorZ ByteAddr
         | RorZX ByteAddr
         | RorA WordAddr
         | RorAX WordAddr
           deriving (Show, Eq)

data Sbc = SbcI Immediate
         | SbcZ ByteAddr
         | SbcZX ByteAddr
         | SbcA WordAddr
         | SbcAX WordAddr
         | SbcAY WordAddr
         | SbcIX ByteAddr
         | SbcIY ByteAddr
           deriving (Show, Eq)

data Sta = StaZ ByteAddr
         | StaZX ByteAddr
         | StaA WordAddr
         | StaAX WordAddr
         | StaAY WordAddr
         | StaIX ByteAddr
         | StaIY ByteAddr
           deriving (Show, Eq)

data Stx = StxZ ByteAddr
         | StxZY ByteAddr
         | StxA WordAddr
           deriving (Show, Eq)

data Sty = StyZ ByteAddr
         | StyZX ByteAddr
         | StyA WordAddr
           deriving (Show, Eq)

opcode :: Operation -> [Word8]
opcode (ADC adc) = adcOpcode adc
opcode (AND and) = andOpcode and
opcode (ASL asl) = aslOpcode asl
opcode (BCC (RelAddr w8)) = [0x90, fromIntegral w8]
opcode (BCS (RelAddr w8)) = [0xB0, fromIntegral w8]
opcode (BEQ (RelAddr w8)) = [0xF0, fromIntegral w8]
opcode (BIT bit) = bitOpcode bit
opcode (BMI (RelAddr w8)) = [0x30, fromIntegral w8]
opcode (BNE (RelAddr w8)) = [0xD0, fromIntegral w8]
opcode (BPL (RelAddr w8)) = [0x10, fromIntegral w8]
opcode BRK       = [0x00]
opcode (BVC (RelAddr w8)) = [0x50, fromIntegral w8]
opcode (BVS (RelAddr w8)) = [0x70, fromIntegral w8]
opcode CLC       = [0x18]
opcode CLD       = [0xD8]
opcode CLI       = [0x58]
opcode CLV       = [0xB8]
opcode (CMP cmp) = cmpOpcode cmp
opcode (CPX cpx) = cpxOpcode cpx
opcode (CPY cpy) = cpyOpcode cpy
opcode DEX       = [0xCA]
opcode DEY       = [0x88]
opcode (DEC dec) = decOpcode dec
opcode (EOR eor) = eorOpcode eor
opcode (INC inc) = incOpcode inc
opcode INX       = [0xE8]
opcode INY       = [0xC8]
opcode (JMP jmp) = jmpOpcode jmp
opcode (JSR (AbsAddr w16)) = 0x20 : encodeWord16 w16
opcode (Label _) = []
opcode (LDA lda) = ldaOpcode lda
opcode (LDX ldx) = ldxOpcode ldx
opcode (LDY ldy) = ldyOpcode ldy
opcode (LSR lsr) = lsrOpcode lsr
opcode NOP       = [0xEA]
opcode (ORA ora) = oraOpcode ora
opcode PHA       = [0x48]
opcode PHP       = [0x08]
opcode PLA       = [0x68]
opcode PLP       = [0x28]
opcode (ROL rol) = rolOpcode rol
opcode (ROR ror) = rorOpcode ror
opcode RTI       = [0x4D]
opcode RTS       = [0x60]
opcode (SBC sbc) = sbcOpcode sbc
opcode SEC       = [0x38]
opcode SED       = [0xF8]
opcode SEI       = [0x78]
opcode (STA sta) = staOpcode sta
opcode (STX stx) = stxOpcode stx
opcode (STY sty) = styOpcode sty
opcode TAX       = [0xAA]
opcode TAY       = [0xA8]
opcode TSX       = [0xBA]
opcode TXA       = [0x8A]
opcode TXS       = [0x9A]
opcode TYA       = [0x98]
opcode x         = error $ "Opcode error: " ++ show x

adcOpcode :: Adc -> [Word8]
adcOpcode (AdcI   w8)  = 0x69 : encodeWord8   w8
adcOpcode (AdcZ   w8)  = 0x65 : encodeWord8   w8
adcOpcode (AdcZX  w8)  = 0x75 : encodeWord8   w8
adcOpcode (AdcA  w16)  = 0x6D : encodeWord16 w16
adcOpcode (AdcAX w16)  = 0x7D : encodeWord16 w16
adcOpcode (AdcAY w16)  = 0x79 : encodeWord16 w16
adcOpcode (AdcIX  w8)  = 0x61 : encodeWord8   w8
adcOpcode (AdcIY  w8)  = 0x71 : encodeWord8   w8

andOpcode :: And -> [Word8]
andOpcode (AndI   w8)  = 0x29 : encodeWord8   w8
andOpcode (AndZ   w8)  = 0x25 : encodeWord8   w8
andOpcode (AndZX  w8)  = 0x35 : encodeWord8   w8
andOpcode (AndA  w16)  = 0x2D : encodeWord16 w16
andOpcode (AndAX w16)  = 0x3D : encodeWord16 w16
andOpcode (AndAY w16)  = 0x39 : encodeWord16 w16
andOpcode (AndIX  w8)  = 0x21 : encodeWord8   w8
andOpcode (AndIY  w8)  = 0x31 : encodeWord8   w8

aslOpcode :: Asl -> [Word8]
aslOpcode AslAc        = 0x0A : []
aslOpcode (AslZ   w8)  = 0x06 : encodeWord8   w8
aslOpcode (AslZX  w8)  = 0x16 : encodeWord8   w8
aslOpcode (AslA  w16)  = 0x0E : encodeWord16 w16
aslOpcode (AslAX w16)  = 0x1E : encodeWord16 w16

bitOpcode :: Bit -> [Word8]
bitOpcode (BitZ   w8)  = 0x24 : encodeWord8   w8
bitOpcode (BitA w16)  = 0x2C : encodeWord16 w16

cmpOpcode :: Cmp -> [Word8]
cmpOpcode (CmpI   w8)  = 0xC9 : encodeWord8   w8
cmpOpcode (CmpZ   w8)  = 0xC5 : encodeWord8   w8
cmpOpcode (CmpZX  w8)  = 0xD5 : encodeWord8   w8
cmpOpcode (CmpA  w16)  = 0xCD : encodeWord16 w16
cmpOpcode (CmpAX w16)  = 0xDD : encodeWord16 w16
cmpOpcode (CmpAY w16)  = 0xD9 : encodeWord16 w16
cmpOpcode (CmpIX  w8)  = 0xC1 : encodeWord8   w8
cmpOpcode (CmpIY  w8)  = 0xD1 : encodeWord8   w8

cpxOpcode :: Cpx -> [Word8]
cpxOpcode (CpxI   w8)  = 0xE0 : encodeWord8   w8
cpxOpcode (CpxZ   w8)  = 0xE4 : encodeWord8   w8
cpxOpcode (CpxA  w16)  = 0xEC : encodeWord16 w16

cpyOpcode :: Cpy -> [Word8]
cpyOpcode (CpyI   w8)  = 0xC0 : encodeWord8   w8
cpyOpcode (CpyZ   w8)  = 0xC4 : encodeWord8   w8
cpyOpcode (CpyA  w16)  = 0xCC : encodeWord16 w16

decOpcode :: Dec -> [Word8]
decOpcode (DecZ   w8)  = 0xC6 : encodeWord8   w8
decOpcode (DecZX  w8)  = 0xD6 : encodeWord8   w8
decOpcode (DecA  w16)  = 0xCE : encodeWord16 w16
decOpcode (DecAX w16)  = 0xDE : encodeWord16 w16

eorOpcode :: Eor -> [Word8]
eorOpcode (EorI   w8)  = 0x49 : encodeWord8   w8
eorOpcode (EorZ   w8)  = 0x45 : encodeWord8   w8
eorOpcode (EorZX  w8)  = 0x55 : encodeWord8   w8
eorOpcode (EorA  w16)  = 0x40 : encodeWord16 w16
eorOpcode (EorAX w16)  = 0x50 : encodeWord16 w16
eorOpcode (EorAY w16)  = 0x59 : encodeWord16 w16
eorOpcode (EorIX  w8)  = 0x41 : encodeWord8   w8
eorOpcode (EorIY  w8)  = 0x51 : encodeWord8   w8

incOpcode :: Inc -> [Word8]
incOpcode (IncZ   w8)  = 0xE6 : encodeWord8   w8
incOpcode (IncZX  w8)  = 0xF6 : encodeWord8   w8
incOpcode (IncA  w16)  = 0xEE : encodeWord16 w16
incOpcode (IncAX w16)  = 0xFE : encodeWord16 w16

jmpOpcode :: Jmp -> [Word8]
jmpOpcode (JmpA (AbsAddr w16)) = 0x4C : encodeWord16 w16
jmpOpcode (JmpI (AbsAddr w16)) = 0x6C : encodeWord16 w16
jmpOpcode x         = error $ "Error: " ++ show x

ldaOpcode :: Lda -> [Word8]
ldaOpcode (LdaI   w8)  = 0xA9 : encodeWord8   w8
ldaOpcode (LdaZ   w8)  = 0xA5 : encodeWord8   w8
ldaOpcode (LdaZX  w8)  = 0xB5 : encodeWord8   w8
ldaOpcode (LdaA  w16)  = 0xAD : encodeWord16 w16
ldaOpcode (LdaAX w16)  = 0xBD : encodeWord16 w16
ldaOpcode (LdaAY w16)  = 0xB9 : encodeWord16 w16
ldaOpcode (LdaIX  w8)  = 0xA1 : encodeWord8   w8
ldaOpcode (LdaIY  w8)  = 0xB1 : encodeWord8   w8

ldxOpcode :: Ldx -> [Word8]
ldxOpcode (LdxI   w8)  = 0xA2 : encodeWord8   w8
ldxOpcode (LdxZ   w8)  = 0xA6 : encodeWord8   w8
ldxOpcode (LdxZY  w8)  = 0xB6 : encodeWord8   w8
ldxOpcode (LdxA  w16)  = 0xAE : encodeWord16 w16
ldxOpcode (LdxAY w16)  = 0xBE : encodeWord16 w16

ldyOpcode :: Ldy -> [Word8]
ldyOpcode (LdyI   w8)  = 0xA0 : encodeWord8   w8
ldyOpcode (LdyZ   w8)  = 0xA4 : encodeWord8   w8
ldyOpcode (LdyZX  w8)  = 0xB4 : encodeWord8   w8
ldyOpcode (LdyA  w16)  = 0xAC : encodeWord16 w16
ldyOpcode (LdyAX w16)  = 0xBC : encodeWord16 w16

oraOpcode :: Ora -> [Word8]
oraOpcode (OraI   w8)  = 0x09 : encodeWord8   w8
oraOpcode (OraZ   w8)  = 0x05 : encodeWord8   w8
oraOpcode (OraZX  w8)  = 0x15 : encodeWord8   w8
oraOpcode (OraA  w16)  = 0x0D : encodeWord16 w16
oraOpcode (OraAX w16)  = 0x1D : encodeWord16 w16
oraOpcode (OraAY w16)  = 0x19 : encodeWord16 w16
oraOpcode (OraIX  w8)  = 0x01 : encodeWord8   w8
oraOpcode (OraIY  w8)  = 0x11 : encodeWord8   w8

lsrOpcode :: Lsr -> [Word8]
lsrOpcode LsrAc        = 0x4A : []
lsrOpcode (LsrZ   w8)  = 0x46 : encodeWord8   w8
lsrOpcode (LsrZX  w8)  = 0x56 : encodeWord8   w8
lsrOpcode (LsrA  w16)  = 0x4E : encodeWord16 w16
lsrOpcode (LsrAX w16)  = 0x5E : encodeWord16 w16

rolOpcode :: Rol -> [Word8]
rolOpcode RolAc        = 0x2A : []
rolOpcode (RolZ   w8)  = 0x26 : encodeWord8   w8
rolOpcode (RolZX  w8)  = 0x36 : encodeWord8   w8
rolOpcode (RolA  w16)  = 0x2E : encodeWord16 w16
rolOpcode (RolAX w16)  = 0x3E : encodeWord16 w16

rorOpcode :: Ror -> [Word8]
rorOpcode RorAc        = 0x6A : []
rorOpcode (RorZ   w8)  = 0x66 : encodeWord8   w8
rorOpcode (RorZX  w8)  = 0x76 : encodeWord8   w8
rorOpcode (RorA  w16)  = 0x6E : encodeWord16 w16
rorOpcode (RorAX w16)  = 0x7E : encodeWord16 w16

sbcOpcode :: Sbc -> [Word8]
sbcOpcode (SbcI   w8)  = 0xE9 : encodeWord8   w8
sbcOpcode (SbcZ   w8)  = 0xE5 : encodeWord8   w8
sbcOpcode (SbcZX  w8)  = 0xF5 : encodeWord8   w8
sbcOpcode (SbcA  w16)  = 0xED : encodeWord16 w16
sbcOpcode (SbcAX w16)  = 0xFD : encodeWord16 w16
sbcOpcode (SbcAY w16)  = 0xF9 : encodeWord16 w16
sbcOpcode (SbcIX  w8)  = 0xE1 : encodeWord8   w8
sbcOpcode (SbcIY  w8)  = 0xF1 : encodeWord8   w8

staOpcode :: Sta -> [Word8]
staOpcode (StaZ   w8)  = 0x85 : encodeWord8   w8
staOpcode (StaZX  w8)  = 0x95 : encodeWord8   w8
staOpcode (StaA  w16)  = 0x8D : encodeWord16 w16
staOpcode (StaAX w16)  = 0x9D : encodeWord16 w16
staOpcode (StaAY w16)  = 0x99 : encodeWord16 w16
staOpcode (StaIX  w8)  = 0x81 : encodeWord8   w8
staOpcode (StaIY  w8)  = 0x91 : encodeWord8   w8

stxOpcode :: Stx -> [Word8]
stxOpcode (StxZ   w8)  = 0x86 : encodeWord8   w8
stxOpcode (StxZY  w8)  = 0x96 : encodeWord8   w8
stxOpcode (StxA  w16)  = 0x8E : encodeWord16 w16

styOpcode :: Sty -> [Word8]
styOpcode (StyZ   w8)  = 0x84 : encodeWord8   w8
styOpcode (StyZX  w8)  = 0x94 : encodeWord8   w8
styOpcode (StyA  w16)  = 0x8C : encodeWord16 w16

opsize :: Operation -> Int
opsize (BCC _) = 2
opsize (BCS _) = 2
opsize (BEQ _) = 2
opsize (BMI _) = 2
opsize (BNE _) = 2
opsize (BPL _) = 2
opsize (BVC _) = 2
opsize (BVS _) = 2
opsize (JMP _) = 3
opsize (JSR _) = 3
opsize op = length $ opcode op

encodeWord8 :: Word8 -> [Word8]
encodeWord8 x = [x]

encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = map fromIntegral [ x .&. 0xFF, (x .&. 0xFF00) `shiftR` 8 ]

printHex :: [Word8] -> [String]
printHex = map (`showHex` "")
