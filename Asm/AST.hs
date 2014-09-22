module SFOT.Asm.AST where

import Data.Word
import Data.Bits
import Data.Int
import Numeric

type ByteAddr = Word8
type Immediate = Word8
type WordAddr = Word16

data ShortJump =  ShortLabel String | RelAddr Int8 deriving (Show, Eq)

data LongJump = LongLabel String | AbsAddr WordAddr deriving (Show, Eq)

type Program = [Operation]

data Operation = ADC Adc
               | BEQ ShortJump
               | CMP Cmp
               | DEX
               | DEY
               | INX
               | INY
               | JMP Jmp
               | JSR LongJump
               | Label String
               | LDA Lda
               | STA Sta
               | TAX
               | TAY
               | TXA
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

data Cmp = CmpI  Immediate
         | CmpZ  ByteAddr
         | CmpZX ByteAddr
         | CmpA  WordAddr
         | CmpAX WordAddr
         | CmpAY WordAddr
         | CmpIX ByteAddr
         | CmpIY ByteAddr
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

data Sta = StaZ ByteAddr
         | StaZX ByteAddr
         | StaA WordAddr
         | StaAX WordAddr
         | StaAY WordAddr
         | StaIX ByteAddr
         | StaIY ByteAddr
           deriving (Show, Eq)

opcode :: Operation -> [Word8]
opcode (ADC adc) = adcOpcode adc
opcode (BEQ (RelAddr w8)) = [0xF0, fromIntegral w8]
opcode (CMP cmp) = cmpOpcode cmp
opcode (JMP jmp) = jmpOpcode jmp
opcode (JSR (AbsAddr w16)) = 0x20 : encodeWord16 w16
opcode (LDA lda) = ldaOpcode lda
opcode (Label _) = []
opcode (STA sta) = staOpcode sta
opcode DEX       = [0xCA]
opcode DEY       = [0x88]
opcode INX       = [0xE8]
opcode INY       = [0xC8]
opcode TAX       = [0xAA]
opcode TAY       = [0xA8]
opcode TXA       = [0x8A]
opcode TYA       = [0x98]
opcode x         = error $ "Error: " ++ show x

adcOpcode :: Adc -> [Word8]
adcOpcode (AdcA  w16)  = 0x6D : encodeWord16 w16
adcOpcode (AdcAX w16)  = 0x7D : encodeWord16 w16
adcOpcode (AdcAY w16)  = 0x79 : encodeWord16 w16
adcOpcode (AdcI   w8)  = 0x69 : encodeWord8   w8
adcOpcode (AdcIX  w8)  = 0x61 : encodeWord8   w8
adcOpcode (AdcIY  w8)  = 0x71 : encodeWord8   w8
adcOpcode (AdcZ   w8)  = 0x65 : encodeWord8   w8
adcOpcode (AdcZX  w8)  = 0x75 : encodeWord8   w8

cmpOpcode :: Cmp -> [Word8]
cmpOpcode (CmpA  w16)  = 0xCD : encodeWord16 w16
cmpOpcode (CmpAX w16)  = 0xDD : encodeWord16 w16
cmpOpcode (CmpAY w16)  = 0xD9 : encodeWord16 w16
cmpOpcode (CmpI   w8)  = 0xC9 : encodeWord8   w8
cmpOpcode (CmpIX  w8)  = 0xC1 : encodeWord8   w8
cmpOpcode (CmpIY  w8)  = 0xD1 : encodeWord8   w8
cmpOpcode (CmpZ   w8)  = 0xC5 : encodeWord8   w8
cmpOpcode (CmpZX  w8)  = 0xD5 : encodeWord8   w8

jmpOpcode :: Jmp -> [Word8]
jmpOpcode (JmpA (AbsAddr w16)) = 0x4C : encodeWord16 w16
jmpOpcode (JmpI (AbsAddr w16)) = 0x6C : encodeWord16 w16
jmpOpcode x         = error $ "Error: " ++ show x

ldaOpcode :: Lda -> [Word8]
ldaOpcode (LdaA  w16)  = 0xAD : encodeWord16 w16
ldaOpcode (LdaAX w16)  = 0xBD : encodeWord16 w16
ldaOpcode (LdaAY w16)  = 0xB9 : encodeWord16 w16
ldaOpcode (LdaI   w8)  = 0xA9 : encodeWord8   w8
ldaOpcode (LdaIX  w8)  = 0xA1 : encodeWord8   w8
ldaOpcode (LdaIY  w8)  = 0xB1 : encodeWord8   w8
ldaOpcode (LdaZ   w8)  = 0xA5 : encodeWord8   w8
ldaOpcode (LdaZX  w8)  = 0xB5 : encodeWord8   w8

staOpcode :: Sta -> [Word8]
staOpcode (StaA  w16)  = 0x8D : encodeWord16 w16
staOpcode (StaAX w16)  = 0x9D : encodeWord16 w16
staOpcode (StaAY w16)  = 0x99 : encodeWord16 w16
staOpcode (StaIX  w8)  = 0x81 : encodeWord8   w8
staOpcode (StaIY  w8)  = 0x91 : encodeWord8   w8
staOpcode (StaZ   w8)  = 0x85 : encodeWord8   w8
staOpcode (StaZX  w8)  = 0x95 : encodeWord8   w8

opsize :: Operation -> Int
opsize (BEQ _) = 2
opsize (JMP _) = 3
opsize (JSR _) = 3
opsize op = length $ opcode op

encodeWord8 :: Word8 -> [Word8]
encodeWord8 x = [x]

encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = map fromIntegral [ x .&. 0xFF, (x .&. 0xFF00) `shiftR` 8 ]

printHex :: [Word8] -> [String]
printHex = map (`showHex` "")
