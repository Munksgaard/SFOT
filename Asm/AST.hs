module SFOT.Asm.AST where

import Data.Word
import Data.Bits

type ByteAddr = Word8
type Immediate = Word8
type WordAddr = Word16

type Program = [Operation]

data Operation = LDA Lda
               | STA Sta
               | INX
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
opcode (LDA lda) = ldaOpcode lda
opcode (STA sta) = staOpcode sta
opcode INX       = [0xE8]

ldaOpcode :: Lda -> [Word8]
ldaOpcode (LdaI   w8)  = 0xA9 : encodeWord8   w8
ldaOpcode (LdaZ   w8)  = 0xA5 : encodeWord8   w8
ldaOpcode (LdaZX  w8)  = 0xB5 : encodeWord8   w8
ldaOpcode (LdaA  w16)  = 0xAD : encodeWord16 w16
ldaOpcode (LdaAX w16)  = 0xBD : encodeWord16 w16
ldaOpcode (LdaAY w16)  = 0xB9 : encodeWord16 w16
ldaOpcode (LdaIX  w8)  = 0xA1 : encodeWord8   w8
ldaOpcode (LdaIY  w8)  = 0xB1 : encodeWord8   w8

staOpcode :: Sta -> [Word8]
staOpcode (StaZ   w8)  = 0x85 : encodeWord8   w8
staOpcode (StaZX  w8)  = 0x95 : encodeWord8   w8
staOpcode (StaA  w16)  = 0x8D : encodeWord16 w16
staOpcode (StaAX w16)  = 0x9D : encodeWord16 w16
staOpcode (StaAY w16)  = 0x99 : encodeWord16 w16
staOpcode (StaIX  w8)  = 0x81 : encodeWord8   w8
staOpcode (StaIY  w8)  = 0x91 : encodeWord8   w8

opsize :: Operation -> Int
opsize = length . opcode

encodeWord8 :: Word8 -> [Word8]
encodeWord8 x = [x]

encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = map fromIntegral [ x .&. 0xFF, (x .&. 0xFF00) `shiftR` 8 ]
