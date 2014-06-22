module SFOT.Asm.AST where

import Data.Word

type ByteAddr = Word8
type Immediate = Word8
type WordAddr = Word16

type Program = [Operation]

data Operation = LDA Lda
               | STA Sta
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

opcode :: Operation -> Int
opcode (LDA lda) = ldaOpcode lda
opcode (STA sta) = staOpcode sta

ldaOpcode :: Lda -> Int
ldaOpcode (LdaI _)   = 0xA9
ldaOpcode (LdaZ _)   = 0xA5
ldaOpcode (LdaZX _)  = 0xB5
ldaOpcode (LdaA _)   = 0xAD
ldaOpcode (LdaAX _)  = 0xBD
ldaOpcode (LdaAY _)  = 0xB9
ldaOpcode (LdaIX _)  = 0xA1
ldaOpcode (LdaIY _)  = 0xB1

staOpcode :: Sta -> Int
staOpcode (StaZ _)   = 0x85
staOpcode (StaZX _)  = 0x95
staOpcode (StaA _)   = 0x8D
staOpcode (StaAX _)  = 0x9D
staOpcode (StaAY _)  = 0x99
staOpcode (StaIX _)  = 0x81
staOpcode (StaIY _)  = 0x91

opsize :: Operation -> Int
opsize (LDA lda) = ldaOpsize lda
opsize (STA sta) = staOpsize sta

ldaOpsize :: Lda -> Int
ldaOpsize (LdaI _)   = 2
ldaOpsize (LdaZ _)   = 2
ldaOpsize (LdaZX _)  = 2
ldaOpsize (LdaA _)   = 3
ldaOpsize (LdaAX _)  = 3
ldaOpsize (LdaAY _)  = 3
ldaOpsize (LdaIX _)  = 2
ldaOpsize (LdaIY _)  = 2

staOpsize :: Sta -> Int
staOpsize (StaZ _)   = 2
staOpsize (StaZX _)  = 2
staOpsize (StaA _)   = 3
staOpsize (StaAX _)  = 3
staOpsize (StaAY _)  = 3
staOpsize (StaIX _)  = 2
staOpsize (StaIY _)  = 2
