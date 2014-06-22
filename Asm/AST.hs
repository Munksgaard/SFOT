module SFOT.Asm.AST where

import Data.Word

type ByteAddr = Word8
type Immediate = Word8
type WordAddr = Word16

type Program = [Operation]

data Operation = LDA Lda
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

opcode :: Operation -> Int
opcode (LDA lda) = ldaOpcode lda

ldaOpcode :: Lda -> Int
ldaOpcode (LdaI _)   = 0xA9
ldaOpcode (LdaZ _)   = 0xA5
ldaOpcode (LdaZX _)  = 0xB5
ldaOpcode (LdaA _)   = 0xAD
ldaOpcode (LdaAX _)  = 0xBD
ldaOpcode (LdaAY _)  = 0xB9
ldaOpcode (LdaIX _)  = 0xA1
ldaOpcode (LdaIY _)  = 0xB1

opsize :: Operation -> Int
opsize (LDA lda) = ldaOpsize lda

ldaOpsize :: Lda -> Int
ldaOpsize (LdaI _)   = 2
ldaOpsize (LdaZ _)   = 2
ldaOpsize (LdaZX _)  = 2
ldaOpsize (LdaA _)   = 3
ldaOpsize (LdaAX _)  = 3
ldaOpsize (LdaAY _)  = 3
ldaOpsize (LdaIX _)  = 2
ldaOpsize (LdaIY _)  = 2
