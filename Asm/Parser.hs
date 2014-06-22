{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module SFOT.Asm.Parser where

import SFOT.Asm.AST

import Text.ParserCombinators.Parsec

import Control.Monad

dec ::  Parser Int
dec = do
  cs <- many1 digit
  return $ read cs

hex :: Parser Int
hex = do
  char '$'
  cs <- many1 hexDigit
  return $ read $ "0x" ++ cs

number :: Parser Int
number = hex <|> dec

immediate :: Parser Immediate
immediate = do
  char '#'
  num <- number
  if num <= 0xff then
      return $ fromIntegral num
  else
      fail "Number too big"

byteAddr :: Parser ByteAddr
byteAddr = do
  num <- number
  if num <= 0xff then
      return $ fromIntegral num
  else
      fail "Number too big"

wordAddr :: Parser WordAddr
wordAddr = do
  num <- number
  if num <= 0xffff then
      return $ fromIntegral num
  else
      fail "Only supports 16 bit addresses"

ldaImm :: Parser Lda
ldaImm = do
  imm <- immediate
  return $ LdaI imm

ldaZ :: Parser Lda
ldaZ = do
  byte <- byteAddr
  option (LdaZ byte)
       (do string ",X"
           return $ LdaZX byte)

regXOrY :: (a -> Lda) -> (a -> Lda) -> a -> Parser Lda
regXOrY f1 f2 x = char ',' >>
  (do try $ string "X"
      return $ f1 x
   <|>
   do string "Y"
      return $ f2 x)

ldaA :: Parser Lda
ldaA = do
  word <- wordAddr
  option (LdaA word) $ regXOrY LdaAX LdaAY word

ldaInd :: Parser Lda
ldaInd = do
  char '('
  byte <- byteAddr
  (string ",X)" >> return (LdaIX byte)) <|>
    (string "),Y" >> return (LdaIY byte))

lda :: Parser Operation
lda = do
  string "LDA"
  spaces
  choice ldaParsers
    where
      ldaParsers = map (liftM LDA) ldaParsers'
      ldaParsers' = [ldaImm, try ldaZ, try ldaA, ldaInd]
