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

commaRegister :: Char -> Parser ()
commaRegister c = do
  spaces
  char ','
  spaces
  char c
  return ()

immediate :: (ByteAddr -> a) -> Parser a
immediate f = do
  char '#'
  num <- number
  if num <= 0xff then
      return $ f $ fromIntegral num
  else
      fail "Number too big"

zeroPage :: (ByteAddr -> a) -> Parser a
zeroPage f = try $ do
  num <- byteAddr
  notFollowedBy (spaces >> char ',')
  return $ f num

zeroPageX :: (ByteAddr -> a) -> Parser a
zeroPageX f = try $ do
  num <- byteAddr
  commaRegister 'X'
  return $ f num

absolute :: (WordAddr -> a) -> Parser a
absolute f = try $ do
  num <- wordAddr
  notFollowedBy (spaces >> char ',')
  return $ f num

absoluteX :: (WordAddr -> a) -> Parser a
absoluteX f = try $ do
  num <- wordAddr
  commaRegister 'X'
  return $ f num

absoluteY :: (WordAddr -> a) -> Parser a
absoluteY f = try $ do
  num <- wordAddr
  commaRegister 'Y'
  return $ f num

startIndirect :: Parser ByteAddr
startIndirect = do
  char '('
  spaces
  byteAddr

indirectX :: (ByteAddr -> a) -> Parser a
indirectX f = try $ do
  num <- startIndirect
  commaRegister 'X'
  spaces
  char ')'
  return $ f num

indirectY :: (ByteAddr -> a) -> Parser a
indirectY f = try $ do
  num <- startIndirect
  spaces
  char ')'
  commaRegister 'Y'
  return $ f num

lda :: Parser Operation
lda = do
  string "LDA"
  spaces
  choice ldaParsers
    where
      ldaParsers = map (liftM LDA) ldaParsers'
      ldaParsers' = [immediate LdaI, zeroPage LdaZ, zeroPageX LdaZX,
                     absolute LdaA, absoluteX LdaAX, absoluteY LdaAY,
                     indirectX LdaIX, indirectY LdaIY]

sta :: Parser Operation
sta = do
  string "STA"
  spaces
  choice staParsers
    where
      staParsers = map (liftM STA) staParsers'
      staParsers' = [zeroPage StaZ, zeroPageX StaZX, absolute StaA,
                     absoluteX StaAX, absoluteY StaAY, indirectX StaIX,
                     indirectY StaIY]
