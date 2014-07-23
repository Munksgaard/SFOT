{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module SFOT.Asm.Parser where

import SFOT.Asm.AST

import Text.ParserCombinators.Parsec hiding (Parser, token, label, labels)

import Data.Word
import Data.List

import Control.Monad
import Data.Maybe

type Parser = GenParser Char [String]

reservedWords = ["ADC", "AND", "ASL", "BCC", "BCS", "BEQ", "BIT", "BMI", "BNE",
                 "BPL", "BRK", "BVC", "BVS", "CLC", "CLD", "CLI", "CLV", "CMP",
                 "CPX", "CPY", "DEC", "DEX", "DEY", "EOR", "INC", "INX", "INY",
                 "JMP", "JSR", "LDA", "LDX", "LDY", "LSR", "NOP", "ORA", "PHA",
                 "PHP", "PLA", "PLP", "ROL", "ROR", "RTI", "RTS", "SBC", "SEC",
                 "SED", "SEI", "STA", "STX", "STY", "TAX", "TAY", "TSX", "TXA",
                 "TXS", "TYA"]

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

comment :: Parser ()
comment = do
  char ';'
  manyTill anyChar (void newline <|> eof)
  return ()

whitespace :: Parser ()
whitespace = skipMany (skipMany1 space <|> comment)

token :: String -> Parser ()
token s = try $ string s >> notFollowedBy (alphaNum <|> char '_') >> skipMany space

startPar :: Parser ()
startPar = char '(' >> spaces

endPar :: Parser ()
endPar = char ')' >> spaces

parens :: Parser a -> Parser a
parens = between startPar endPar

commaRegister :: Char -> Parser ()
commaRegister c = do
  spaces
  char ','
  spaces
  char c
  notFollowedBy (alphaNum <|> char '_')
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

indirectX :: (ByteAddr -> a) -> Parser a
indirectX f = try $ parens $ do
  num <- byteAddr
  commaRegister 'X'
  return $ f num

indirectY :: (ByteAddr -> a) -> Parser a
indirectY f = try $ do
  num <- parens byteAddr
  commaRegister 'Y'
  return $ f num

lda :: Parser Operation
lda = do
  token "LDA"
  choice ldaParsers
    where
      ldaParsers = map (liftM LDA) ldaParsers'
      ldaParsers' = [immediate LdaI, zeroPage LdaZ, zeroPageX LdaZX,
                     absolute LdaA, absoluteX LdaAX, absoluteY LdaAY,
                     indirectX LdaIX, indirectY LdaIY]

sta :: Parser Operation
sta = do
  token "STA"
  choice staParsers
    where
      staParsers = map (liftM STA) staParsers'
      staParsers' = [zeroPage StaZ, zeroPageX StaZX, absolute StaA,
                     absoluteX StaAX, absoluteY StaAY, indirectX StaIX,
                     indirectY StaIY]

adc :: Parser Operation
adc = do
  token "ADC"
  choice adcParsers
    where
      adcParsers = map (liftM ADC) adcParsers'
      adcParsers' = [immediate AdcI, zeroPage AdcZ, zeroPageX AdcZX,
                     absolute AdcA, absoluteX AdcAX, absoluteY AdcAY,
                     indirectX AdcIX, indirectY AdcIY]

cmp :: Parser Operation
cmp = do
  token "CMP"
  choice cmpParsers
    where
      cmpParsers = map (liftM CMP) cmpParsers'
      cmpParsers' = [immediate CmpI, zeroPage CmpZ, zeroPageX CmpZX,
                     absolute CmpA, absoluteX CmpAX, absoluteY CmpAY,
                     indirectX CmpIX, indirectY CmpIY]

inx :: Parser Operation
inx = do
  token "INX"
  return INX

tax :: Parser Operation
tax = do
  token "TAX"
  return TAX

txa :: Parser Operation
txa = do
  token "TXA"
  return TXA

dex :: Parser Operation
dex = do
  token "DEX"
  return DEX

tay :: Parser Operation
tay = do
  token "TAY"
  return TAY

tya :: Parser Operation
tya = do
  token "TYA"
  return TYA

dey :: Parser Operation
dey = do
  token "DEY"
  return DEY

iny :: Parser Operation
iny = do
  token "INY"
  return INY

name :: Parser String
name = do
  c <- letter <|> char '_'
  cs <- many $ alphaNum <|> char '_'
  return $ c : cs

label :: Parser Operation
label = try $ do
  s <- name
  char ':'
  return $ Label $ s

beq :: Parser Operation
beq = do
  token "BEQ"
  s <- name
  return $ BEQ (ShortLabel s)

lexeme :: Parser a -> Parser a
lexeme p = do{ x <- p; spaces; return x  }

program :: Parser Program
program = do
  prgm <- choice instructionParsers`endBy` many (void space <|> comment)
  eof
  return prgm
  where instructionParsers =
            [lda, sta, adc, cmp, beq,
             inx, tax, txa, dex, tay, tya, dey, iny,
             label]

--resolveLabels :: Program -> Program
--resolveLabels :: [Operation] -> [(String, Int)]
resolveLabels :: [Operation] -> [Operation]
resolveLabels p =
    zipWith translateLabels p byteOffsets
    where
      byteOffsets = scanl (\n op -> n + opsize op) 0 p
      labelTable = foldl findLabels [] $ zip p byteOffsets
      --
      findLabels labels (ins, i) =
          case ins of
            Label s -> case find ((== s) . fst) labels of
                           Nothing -> (s, i) : labels
                           _ -> error $ "Label already exists: " ++ show s
            _ -> labels
      --
      shortTrans offset s =
          if (fromIntegral relAddr :: Word8) < (0xff :: Word8) then
              fromIntegral relAddr
          else
              error $ "Branch jump is too big: " ++
                    show (fromIntegral relAddr :: Word8)
          where
            relAddr = labOffset - (offset + 2)
            labOffset = fromMaybe (error $ "Label not found: " ++ show s) $ lookup s labelTable
      --
      translateLabels (BEQ (ShortLabel s)) offset = BEQ $ RelAddr $ shortTrans offset s
      translateLabels inst _ = inst
