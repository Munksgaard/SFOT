{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module SFOT.Asm.Parser where

import SFOT.Asm.AST

import Text.ParserCombinators.Parsec hiding (Parser, token, label, labels)

import Prelude hiding (and)
import Data.Word
import Data.List (find)

import Control.Monad
import Data.Maybe

type Parser = GenParser Char [String]

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

indirect :: (WordAddr -> a) -> Parser a
indirect f = try $ parens $ do
  num <- wordAddr
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

adc :: Parser Operation
adc = do
  token "ADC"
  choice adcParsers
    where
      adcParsers = map (liftM ADC) adcParsers'
      adcParsers' = [immediate AdcI, zeroPage AdcZ, zeroPageX AdcZX,
                     absolute AdcA, absoluteX AdcAX, absoluteY AdcAY,
                     indirectX AdcIX, indirectY AdcIY]

and :: Parser Operation
and = do
  token "AND"
  choice andParsers
    where
      andParsers = map (liftM AND) andParsers'
      andParsers' = [immediate AndI, zeroPage AndZ, zeroPageX AndZX,
                     absolute AndA, absoluteX AndAX, absoluteY AndAY,
                     indirectX AndIX, indirectY AndIY]

asl :: Parser Operation
asl = do
  token "ASL"
  choice aslParsers
    where
      aslParsers = map (liftM ASL) aslParsers'
      aslParsers' = [zeroPage AslZ, zeroPageX AslZX,
                     absolute AslA, absoluteX AslAX, return AslAc]

bcc :: Parser Operation
bcc = do
  token "BCC"
  s <- name
  return $ BCC (ShortLabel s)

bcs :: Parser Operation
bcs = do
  token "BCS"
  s <- name
  return $ BCS (ShortLabel s)

beq :: Parser Operation
beq = do
  token "BEQ"
  s <- name
  return $ BEQ (ShortLabel s)

cmp :: Parser Operation
cmp = do
  token "CMP"
  choice cmpParsers
    where
      cmpParsers = map (liftM CMP) cmpParsers'
      cmpParsers' = [immediate CmpI, zeroPage CmpZ, zeroPageX CmpZX,
                     absolute CmpA, absoluteX CmpAX, absoluteY CmpAY,
                     indirectX CmpIX, indirectY CmpIY]

dex :: Parser Operation
dex = do
  token "DEX"
  return DEX

dey :: Parser Operation
dey = do
  token "DEY"
  return DEY

inx :: Parser Operation
inx = do
  token "INX"
  return INX

iny :: Parser Operation
iny = do
  token "INY"
  return INY

jmp :: Parser Operation
jmp = do
  token "JMP"
  choice [jmpInd, jmpAbs]
    where
      jmpAbs = do
        s <- name
        return $ JMP (JmpA (LongLabel s))
      jmpInd = parens $ do
        s <- name
        return $ JMP (JmpI (LongLabel s))

jsr :: Parser Operation
jsr = do
  token "JSR"
  s <- name
  return $ JSR $ LongLabel s

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

tax :: Parser Operation
tax = do
  token "TAX"
  return TAX

tay :: Parser Operation
tay = do
  token "TAY"
  return TAY

txa :: Parser Operation
txa = do
  token "TXA"
  return TXA

tya :: Parser Operation
tya = do
  token "TYA"
  return TYA

name :: Parser String
name = do
  c <- letter <|> char '_'
  cs <- many $ alphaNum <|> char '_'
  return $ c : cs

label :: Parser Operation
label = try $ do
  s <- name
  char ':'
  return $ Label s

lexeme :: Parser a -> Parser a
lexeme p = do{ x <- p; spaces; return x  }

program :: Parser Program
program = do
  prgm <- choice instructionParsers`endBy` many (void space <|> comment)
  eof
  return prgm
  where instructionParsers =
            [lda, sta, adc, cmp, beq, jmp, jsr, and, asl, bcc, bcs,
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
      longTrans s =
          fromIntegral $ fromMaybe (error $ "Label not found: " ++ show s) $ lookup s labelTable
      --
      translateLabels (BCC (ShortLabel s)) offset = BCC $ RelAddr $ shortTrans offset s
      translateLabels (BCS (ShortLabel s)) offset = BCS $ RelAddr $ shortTrans offset s
      translateLabels (BEQ (ShortLabel s)) offset = BEQ $ RelAddr $ shortTrans offset s
      translateLabels (JMP (JmpA (LongLabel s))) _ = JMP $ JmpA $ AbsAddr $ longTrans s
      translateLabels (JMP (JmpI (LongLabel s))) _ = JMP $ JmpI $ AbsAddr $ longTrans s
      translateLabels (JSR (LongLabel s)) _ = JSR $ AbsAddr $ longTrans s
      translateLabels inst _ = inst
