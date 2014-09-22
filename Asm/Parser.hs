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

decimal ::  Parser Int
decimal = do
  cs <- many1 digit
  return $ read cs

hex :: Parser Int
hex = do
  char '$'
  cs <- many1 hexDigit
  return $ read $ "0x" ++ cs

number :: Parser Int
number = hex <|> decimal

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

zeroPageY :: (ByteAddr -> a) -> Parser a
zeroPageY f = try $ do
  num <- byteAddr
  commaRegister 'Y'
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

bit :: Parser Operation
bit = do
  token "BIT"
  choice bitParsers
    where
      bitParsers = map (liftM BIT) bitParsers'
      bitParsers' = [zeroPage BitZ, absolute BitA]

bmi :: Parser Operation
bmi = do
  token "BMI"
  s <- name
  return $ BMI (ShortLabel s)

bne :: Parser Operation
bne = do
  token "BNE"
  s <- name
  return $ BNE (ShortLabel s)

bpl :: Parser Operation
bpl = do
  token "BPL"
  s <- name
  return $ BPL (ShortLabel s)

brk :: Parser Operation
brk = do
  token "BRK"
  return BRK

bvc :: Parser Operation
bvc = do
  token "BVC"
  s <- name
  return $ BVC (ShortLabel s)

bvs :: Parser Operation
bvs = do
  token "BVS"
  s <- name
  return $ BVS (ShortLabel s)

clc :: Parser Operation
clc = do
  token "CLC"
  return CLC

cld :: Parser Operation
cld = do
  token "CLD"
  return CLD

cli :: Parser Operation
cli = do
  token "CLI"
  return CLI

clv :: Parser Operation
clv = do
  token "CLV"
  return CLV

cmp :: Parser Operation
cmp = do
  token "CMP"
  choice cmpParsers
    where
      cmpParsers = map (liftM CMP) cmpParsers'
      cmpParsers' = [immediate CmpI, zeroPage CmpZ, zeroPageX CmpZX,
                     absolute CmpA, absoluteX CmpAX, absoluteY CmpAY,
                     indirectX CmpIX, indirectY CmpIY]

cpx :: Parser Operation
cpx = do
  token "CPX"
  choice cpxParsers
    where
      cpxParsers = map (liftM CPX) cpxParsers'
      cpxParsers' = [immediate CpxI, zeroPage CpxZ, absolute CpxA]

cpy :: Parser Operation
cpy = do
  token "CPY"
  choice cpyParsers
    where
      cpyParsers = map (liftM CPY) cpyParsers'
      cpyParsers' = [immediate CpyI, zeroPage CpyZ, absolute CpyA]

dec :: Parser Operation
dec = do
  token "DEC"
  choice decParsers
    where
      decParsers = map (liftM DEC) decParsers'
      decParsers' = [zeroPage DecZ, zeroPageX DecZX, absolute DecA, absoluteX DecAX]

dex :: Parser Operation
dex = do
  token "DEX"
  return DEX

dey :: Parser Operation
dey = do
  token "DEY"
  return DEY

eor :: Parser Operation
eor = do
  token "EOR"
  choice eorParsers
    where
      eorParsers = map (liftM EOR) eorParsers'
      eorParsers' = [immediate EorI, zeroPage EorZ, zeroPageX EorZX,
                     absolute EorA, absoluteX EorAX, absoluteY EorAY,
                     indirectX EorIX, indirectY EorIY]

inc :: Parser Operation
inc = do
  token "INC"
  choice incParsers
    where
      incParsers = map (liftM INC) incParsers'
      incParsers' = [zeroPage IncZ, zeroPageX IncZX,
                     absolute IncA, absoluteX IncAX]

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

ldx :: Parser Operation
ldx = do
  token "LDX"
  choice ldxParsers
    where
      ldxParsers = map (liftM LDX) ldxParsers'
      ldxParsers' = [immediate LdxI, zeroPage LdxZ, zeroPageY LdxZY,
                     absolute LdxA, absoluteY LdxAY]

ldy :: Parser Operation
ldy = do
  token "LDY"
  choice ldyParsers
    where
      ldyParsers = map (liftM LDY) ldyParsers'
      ldyParsers' = [immediate LdyI, zeroPage LdyZ, zeroPageX LdyZX,
                     absolute LdyA, absoluteX LdyAX]

lsr :: Parser Operation
lsr = do
  token "LSR"
  choice lsrParsers
    where
      lsrParsers = map (liftM LSR) lsrParsers'
      lsrParsers' = [zeroPage LsrZ, zeroPageX LsrZX,
                     absolute LsrA, absoluteX LsrAX, return LsrAc]

ora :: Parser Operation
ora = do
  token "ORA"
  choice oraParsers
    where
      oraParsers = map (liftM ORA) oraParsers'
      oraParsers' = [immediate OraI, zeroPage OraZ, zeroPageX OraZX,
                     absolute OraA, absoluteX OraAX, absoluteY OraAY,
                     indirectX OraIX, indirectY OraIY]

nop :: Parser Operation
nop = do
  token "NOP"
  return NOP

pha :: Parser Operation
pha = do
  token "PHA"
  return PHA

php :: Parser Operation
php = do
  token "PHP"
  return PHP

pla :: Parser Operation
pla = do
  token "PLA"
  return PLA

plp :: Parser Operation
plp = do
  token "PLP"
  return PLP

rol :: Parser Operation
rol = do
  token "ROL"
  choice rolParsers
    where
      rolParsers = map (liftM ROL) rolParsers'
      rolParsers' = [zeroPage RolZ, zeroPageX RolZX, absolute RolA,
                     absoluteX RolAX, return RolAc]

ror :: Parser Operation
ror = do
  token "ROR"
  choice rorParsers
    where
      rorParsers = map (liftM ROR) rorParsers'
      rorParsers' = [zeroPage RorZ, zeroPageX RorZX, absolute RorA,
                     absoluteX RorAX, return RorAc]

rti :: Parser Operation
rti = do
  token "RTI"
  return RTI

rts :: Parser Operation
rts = do
  token "RTS"
  return RTS

sbc :: Parser Operation
sbc = do
  token "SBC"
  choice sbcParsers
    where
      sbcParsers = map (liftM SBC) sbcParsers'
      sbcParsers' = [immediate SbcI, zeroPage SbcZ, zeroPageX SbcZX,
                     absolute SbcA, absoluteX SbcAX, absoluteY SbcAY,
                     indirectX SbcIX, indirectY SbcIY]

sec :: Parser Operation
sec = do
  token "SEC"
  return SEC

sed :: Parser Operation
sed = do
  token "SED"
  return SED

sei :: Parser Operation
sei = do
  token "SEI"
  return SEI

sta :: Parser Operation
sta = do
  token "STA"
  choice staParsers
    where
      staParsers = map (liftM STA) staParsers'
      staParsers' = [zeroPage StaZ, zeroPageX StaZX, absolute StaA,
                     absoluteX StaAX, absoluteY StaAY, indirectX StaIX,
                     indirectY StaIY]

stx :: Parser Operation
stx = do
  token "STX"
  choice stxParsers
    where
      stxParsers = map (liftM STX) stxParsers'
      stxParsers' = [zeroPage StxZ, zeroPageY StxZY, absolute StxA]

sty :: Parser Operation
sty = do
  token "STY"
  choice styParsers
    where
      styParsers = map (liftM STY) styParsers'
      styParsers' = [zeroPage StyZ, zeroPageX StyZX, absolute StyA]

tax :: Parser Operation
tax = do
  token "TAX"
  return TAX

tay :: Parser Operation
tay = do
  token "TAY"
  return TAY

tsx :: Parser Operation
tsx = do
  token "TSX"
  return TSX

txa :: Parser Operation
txa = do
  token "TXA"
  return TXA

txs :: Parser Operation
txs = do
  token "TXS"
  return TXS

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
            [adc, and, asl, bcc, bcs, beq, bit, bmi, bne, bpl, brk, bvc, bvs,
             clc, cld, cli, clv, cmp, cpx, cpy, dec, dey, eor, inc, inx, iny,
             jmp, jsr, label, lda, ldx, ldy, lsr, nop, ora, pha, php, pla, plp,
             rol, ror, rti, rts, sbc, sec, sed, sei, sta, stx, sty, tax, tsx,
             txa, txs, dex, tay, tya]

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
      translateLabels (BMI (ShortLabel s)) offset = BMI $ RelAddr $ shortTrans offset s
      translateLabels (BNE (ShortLabel s)) offset = BNE $ RelAddr $ shortTrans offset s
      translateLabels (BPL (ShortLabel s)) offset = BPL $ RelAddr $ shortTrans offset s
      translateLabels (BVC (ShortLabel s)) offset = BVC $ RelAddr $ shortTrans offset s
      translateLabels (BVS (ShortLabel s)) offset = BVS $ RelAddr $ shortTrans offset s
      translateLabels (JMP (JmpA (LongLabel s))) _ = JMP $ JmpA $ AbsAddr $ longTrans s
      translateLabels (JMP (JmpI (LongLabel s))) _ = JMP $ JmpI $ AbsAddr $ longTrans s
      translateLabels (JSR (LongLabel s)) _ = JSR $ AbsAddr $ longTrans s
      translateLabels inst _ = inst
