module Day17 (part1, part2) where

import Data.Bifunctor (bimap)
import Data.Bits (xor)
import Data.Char (digitToInt)
import Data.List (foldl', intercalate)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace (trace)
import Text.Parsec (parse, sepBy1)
import Text.Parsec.Char (string)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (integer, makeTokenParser, symbol)
import Utils.Lists (chunks)

lexer = makeTokenParser haskellDef

data Opcode = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv deriving (Show)

type Operand = Integer

type Instruction = (Opcode, Operand)

type Output = [Integer]

data Computer = Computer
  { ra :: Integer,
    rb :: Integer,
    rc :: Integer,
    pointer :: Int,
    program :: Vector Instruction,
    rawProgram :: [Integer],
    output :: [Integer]
  }
  deriving (Show)

parser :: Parser Computer
parser =
  (\ra rb rc program -> Computer ra rb rc 0 (parseProgram program) program [])
    <$> (string "Register A: " *> integer lexer)
    <*> (string "Register B: " *> integer lexer)
    <*> (string "Register C: " *> integer lexer)
    <*> (string "Program: " *> (integer lexer `sepBy1` symbol lexer ","))
  where
    parseOpcode = \case 0 -> Adv; 1 -> Bxl; 2 -> Bst; 3 -> Jnz; 4 -> Bxc; 5 -> Out; 6 -> Bdv; 7 -> Cdv
    parseProgram xs = V.fromList $ map (\[x, y] -> (parseOpcode x, y)) (chunks 2 xs)

combo :: Computer -> Integer -> Integer
combo c@(Computer {ra, rb, rc}) operand = case operand of
  l | l <= 3 -> l
  4 -> ra
  5 -> rb
  6 -> rc

move :: Computer -> Computer
move c@(Computer {pointer}) = c {pointer = pointer + 1}

adv :: Computer -> Operand -> Computer
adv c@(Computer {ra}) operand = move $ c {ra = ra `div` (2 ^ combo c operand)}

bdv :: Computer -> Operand -> Computer
bdv c@(Computer {ra}) operand = move $ c {rb = ra `div` (2 ^ combo c operand)}

cdv :: Computer -> Operand -> Computer
cdv c@(Computer {ra}) operand = move $ c {rc = ra `div` (2 ^ combo c operand)}

bxl :: Computer -> Operand -> Computer
bxl c@(Computer {rb}) operand = move $ c {rb = rb `xor` operand}

bst :: Computer -> Operand -> Computer
bst c operand = move $ c {rb = combo c operand `mod` 8}

jnz :: Computer -> Operand -> Computer
jnz c@(Computer {ra}) operand = if ra == 0 then move c else c {pointer = fromInteger operand}

bxc :: Computer -> Computer
bxc c@(Computer {rb, rc}) = move $ c {rb = rb `xor` rc}

out :: Computer -> Operand -> Computer
out c@(Computer {output}) operand = move $ c {output = (combo c operand `mod` 8) : output}

run :: Computer -> Output
run c@(Computer {pointer, program, output}) = case program V.!? pointer of
  Nothing -> reverse output
  Just (Adv, operand) -> run $ adv c operand
  Just (Bdv, operand) -> run $ bdv c operand
  Just (Cdv, operand) -> run $ cdv c operand
  Just (Bxl, operand) -> run $ bxl c operand
  Just (Bst, operand) -> run $ bst c operand
  Just (Jnz, operand) -> run $ jnz c operand
  Just (Out, operand) -> run $ out c operand
  Just (Bxc, _) -> run $ bxc c

printOutput :: Output -> String
printOutput out = intercalate "," (map show out)

lowestA :: Computer -> Int -> Integer -> Maybe Integer
lowestA c@(Computer {rawProgram}) n a0 | n > 16 = Just a0
lowestA c@(Computer {rawProgram}) n a0 = listToMaybe $ mapMaybe (lowestA c (n + 1)) viable
  where
    a1 = a0 * 8
    target = drop (length rawProgram - n) rawProgram
    viable = filter (\a -> run c {ra = a} == target) [a1 .. a1 + 7]

part1 :: String -> String
part1 input = case parse parser "computer" input of
  Left e -> error (show e)
  Right computer -> printOutput $ run computer

part2 :: String -> String
part2 input = case parse parser "computer" input of
  Left e -> error (show e)
  Right computer -> maybe "Oops" show (lowestA computer 1 0)
