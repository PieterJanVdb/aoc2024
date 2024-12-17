module Day17 (part1, part2) where

import Data.Bifunctor (bimap)
import Data.Bits (xor)
import Data.List (foldl', intercalate)
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

ex :: String
ex =
  "Register A: 117440\n\
  \Register B: 0\n\
  \Register C: 0\n\
  \\n\
  \Program: 0,3,5,4,3,0"

data Opcode = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv deriving (Show)

type Operand = Int

type Instruction = (Opcode, Operand)

type Output = [Int]

data Computer = Computer
  { ra :: Int,
    rb :: Int,
    rc :: Int,
    pointer :: Int,
    program :: Vector Instruction,
    output :: [Int]
  }
  deriving (Show)

parser :: Parser Computer
parser =
  (\ra rb rc program -> Computer (fromInteger ra) (fromInteger rb) (fromInteger rc) 0 (parseProgram program) [])
    <$> (string "Register A: " *> integer lexer)
    <*> (string "Register B: " *> integer lexer)
    <*> (string "Register C: " *> integer lexer)
    <*> (string "Program: " *> (integer lexer `sepBy1` symbol lexer ","))
  where
    parseOpcode = \case 0 -> Adv; 1 -> Bxl; 2 -> Bst; 3 -> Jnz; 4 -> Bxc; 5 -> Out; 6 -> Bdv; 7 -> Cdv
    parseProgram xs = V.fromList $ map (\[x, y] -> (parseOpcode x, fromInteger y)) (chunks 2 xs)

combo :: Computer -> Int -> Int
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
jnz c@(Computer {ra}) operand = if ra == 0 then move c else c {pointer = operand}

bxc :: Computer -> Computer
bxc c@(Computer {rb, rc}) = move $ c {rb = rb `xor` rc}

out :: Computer -> Operand -> Computer
out c@(Computer {output}) operand = move $ c {output = (combo c operand `mod` 8) : output}

run :: Computer -> Output
run c@(Computer {pointer, program, output}) = case program V.!? pointer of
  Nothing -> reverse output
  Just (Adv, operand) -> let n = adv c operand in trace ("ADV -> " <> show n) run n
  Just (Bdv, operand) -> let n = bdv c operand in trace ("BDV -> " <> show n) run n
  Just (Cdv, operand) -> let n = cdv c operand in trace ("CDV -> " <> show n) run n
  Just (Bxl, operand) -> let n = bxl c operand in trace ("BXL -> " <> show n) run n
  Just (Bst, operand) -> let n = bst c operand in trace ("BST -> " <> show n) run n
  Just (Jnz, operand) -> let n = jnz c operand in trace ("JNZ -> " <> show n) run n
  Just (Out, operand) -> let n = out c operand in trace ("OUT -> " <> show n) run n
  Just (Bxc, _) -> let n = run (bxc c) in trace ("BXC -> " <> show n) n

printOutput :: Output -> String
printOutput out = intercalate "," (map show out)

part1 :: String -> String
part1 input = case parse parser "computer" input of
  Left e -> error (show e)
  Right computer -> printOutput $ run computer

part2 :: String -> String
part2 input = ""
