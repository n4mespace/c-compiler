module Compiler.Generator.Emiters where

import           Compiler.Types

import           System.IO.Unsafe (unsafePerformIO)
import           System.Random    (StdGen, randomRs)

-- Helpers
emitBlock :: [Either ErrT String] -> Either ErrT String
emitBlock = (concat <$>) . sequence

(<$*>) :: Either ErrT String -> Either ErrT String -> Either ErrT String
expr1 <$*> expr2 = (<>) <$> expr1 <*> expr2
infixr 6 <$*>

emitNLn :: String -> Either ErrT String
emitNLn = Right . ("\n\t" <>)

nLine :: Either ErrT String
nLine = emitNLn ""

emitLn :: String -> Either ErrT String
emitLn = Right . ("\t" <>)

emitLbl :: String -> Either ErrT String
emitLbl = Right . ("\n" <>) . (<> ":")

pop :: String -> Either ErrT String
pop = emitNLn . ("pop " <>)

popEax :: Either ErrT String
popEax = pop "eax"

popEbx :: Either ErrT String
popEbx = pop "ebx"

push :: String -> Either ErrT String
push = emitNLn . ("push " <>)

pushEax :: Either ErrT String
pushEax = push "eax"

pushEbx :: Either ErrT String
pushEbx = push "ebx"

movTo :: String -> Either ErrT String
movTo = emitNLn . ("mov " <>) . (<> ", eax")

-- Basic math functions
subOp :: Either ErrT String
subOp = emitNLn "sub eax, ebx"

addOp :: Either ErrT String
addOp = emitNLn "add eax, ebx"

modOp :: Either ErrT String
modOp =
  emitBlock
    [ emitNLn "xor edx, edx"
    , emitNLn "div ebx"
    , emitNLn "mov eax, edx"
    ]

mulOp :: Either ErrT String
mulOp = emitNLn "imul ebx"

divOp :: Either ErrT String
divOp =
  emitNLn "cdq" <$*>
  emitNLn "idiv ebx"

andOp :: Either ErrT String
andOp = emitNLn "and eax, ebx"

orOp :: Either ErrT String
orOp = emitNLn "or eax, ebx"

eqOp :: Either ErrT String
eqOp =
  emitNLn "cmp eax, ebx" <$*>
  emitNLn "sete al"

greaterOp :: Either ErrT String
greaterOp =
  emitNLn "cmp eax, ebx" <$*>
  emitNLn "setg al"

lessOp :: Either ErrT String
lessOp =
  emitNLn "cmp eax, ebx" <$*>
  emitNLn "setl al"

notOp :: Either ErrT String
notOp =
  emitNLn "cmp eax, 0" <$*>
  emitNLn "sete al"

negOp :: Either ErrT String
negOp = emitNLn "neg eax"

complementOp :: Either ErrT String
complementOp = emitNLn "xor eax, -1"

-- Work with control flow
goToIfElse :: String -> String -> Either ErrT String
goToIfElse lbl lbl' =
  emitBlock
    [ emitNLn "cmp eax, 0"
    , emitNLn ("jne " <> lbl)
    , emitNLn ("je " <> lbl')
    ]

goToIf :: String -> Either ErrT String
goToIf lbl =
  emitBlock
    [ emitNLn "cmp eax, 0"
    , emitNLn $ "je " <> lbl
    ]

goToIfNot :: String -> Either ErrT String
goToIfNot lbl =
  emitBlock
    [ emitNLn "cmp eax, 1"
    , emitNLn $ "je " <> lbl
    ]

goTo :: String -> Either ErrT String
goTo = emitNLn . ("jmp " <>)

ret :: Either ErrT String
ret = emitNLn "leave" <$*> emitNLn "ret"

getRandomLbl :: IO StdGen -> String
getRandomLbl gen =
  "__" <> take 6 (randomRs ('a','z') $ unsafePerformIO gen)

addMainFuncCall :: Either ErrT String -> Either ErrT String
addMainFuncCall emitedProgram =
  emitBlock
    [ goTo "__start_program"
    , emitedProgram
    , nLine
    , emitLbl "__start_program"
    , emitNLn "call __func_main"
    , emitNLn "mov b, eax"
    , nLine
    ]
