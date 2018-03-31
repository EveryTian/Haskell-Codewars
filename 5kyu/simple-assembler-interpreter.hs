-- https://www.codewars.com/kata/simple-assembler-interpreter

module SimpleAssembler (simpleAssembler) where
import qualified Data.Map.Strict as M
import Data.Either

type RegisterName = String
type RegisterNameOrConst = Either RegisterName Int
type Registers = M.Map String Int

data Instruction = Mov RegisterName RegisterNameOrConst
                 | Inc RegisterName
                 | Dec RegisterName
                 | Jnz RegisterNameOrConst RegisterNameOrConst
                 | Nil
data Status = Both Int Registers

simpleAssembler :: [String] -> Registers
simpleAssembler = run . genInsts

run :: [Instruction] -> Registers
run insts = simulate insts $ Both 0 (M.fromList [])

simulate :: [Instruction] -> Status -> Registers
simulate insts status@(Both index regs)
    | index < 0 || index >= length insts = regs
    | otherwise = simulate insts $ operate (insts !! index) status

operate :: Instruction -> Status -> Status
operate inst status@(Both index regs) = case inst of
    Mov reg regOrConst -> Both (index + 1)
                               (setRegValue reg 
                                            (getRegOrConstValue regOrConst regs)
                                            regs)
    Inc reg -> Both (index + 1)
                    (setRegValue reg
                                 ((getRegValue reg regs) + 1)
                                 regs)
    Dec reg -> Both (index + 1)
                    (setRegValue reg
                                 ((getRegValue reg regs) - 1)
                                 regs)
    Jnz regOrConst n -> if getRegOrConstValue regOrConst regs == 0
                        then Both (index + 1) regs
                        else Both (index + getRegOrConstValue n regs) regs

setRegValue :: RegisterName -> Int -> Registers -> Registers
setRegValue = M.insert

getRegOrConstValue :: RegisterNameOrConst -> Registers -> Int
getRegOrConstValue regOrConst regs = case regOrConst of
    Left reg -> getRegValue reg regs
    Right n -> n

getRegValue :: RegisterName -> Registers -> Int
getRegValue reg regs = let result = M.lookup reg regs
                       in case result of Just n -> n
                                         Nothing -> 0

genInsts :: [String] -> [Instruction]
genInsts = map genInst

genInst :: String -> Instruction
genInst str = let w = words str
              in genInst' w
    where genInst' ("mov":x:y:[]) = Mov x $ toRegisterNameOrConst y
          genInst' ("inc":x:[]) = Inc x
          genInst' ("dec":x:[]) = Dec x
          genInst' ("jnz":x:y:[]) = Jnz (toRegisterNameOrConst x) (toRegisterNameOrConst y)
          genInst' _ = Nil

toRegisterNameOrConst :: String -> RegisterNameOrConst
toRegisterNameOrConst str = if isNum str 
                            then Right $ (read str :: Int) 
                            else Left str
    where isNum :: String -> Bool
          isNum str = (str !! 0) `elem` ('-':['0'..'9'])
