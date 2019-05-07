module Machine.Types.State where

import qualified Data.Map.Strict as Map
import qualified Data.SBV        as SBV
import           Machine.Types
import           Machine.Decode
import           Machine.Encode

-- | The state of symbolic computation
data State = State { registers         :: Map.Map Register (Sym Value)
                   , instructionCounter :: InstructionAddress -- Sym
                   , instructionRegister :: InstructionCode
                   , flags :: Map.Map Flag (Sym Bool)
                   , memory :: Map.Map MemoryAddress (Sym Value)
                   , program :: Program
                   , clock :: Clock
                   , pathConstraintList :: [Sym Bool]
                   }

appendConstraints :: [Sym Bool] -> State -> State
appendConstraints cs s =
    let cs' = cs ++ pathConstraintList s
    in s { pathConstraintList = cs' }

-- | Pretty-print state as an ASCII string
renderState :: State -> String
renderState state =
  "IC: " <> show (instructionCounter state) <> "\n" <>
  "IR: " <> show (decode $ instructionRegister state) <> "\n" <>
  "Registers: " <> show (Map.toList $ registers state) <> "\n" <>
  "Flags: " <> show (Map.toList $ flags state) <> "\n" <>
  "Memory: " <> show (filter ((/= SConst 0) . snd) . Map.toList $ memory state) <> "\n" <>
  "Path Constraints: \n" <> renderPathConstraints (pathConstraintList state) <> "\n"

renderPathConstraints :: [Sym Bool] -> String
renderPathConstraints xs = foldr (\x acc -> "  && " <> show x <> "\n" <> acc) "" xs

emptyRegisters :: Map.Map Register (Sym Value)
emptyRegisters = Map.fromList $ zip [R0, R1, R2, R3] (map SConst [0, 0..])

emptyFlags :: Map.Map Flag (Sym Bool)
emptyFlags = Map.fromList $ zip [Zero, Overflow, Halted] (map SConst $ repeat False)

initialiseMemory :: [(MemoryAddress, Sym Value)] -> Map.Map MemoryAddress (Sym Value)
initialiseMemory vars =
    let blankMemory = Map.fromList $ zip [0..255] (map SConst [0, 0..])
    in foldr (\(addr, value) acc -> Map.adjust (const value) (fromIntegral addr) acc) blankMemory vars

boot :: Program -> Map.Map MemoryAddress (Sym Value) -> State
boot prog mem = State { registers = emptyRegisters
                      , instructionCounter = 0
                      , instructionRegister = encode . Instruction $ Jump 0
                      , program = prog
                      , flags = emptyFlags
                      , memory = mem
                      , clock = 0
                      , pathConstraintList = []
                      }

----------------------------------------------------------------------------------------------------
-- | Constant-fold all expressions which only contain literals in their leafs
foldConstantsInState :: State -> State
foldConstantsInState s@(State regs ic ir flags mem prog clock pathConstraints) =
  let regs'            = Map.map tryFoldConstant regs
      flags'           = Map.map tryFoldConstant flags
      mem'             = Map.map tryFoldConstant mem
      pathConstraints' = map tryFoldConstant pathConstraints
  in  State regs'
            ic
            ir
            flags'
            mem'
            prog
            clock
            pathConstraints

----------------------------------------------------------------------------------------------------

data SolvedState = SolvedState State SBV.SMTResult

-- | Render the output of the SMT solver into a human-readable form
renderSMTResult :: SBV.SMTResult -> String
renderSMTResult (SBV.Unsatisfiable _ _) = "Unsatisfiable"
renderSMTResult s@(SBV.Satisfiable _ _) =
  let dict = SBV.getModelDictionary s
  in  if Map.null dict then "Trivial" else renderDict dict
renderSMTResult _ = "Error"

renderDict :: Show v => Map.Map String v -> String
renderDict m =
  foldr toStr "" (Map.toList m)
  where toStr (k,v) s = k <> " = " <> show v <> ", " <> s

renderSolvedState :: SolvedState -> String
renderSolvedState (SolvedState state c) =
  "Clock: " <> show (clock state) <> "\n" <>
  "IC: " <> show (instructionCounter state) <> "\n" <>
  "IR: " <> show (decode $ instructionRegister state) <> "\n" <>
  "Regs: " <> show (Map.toList $ registers state) <> "\n" <>
  "Memory: " <> show (filter ((not . nonZero) . snd) . Map.toList $ memory state) <> "\n" <>
  "Flags: " <> show (Map.toList $ flags state) <> "\n" <>
  "Path Constraints: \n" <> renderPathConstraints (pathConstraintList state) <> "\n" <>
  "Solved Values: " <> renderSMTResult c

