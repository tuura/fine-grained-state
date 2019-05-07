module Machine.Examples.Common where

import           Machine.Types
import           Machine.Types.State
import qualified Data.Map.Strict as Map

readProgram :: FilePath -> IO Program
readProgram = (fmap parseProgram) . readFile

-- | Quick-and-dirty program parser.
--   Comments start with the '#' character.
--   Blank lines are ignored.
parseProgram :: String -> Program
parseProgram src =
    let instructions = map read . removeBlankLines . removeComments . lines $ src
    in addInstructionAddresses instructions
    where removeComments = map (takeWhile (/= '#'))
          removeBlankLines = filter (not . null)
          addInstructionAddresses = zip [0..]

showProgram :: Program -> String
showProgram prog = let is = map snd $ prog
                   in unlines . map show $ is

overflowSet :: State -> Sym Bool
overflowSet s = (Map.!) (flags s) Overflow

halted :: State -> Sym Bool
halted s = (Map.!) (flags s) Halted
