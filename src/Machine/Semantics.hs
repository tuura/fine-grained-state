module Machine.Semantics where

import Prelude hiding (Read, Monad, div, mod, abs)
import qualified Prelude (Read, Monad, div, mod, abs)
import Data.Functor (void)
import Data.Either (partitionEithers)
import Control.Selective
import Control.Arrow (second)
import Machine.Decode (decode)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Maybe (fromJust)
import Algebra.Graph hiding (graph)
import Algebra.Graph.Export.Dot
import Machine.Types

--------------------------------------------------------------------------------
---------------- Instruction Semantics -----------------------------------------
--------------------------------------------------------------------------------

data Key a where
    Reg  :: Register -> Key (Value)
    -- ^ register
    Addr :: MemoryAddress -> Key (Value)
    -- ^ memory address
    F   :: Flag -> Key (Bool)
    -- -- ^ flag
    IC   :: Key (Value)
    -- -- ^ instruction counter
    IR   :: Key (InstructionCode)
    -- ^ instruction register
    Prog :: InstructionAddress -> Key (InstructionCode)
    -- ^ program memory address

instance Show (Key a) where
    show = \case
        Reg  reg   -> show reg
        Addr addr  -> show addr
        F    flag  -> show flag
        IC         -> "IC"
        IR         -> "IR"
        Prog addr  -> show addr

type Read f = forall a. Key a -> f (Sym a)

type Write f = forall a. Key a -> f (Sym a) -> f (Sym a)

type FS c a = forall f. c f => Read f ->
                               Write f ->
                               f a

--------------------------------------------------------------------------------
--------------- Data-flow graphs of programs -----------------------------------
--------------------------------------------------------------------------------

type KeyLabel = String

type InstructionLabel = String

-- | Extract input and output data-dependencies of a computation
dependencies :: FS Selective a
             -> ([KeyLabel], [KeyLabel])
dependencies task =
    partitionEithers . getOver $
    task trackingRead trackingWrite
  where trackingRead  k    = Over       [Left  $ show k]
        trackingWrite k fv = fv *> Over [Right $ show k]

-- | Compute static data flow graph of an instruction. In case of supplying a
--   monadic, i.e. data-dependent instruction, 'Nothing' is returned.
--
-- Since no data requiring simulation is performed, the semantics metalanguage
-- terms are mocked: 'read' becomes 'const 0' is 'write' is simply ignored.
instructionGraph :: (InstructionAddress, Instruction)
                    -> Maybe (Graph (Either KeyLabel InstructionLabel))
instructionGraph i@(addr, instr) = do
    let (ins, outs) = dependencies (instructionSemantics instr)
    let instrInfo = instructionLabel
    pure $ overlay (star (Right instrInfo) (map Left outs))
                   (transpose $ star (Right instrInfo) (map Left ins))
    where instructionLabel = (show addr <> "|" <> show instr)

-- | Serialise data flow graph as a .dot string
drawGraph :: Graph (Either KeyLabel InstructionLabel) -> String
drawGraph g = export style g
  where
    style = defaultStyleViaShow
        { vertexName = \v -> "v" ++ show (fromJust $ Set.lookupIndex v names)
        , vertexAttributes = \x -> case x of
            Left  k      -> [ "shape"  := "circle"
                            , "label"  := k ]
            Right i -> [ "shape" := "record"
                            , "label" := i ] }
    names = vertexSet g
    -- instructionLabel a i = fromString (show a <> "|" <> show i)

-- | Compute static data flow graph of a program. In case of supplying a
--   monadic, i.e. data-dependent instruction, 'Nothing' is returned.
programDataGraph :: Program
                 -> Maybe (Graph (Either KeyLabel InstructionLabel))
programDataGraph p =
    let p' = map (second decode) p
    in  foldl go (Just empty) (map instructionGraph p')
    where go _   Nothing  = Nothing
          go acc g        = overlay <$> acc <*> g

--------------------------------------------------------------------------------
--------------- Semantics of instructions --------------------------------------
--------------------------------------------------------------------------------

-- | Halt the execution.
--   Applicative.
halt :: FS Applicative ()
halt read write = void $
    write (F Halted) (pure (SConst True))

set :: Register -> SImm8 -> FS Applicative ()
set reg simm read write = void $
    write (Reg reg) (pure (SConst . fromIntegral $ simm))

-- | Add a value from memory location to one in a register.
--   Applicative.
--
--   It looks like we after all need a Monad here since Applicative theoretically permits reordering
--   of effects. Here, it is important for the overflow check to happen before the addition, otherwise
--   the check may be executed with the updated value of the register and will give an invalid result.
add :: Register -> MemoryAddress
    -> FS Applicative ()
add reg addr = \read write -> void $
    let arg1     = read (Reg reg)
        arg2     = read (Addr addr)
        result   = SAdd <$> arg1
                        <*> arg2
        overflow = willOverflow <$> arg1 <*> arg2
    in write (F Overflow) overflow *>
       write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    where willOverflow :: Sym Value -> Sym Value -> Sym Bool
          willOverflow x y =
              let o1 = SGt y (SConst 0)
                  o2 = SGt x (SSub maxBound y)
                  o3 = SLt y (SConst 0)
                  o4 = SLt x (SSub minBound y)
              in  SOr (SAnd o1 o2)
                      (SAnd o3 o4)
-- | Load a value from a memory location to a register.
--   Functor.
load :: Register -> MemoryAddress
     -> FS Functor ()
load reg addr read write = void $
    write (Reg reg) (read (Addr addr))

-- | Store a value from a register to a memory location.
--   Functor.
store :: Register -> MemoryAddress -> FS Functor ()
store reg addr read write = void $
    write (Addr addr) (read (Reg reg))

    -- let overflow = arg2 .> 0 &&& arg1 .< (minBound @Value + arg2) |||
    --                arg2 .< 0 &&& arg1 .> (maxBound @Value + arg2)

-- | Sub a value from memory location to one in a register.
--   Applicative.
sub :: Register -> MemoryAddress -> FS Applicative ()
sub reg addr = \read write -> void $
    let arg1     = read (Reg reg)
        arg2     = read (Addr addr)
        result   = SSub <$> arg1
                        <*> arg2
        overflow = willOverflow <$> arg1 <*> arg2
    in write (F Overflow) overflow *>
       write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    where willOverflow :: Sym Value -> Sym Value -> Sym Bool
          willOverflow x y =
              let o1 = SGt y (SConst 0)
                  o2 = SLt x (SAdd minBound y)
                  o3 = SLt y (SConst 0)
                  o4 = SGt x (SAdd maxBound y)
              in  SOr (SAnd o1 o2)
                      (SAnd o3 o4)

-- | Multiply a value from memory location to one in a register.
--   Applicative.
mul :: Register -> MemoryAddress -> FS Applicative ()
mul reg addr = \read write -> void $
    let result = (SMul) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

-- | Subtract a value from memory location to one in a register.
--   Applicative.
div :: Register -> MemoryAddress -> FS Applicative ()
div reg addr = \read write -> void $
    let result = SDiv <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    -- in  write (F Zero) (write (Reg reg) result)

mod :: Register -> MemoryAddress -> FS Applicative ()
mod reg addr = \read write -> void $
    let arg1     = read (Reg reg)
        arg2     = read (Addr addr)
        result   = SMod <$> arg1
                        <*> arg2
        overflow = willOverflow <$> arg1 <*> arg2
    in write (F Overflow) overflow *>
       write (F Zero) ((SEq (SConst 0)) <$> write (Reg reg) result)
    where willOverflow :: Sym Value -> Sym Value -> Sym Bool
          willOverflow x y =
              let o1 = SEq y (SConst 0)
                  o2 = SEq x minBound
                  o3 = SEq y (SConst (-1))
              in  SOr o1
                      (SAnd o2 o3)

abs :: Register -> FS Applicative ()
abs reg = \read write -> void $
    let result = SAbs <$> read (Reg reg)
        overflow x = SLt <$> x <*> pure (SConst 0)
    in  write (F Overflow) (overflow $ write (Reg reg) result)

-- | Unconditional jump.
--   Functor.
jump :: SImm8 -> FS Functor ()
jump simm read write = void $
    write IC (fmap (SAdd (SConst . fromIntegral $ simm)) (read IC))

-- | Note this the this polymorphic semantics of conditional jumps only works
--   for simulation and dependency analysis. For symbolic execution it has
--   to be implemented separately. See 'Machine.Symbolic.jumpZeroSym'.
jumpZero :: SImm8 -> FS Selective ()
jumpZero _ _ _ = error "jumpZero not implemented"

-- | Note this the this polymorphic semantics of conditional jumps only works
--   for simulation and dependency analysis. For symbolic execution it has
--   to be implemented separately. See 'Machine.Symbolic.jumpCtSym'.
jumpCt :: SImm8 -> FS Selective ()
jumpCt offset read write =
    whenS (unliteral <$> read (F Condition))
          (void $ write IC (SAdd <$> pure (SConst (fromIntegral offset))
                                 <*> read IC))

-- | Note this the this polymorphic semantics of conditional jumps only works
--   for simulation and dependency analysis. For symbolic execution it has
--   to be implemented separately. See 'Machine.Symbolic.jumpCfSym'.
jumpCf :: SImm8 -> FS Selective ()
jumpCf offset read write =
    whenS (not . unliteral <$> read (F Condition))
          (void $ write IC (SAdd <$> pure (SConst (fromIntegral offset))
                                 <*> read IC))

cmpEq :: Register -> MemoryAddress -> FS Applicative ()
cmpEq reg addr = \read write -> void $
    write (F Condition) (SEq <$> read (Reg reg) <*> read (Addr addr))

cmpGt :: Register -> MemoryAddress -> FS Applicative ()
cmpGt reg addr = \read write -> void $
    write (F Condition) (SGt <$> read (Reg reg) <*> read (Addr addr))

cmpLt :: Register -> MemoryAddress -> FS Applicative ()
cmpLt reg addr = \read write -> void $
    write (F Condition) (SLt <$> read (Reg reg) <*> read (Addr addr))

instructionSemantics :: Instruction -> FS Selective ()
instructionSemantics (Instruction i) r w = case i of
    Halt -> halt r w
    Load reg addr -> load reg addr r w
    -- -- LoadMI reg addr -> loadMI reg addr r w
    Set reg simm8  -> set reg simm8 r w
    Store reg addr -> store reg addr r w
    Add reg addr   -> add reg addr r w
    Sub reg addr   -> sub reg addr r w
    Mul reg addr   -> mul reg addr r w
    Div reg addr   -> div reg addr r w
    Mod reg addr   -> mod reg addr r w
    Abs reg        -> abs reg r w
    Jump simm8     -> jump simm8 r w
    JumpZero simm8 -> jumpZero simm8 r w

    CmpEq reg addr -> cmpEq reg addr r w
    CmpGt reg addr -> cmpGt reg addr r w
    CmpLt reg addr -> cmpLt reg addr r w

    JumpCt simm8   -> jumpCt simm8 r w
    JumpCf simm8   -> jumpCf simm8 r w