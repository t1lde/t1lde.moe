-----------
title: 'Day 8: Handheld Halting'
subtitle: 'Advent of Code 2020'
published: 'Wednesday 23 Dec 2020 05:31:10 GMT'
sort: 8
-----------


> module Day8 (run1, run2) where
> import qualified Data.Map as M
> import Data.Text (breakOn, stripPrefix)
> import Control.Arrow as Ar
> import qualified Data.Sequence as Seq
> import Data.Monoid
> import Control.Monad.Fix
> import Control.Monad.State.Strict
> import qualified Data.IntSet as IS

 <!--imports-->

Day 8
======

> data InstructionType = Acc | Nop | Jmp
>   deriving stock Show

> data Instruction = Ins InstructionType Int
>   deriving stock Show
>
> type Program = Seq Instruction
> mkProgram :: [Instruction] -> Program
> mkProgram = fromList @(Seq Instruction)

> parseInsType :: Text -> Maybe InstructionType
> parseInsType tx | tx == "acc" = pure $ Acc
>                 | tx == "jmp" = pure $ Jmp
>                 | tx == "nop" = pure $ Nop
>                 | otherwise   = Nothing

> parseIntSign :: Text -> Maybe Int
> parseIntSign (stripPrefix " +" -> Just tx) = (toString >>> (readMaybe @Int)) tx
> parseIntSign (stripPrefix " " -> Just tx) = (toString >>> (readMaybe @Int)) tx
> parseIntSign _ = Nothing

> parseInstruction :: Text -> Maybe Instruction
> parseInstruction =
>   (breakOn " ")
>     >>> (parseInsType *** (parseIntSign))
>     >>> (uncurry $ liftA2 Ins)

> parseProgram :: [Text] -> Program
> parseProgram = (traverse parseInstruction) >>> (fromMaybe []) >>> mkProgram


> type ExecT m a = ReaderT Program (StateT (Int, Int) m) a

> fetchIns :: ExecT ExitInsState Instruction
> fetchIns = do
>   prog <- ask
>   (acc,ins) <- get
>   maybe (lift $ lift $ lift $ Left (acc, True)) return (Seq.lookup ins prog)
>

> execInstruction :: (Monad m) => Instruction -> ExecT m ()
> execInstruction (Ins Jmp n) = modify (Ar.second (+n))
> execInstruction (Ins Acc n) = modify ((+n) *** succ)
> execInstruction (Ins Nop n) = modify (Ar.second succ)

> runProgramWithM :: (ExecT ExitInsState a) -> ExecT ExitInsState a
> runProgramWithM f =
>    (f >> (fetchIns >>= execInstruction)) >> runProgramWithM f

> type ExitInsState = StateT IS.IntSet (Either (Int, Bool))
>
> exitOnRepeatInstruction :: ExecT ExitInsState ()
> exitOnRepeatInstruction = do
>   (acc, instruction) <- get
>   sofar <- lift $ lift get
>   when (instruction `IS.member` sofar) $ lift $ lift $ lift $ Left (acc,False)
>   lift $ lift $ modify (IS.insert instruction)


> run1 tx =
>   (evalStateT (evalStateT (runReaderT (runProgramWithM exitOnRepeatInstruction) (parseProgram tx)) (0,0)) mempty)

> modifications :: Program -> [Program]
> modifications prog = do
>   (addr, ins) <- zip [0..] $ toList prog
>   case ins of
>     (Ins Jmp n) -> pure $ Seq.update addr (Ins Nop n) prog
>     (Ins Nop n) -> pure $ Seq.update addr (Ins Jmp n) prog
>     _ -> empty
>

> run2 :: [Text] -> Maybe Int
> run2 =
>   parseProgram
>     >>> modifications
>     >>> (fmap $ (`evalStateT` mempty) . (`evalStateT` (0,0)) . runReaderT (runProgramWithM exitOnRepeatInstruction))
>     >>> lefts
>     >>> filter (snd)
>     >>> (fmap fst)
>     >>> (viaNonEmpty head)
>
>
