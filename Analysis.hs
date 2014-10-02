module Analysis (
      analyze
) where

import Control.Monad (liftM, mapM)
import Control.Monad.State (get, modify, runState, State)
import Control.Monad.Trans.Writer (runWriterT, tell, WriterT)
import Data.Bits (shift)

import Structures (ProgramTree (..), CodeGenTree (..), CodeGenBlock (..))

-- later: make some config generator that gens .h and .hs file
fixNumShift = 0x2
boolFalse   = 0x2F
boolTrue    = 0x6F
nil         = 0x3F
entryPoint  = "_entry_point"

data Analyzer = Analyzer {
    uuidCounter :: Int
}

newAnalyzer :: Analyzer
newAnalyzer = Analyzer { uuidCounter = 0 }

type Analysis = WriterT [CodeGenBlock] (State Analyzer)

uuid :: Analysis Int
uuid = do
    count <- liftM uuidCounter get
    modify (\analyzer -> analyzer { uuidCounter = count + 1 })
    return count

processLet :: [(String, ProgramTree)] -> ProgramTree -> Analysis CodeGenTree
processLet locals expr = do
    name <- liftM (\i -> "let" ++ show i) uuid 
    expr' <- process expr
    tell [CodeGenBlock name (map fst locals) expr']
    values <- mapM (process . snd) locals
    return $ CGCall name values

processIf :: ProgramTree -> ProgramTree -> ProgramTree -> Analysis CodeGenTree
processIf condition consequent alternative = do
    conditionTree <- process condition
    consequentTree <- process consequent
    alternativeTree <- process alternative
    return $ CGIf conditionTree consequentTree alternativeTree

processLambda :: [String] -> ProgramTree -> Analysis CodeGenTree
processLambda vars expr = do
    let free = freeVars expr vars
    name <- liftM (\i -> "lambda" ++ show i) uuid
    expr' <- process expr
    tell [CodeGenBlock name (vars ++ free) expr']
    return $ CGLambda name (length vars) free

freeVars :: ProgramTree -> [String] -> [String]
freeVars (PLet xs e) vs     = freeVars e (vs ++ map fst xs) ++ concatMap ((`freeVars` vs) . snd) xs
freeVars (PIf c e1 e2) vs   = freeVars c vs ++ freeVars e1 vs ++ freeVars e2 vs
freeVars (PLambda xs e) vs  = freeVars e (xs ++ vs)
freeVars (PApply f es) vs   = freeVars f vs ++ concatMap (`freeVars` vs) es
freeVars (PVar s) vs        = if s `elem` vs then [] else [s]
freeVars _ _                = []

process :: ProgramTree -> Analysis CodeGenTree
process (PInt i)        = return $ CGImmediate (show (i `shift` fixNumShift))
process (PBool b)       = return $ CGImmediate (show (if b then boolTrue else boolFalse))
process PNil            = return $ CGImmediate (show nil)
process (PVar s)        = return $ CGImmediate s
process (PLet xs e)     = processLet xs e
process (PIf c e1 e2)   = processIf c e1 e2
process (PLambda vs e)  = processLambda vs e
process (PApply f es)   = liftM (CGCall "_apply_closure") (mapM process (f:es))

analyze :: ProgramTree -> [CodeGenBlock]
analyze tree =
    let ((mainTree, otherBlocks), _) = runState (runWriterT (process tree)) newAnalyzer
    in  otherBlocks ++ [CodeGenBlock entryPoint [] mainTree]
