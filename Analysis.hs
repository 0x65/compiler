module Analysis (
      analyze
) where

import Control.Monad
import Control.Monad.Trans.Writer
import Data.Bits
import Data.List
import Data.Maybe

import Structures

-- later: make some config generator that gens .h and .hs file
fixNumShift = 0x2
boolFalse   = 0x2F
boolTrue    = 0x6F
nil         = 0x3F
entryPoint  = "_entry_point"

type Analysis = WriterT [CodeGenBlock] Compilation

processLet :: [(String, ProgramTree)] -> ProgramTree -> Analysis CodeGenTree
processLet locals expr = do
    expr' <- process expr
    locals' <- mapM (process . snd) locals
    return $ CGLet (zip (map fst locals) locals') expr'

processIf :: ProgramTree -> ProgramTree -> ProgramTree -> Analysis CodeGenTree
processIf condition consequent alternative = do
    conditionTree <- process condition
    consequentTree <- process consequent
    alternativeTree <- process alternative
    return $ CGIf conditionTree consequentTree alternativeTree

processLambda :: Maybe String -> [String] -> ProgramTree -> Analysis CodeGenTree
processLambda lname vars expr = do
    name <- uuid "lambda"
    expr' <- process expr
    let free = freeVars expr (maybeToList lname ++ vars)
    tell [CodeGenBlock name (vars ++ free ++ [fromMaybe "_self" lname]) expr']
    return $ CGLambda name (length vars) free

-- TODO: fix this nubbing
freeVars :: ProgramTree -> [String] -> [String]
freeVars (PLet xs e) vs       = nub $ freeVars e (vs ++ map fst xs) ++ concatMap ((`freeVars` vs) . snd) xs
freeVars (PIf c e1 e2) vs     = nub $ freeVars c vs ++ freeVars e1 vs ++ freeVars e2 vs
freeVars (PLambda n xs e) vs  = nub $ freeVars e (xs ++ vs)
freeVars (PApply f es) vs     = nub $ freeVars f vs ++ concatMap (`freeVars` vs) es
freeVars (PVar s) vs          = if s `elem` vs then [] else [s]
freeVars _ _                  = []

process :: ProgramTree -> Analysis CodeGenTree
process (PInt i)          = return $ CGImmediate (show (i `shift` fixNumShift))
process (PBool b)         = return $ CGImmediate (show (if b then boolTrue else boolFalse))
process PNil              = return $ CGImmediate (show nil)
process (PVar s)          = return $ CGImmediate s
process (PLet xs e)       = processLet xs e
process (PIf c e1 e2)     = processIf c e1 e2
process (PLambda n vs e)  = processLambda n vs e
process (PApply f es)     = liftM (CGCall "_apply_closure") (mapM process (f:es))

analyze :: ProgramTree -> Compilation [CodeGenBlock]
analyze tree = do
    (mainTree, otherBlocks) <- runWriterT $ process tree
    return $ otherBlocks ++ [CodeGenBlock entryPoint [] mainTree]
