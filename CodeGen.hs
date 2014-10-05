module CodeGen (
      generate
) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Writer
import Data.List

import Structures

type Generation = WriterT [String] Compilation

emit :: String -> Generation ()
emit s = do
    indent <- liftM indentLevel get
    tell [concat (replicate indent "    ") ++ s]

-- TODO: make into string -> string functions
emitVariableDecl :: String -> Generation ()
emitVariableDecl name = emit $ "value_t " ++ name ++ ";"

emitAssign :: String -> String -> Generation ()
emitAssign name value = emit $ name ++ " = " ++ value ++ ";"

emitDeclAssign :: String -> String -> Generation ()
emitDeclAssign name value = emit $ "value_t " ++ name ++ " = " ++ value ++ ";"

emitFunctionSig :: String -> [String] -> Generation ()
emitFunctionSig name params =
    let params' = map ("value_t " ++) params
    in  emit $ "value_t " ++ name ++ "(" ++ intercalate ", " params' ++ ")"

emitFunctionDecl :: String -> [String] -> Generation ()
emitFunctionDecl name params = emit $ "value_t " ++ name ++ "(" ++ intercalate ", " (replicate (length params) "value_t") ++ ");"

newLine :: Generation ()
newLine = emit ""

processCall :: String -> [CodeGenTree] -> Generation String
processCall name arguments = do
    prefix <- uuid "op"
    let varNames        = map ((prefix ++) . show) [1 .. (length arguments)]
        call [] _       = return $ name ++ "(" ++ intercalate ", " varNames ++ ")"
        call (e:es) n   = do
            value <- process e
            emitDeclAssign (prefix ++ show n) value
            call es (n + 1)
    call arguments 1

processCond :: CodeGenTree -> CodeGenTree -> CodeGenTree -> Generation String
processCond condition consequent alternative = do
    name <- uuid "if"
    conditionVal <- process condition
    emitVariableDecl name
    emit $ "if ((" ++ conditionVal ++ ") == BOOL_F) {"
    incrIndent
    alternativeVal <- process alternative
    emitAssign name alternativeVal
    decrIndent
    emit "} else {"
    incrIndent
    consequentVal <- process consequent
    emitAssign name consequentVal
    decrIndent
    emit "}"
    return name

processLet :: [(String, CodeGenTree)] -> CodeGenTree -> Generation String
processLet bindings expr = do
    name <- uuid "let"
    emitVariableDecl name
    emit "{"
    incrIndent
    bindings' <- mapM (process . snd) bindings
    mapM_ (uncurry emitDeclAssign) (zip (map fst bindings) bindings')
    newLine
    result <- process expr
    newLine
    emitAssign name result
    decrIndent
    emit "}"
    return name

process :: CodeGenTree -> Generation String
process (CGImmediate s)   = return s
process (CGCall n es)     = processCall n es
process (CGIf c e1 e2)    = processCond c e1 e2
process (CGLet xs e)      = processLet xs e
process (CGLambda n a fs) = let args = n : show a : show (length fs) : fs
                            in  return $ "_make_closure(" ++ intercalate ", " args ++ ")"

generateBlock :: CodeGenBlock -> Generation ()
generateBlock (CodeGenBlock name params tree) = do
    newLine
    emitFunctionSig name params
    emit "{"
    incrIndent
    retVal <- process tree
    emit $ "return " ++ retVal ++ ";"
    decrIndent
    emit "}"

generate' :: [CodeGenBlock] -> Generation ()
generate' blocks = do
    emit "#include \"runtime.h\""
    emit "#include \"primitives.h\""
    newLine
    mapM_ (\(CodeGenBlock n p _) -> emitFunctionDecl n p) blocks
    mapM_ generateBlock blocks

generate :: [CodeGenBlock] -> Compilation String
generate blocks = do
    (_, code) <- runWriterT $ generate' blocks
    return $ unlines code
