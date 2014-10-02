module CodeGen (
      generate
) where

import Control.Monad (liftM)
import Control.Monad.State (get, modify, runState, State)
import Data.List (intercalate)
import Control.Monad.Trans.Writer (runWriterT, tell, WriterT)

import Structures (CodeGenBlock (..), CodeGenTree (..))

data Generator = Generator {
      uuidCounter :: Int
    , indentLevel :: Int
}

newGenerator :: Generator
newGenerator = Generator { uuidCounter = 0, indentLevel = 0 }

type Generation = WriterT [String] (State Generator)

uuid :: Generation Int
uuid = do
    count <- liftM uuidCounter get
    modify (\compiler -> compiler { uuidCounter = count + 1 })
    return count

incrIndent :: Generation ()
incrIndent = do
    indent <- liftM indentLevel get
    modify (\compiler -> compiler { indentLevel = indent + 1 })

decrIndent :: Generation ()
decrIndent = do
    indent <- liftM indentLevel get
    modify (\compiler -> compiler { indentLevel = indent - 1 })

emit :: String -> Generation ()
emit s = do
    indent <- liftM indentLevel get
    tell [concat (replicate indent "    ") ++ s]

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
    prefix <- liftM (\i -> "op" ++ show i) uuid
    let varNames        = map ((prefix ++) . show) [1 .. (length arguments)]
        call [] _       = return $ name ++ "(" ++ intercalate "," varNames ++ ")"
        call (e:es) n   = do
            value <- process e
            emitDeclAssign (prefix ++ show n) value
            call es (n + 1)
    call arguments 1

processCond :: CodeGenTree -> CodeGenTree -> CodeGenTree -> Generation String
processCond condition consequent alternative = do
    name <- liftM (\i -> "if" ++ show i) uuid
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

process :: CodeGenTree -> Generation String
process (CGImmediate s)     = return s
process (CGCall n es)       = processCall n es
process (CGIf c e1 e2)      = processCond c e1 e2
process (CGBlock s xs e)    = do -- TODO: fix ugly ass code, rename CGBlock?
    emitVariableDecl s
    emit "{"
    incrIndent
    aa <- mapM (process . snd) xs
    mapM_ (uncurry emitDeclAssign) (zip (map fst xs) aa)
    newLine
    res <- process e
    newLine
    emitAssign s res
    decrIndent
    emit "}"
    return s
process (CGLambda n a fs)   = do -- TODO: move into separate function? put into c code?
    name <- liftM (\i -> "env" ++ show i) uuid
    -- TODO: check if 0 args, then just pass null
    emit $ "value_t* " ++ name ++ " = malloc(sizeof(value_t) * " ++ show (length fs + 1) ++ ");"
    mapM_ (\(s, n) -> emitAssign (name ++ "[" ++ show n ++ "]") s) (zip fs [0..])
    emitAssign (name ++ "[" ++ show (length fs) ++ "]") "0"
    return $ "_make_closure(" ++ n ++ ", " ++ show a ++ ", " ++ name ++ ")"

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
    emit "#include <stdlib.h>"
    emit "#include \"runtime.h\""
    emit "#include \"primitives.h\""
    newLine
    mapM_ (\(CodeGenBlock n p _) -> emitFunctionDecl n p) blocks
    mapM_ generateBlock blocks

generate :: [CodeGenBlock] -> String -> IO ()
generate blocks path = do
    let ((_, code), _) = runState (runWriterT (generate' blocks)) newGenerator
    writeFile path (unlines code)
