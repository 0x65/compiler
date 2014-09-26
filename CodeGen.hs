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
}

newGenerator :: Generator
newGenerator = Generator { uuidCounter = 0 }

type Generation = WriterT [String] (State Generator)

uuid :: Generation Int
uuid = do
    count <- liftM uuidCounter get
    modify (\compiler -> compiler { uuidCounter = count + 1 })
    return count

emit :: String -> Generation ()
emit s = tell [s]

emitVariableDecl :: String -> Generation ()
emitVariableDecl name = emit $ "value_t " ++ name ++ ";"

emitAssign :: String -> String -> Generation ()
emitAssign name value = emit $ name ++ " = " ++ value ++ ";"

emitDeclAssign :: String -> String -> Generation ()
emitDeclAssign name value = emit $ "value_t " ++ name ++ " = " ++ value ++ ";"

emitFunctionSig :: String -> [String] -> Generation ()
emitFunctionSig name params =
    let params' = map ("value_t " ++) params
    in  emit $ "value_t " ++ name ++ "(" ++ intercalate "," params' ++ ")"

emitFunctionDecl :: String -> [String] -> Generation ()
emitFunctionDecl name params =
    let params' = map ("value_t " ++) params
    in  emit $ "value_t " ++ name ++ "(" ++ intercalate "," params' ++ ");"

processCall :: String -> [CodeGenTree] -> Generation String
processCall name arguments = do
    prefix <- liftM (\i -> "op" ++ show i) uuid
    let varNames        = map ((prefix ++) . show) [1 .. (length arguments)]
        call [] _       = return $ name ++ "(" ++ intercalate "," varNames ++ ");"
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
    alternativeVal <- process alternative
    emitAssign name alternativeVal
    emit "} else {"
    consequentVal <- process consequent
    emitAssign name consequentVal
    emit "}"
    return name

process :: CodeGenTree -> Generation String
process (CGImmediate s) = return s
process (CGCall n es)   = processCall n es
process (CGIf c e1 e2)  = processCond c e1 e2

generateBlock :: CodeGenBlock -> Generation ()
generateBlock (CodeGenBlock name params tree) = do
    emitFunctionSig name params
    emit "{"
    retVal <- process tree
    emit $ "return " ++ retVal ++ ";"
    emit "}"

generate' :: [CodeGenBlock] -> Generation ()
generate' blocks = do
    emit "#include \"runtime.h\""
    mapM_ (\(CodeGenBlock n p _) -> emitFunctionDecl n p) blocks
    mapM_ generateBlock blocks

generate :: [CodeGenBlock] -> String -> IO ()
generate blocks path = do
    let ((_, code), _) = runState (runWriterT (generate' blocks)) newGenerator
    writeFile path (unlines code)
