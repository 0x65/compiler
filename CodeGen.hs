module CodeGen (
      generate
) where

import Control.Monad (liftM)
import Control.Monad.State (get, modify, runState, State)
import Data.List (intercalate)
import Control.Monad.Trans.Writer (runWriterT, tell, WriterT)

import Trees (CodeGenTree (..))

data Generator = Generator {
      uuidCounter :: Int
}

newGenerator :: Generator
newGenerator = Generator { uuidCounter = 0 }

type Generation = WriterT [String] (State Generator)

uuid :: Generation Int
uuid = do
    count <- liftM uuidCounter get
    modify (\compiler -> compiler { uuidCounter = (count + 1) })
    return count

emitDeclaration :: String -> Generation ()
emitDeclaration name = tell ["val " ++ name ++ ";"]

emitAssign :: String -> String -> Generation ()
emitAssign name value = tell [name ++ " = " ++ value ++ ";"]

emitDeclAssign :: String -> String -> Generation ()
emitDeclAssign name value = tell ["val " ++ name ++ " = " ++ value ++ ";"]

call :: String -> [CodeGenTree] -> Generation String
call name arguments = do
    prefix <- liftM (\i -> "i" ++ (show i) ++ "_op") uuid
    let varNames        = map (uncurry (++)) (zip (repeat prefix) (map show [1..(length arguments)]))
        call' [] _      = return $ name ++ "(" ++ (intercalate "," varNames) ++ ");"
        call' (e:es) n   = do
            value <- generate' e
            emitDeclAssign (prefix ++ (show n)) value
            call' es (n + 1)
    call' arguments 1

cond :: CodeGenTree -> CodeGenTree -> CodeGenTree -> Generation String
cond condition consequent alternative = do
    name <- liftM (\i -> "r" ++ (show i)) uuid
    conditionVal <- generate' condition
    emitDeclaration name
    tell ["if ((" ++ conditionVal ++ ") == BOOL_F) {"]
    alternativeVal <- generate' alternative
    emitAssign name alternativeVal
    tell [" } else { "]
    consequentVal <- generate' consequent
    emitAssign name consequentVal
    tell ["}"]
    return name

generate' :: CodeGenTree -> Generation String
generate' (CGImmediate s) = return s
generate' (CGCall n es)   = call n es
generate' (CGIf c e1 e2)  = cond c e1 e2

prelude :: [String]
prelude = ["typedef unsigned int val;", "#define BOOL_F 0x2F", "val add(val a, val b);"]

generate :: CodeGenTree -> [String]
generate t = let ((val, statements), _) = runState (runWriterT (generate' t)) newGenerator
             in    (prelude
                ++ ["unsigned int _entry_point() {"]
                ++ statements
                ++ ["return " ++ val ++ ";", "}"])
