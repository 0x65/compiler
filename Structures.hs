module Structures (
      RawTree (..)
    , ProgramTree (..)
    , CodeGenBlock (..)
    , CodeGenTree (..)
    , Compiler (..)
    , newCompiler
    , Compilation
    , runCompiler
    , uuid
    , incrIndent
    , decrIndent
) where

import Control.Monad
import Control.Monad.State
import Data.Maybe

data RawTree = RAtom String
             | RInt Int
             | RList [RawTree]

data ProgramTree = PInt Int
                 | PBool Bool
                 | PNil
                 | PVar String
                 | PLet [(String, ProgramTree)] ProgramTree
                 | PIf ProgramTree ProgramTree ProgramTree
                 | PLambda (Maybe String) [String] ProgramTree
                 | PApply ProgramTree [ProgramTree]

data CodeGenBlock = CodeGenBlock String [String] CodeGenTree

data CodeGenTree = CGImmediate String
                 | CGCall String [CodeGenTree]
                 | CGIf CodeGenTree CodeGenTree CodeGenTree
                 | CGLambda String Int [String]
                 | CGLet [(String, CodeGenTree)] CodeGenTree

data Compiler = Compiler {
      uuidCounter :: Int
    , indentLevel :: Int
}

newCompiler :: Compiler
newCompiler = Compiler { uuidCounter = 0, indentLevel = 0 }

type Compilation = State Compiler

runCompiler :: Compilation a -> a
runCompiler compilation = evalState compilation newCompiler

uuid :: MonadState Compiler m => String -> m String
uuid prefix = do
    count <- liftM uuidCounter get
    modify (\compiler -> compiler { uuidCounter = count + 1 })
    return $ prefix ++ show count

incrIndent :: MonadState Compiler m => m ()
incrIndent = do
    indent <- liftM indentLevel get
    modify (\compiler -> compiler { indentLevel = indent + 1 })

decrIndent :: MonadState Compiler m => m ()
decrIndent = do
    indent <- liftM indentLevel get
    modify (\compiler -> compiler { indentLevel = indent - 1 })
