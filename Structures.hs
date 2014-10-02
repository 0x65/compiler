module Structures (
      ProgramTree (..)
    , CodeGenBlock (..)
    , CodeGenTree (..)
) where

data ProgramTree = PInt Int
                 | PBool Bool
                 | PNil
                 | PLet [(String, ProgramTree)] ProgramTree
                 | PVar String
                 | PIf ProgramTree ProgramTree ProgramTree
                 | PLambda [String] ProgramTree
                 | PApply ProgramTree [ProgramTree]

data CodeGenBlock = CodeGenBlock String [String] CodeGenTree

data CodeGenTree = CGImmediate String
                 | CGCall String [CodeGenTree]
                 | CGIf CodeGenTree CodeGenTree CodeGenTree
                 | CGLambda String Int [String]
