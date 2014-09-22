module Trees (
      ProgramTree (..)
    , CodeGenTree (..)
) where

data ProgramTree = PInt Int -- 30 bit int
                 | PBool Bool
                 | PNil
                 | PCall String [ProgramTree] -- temp, until let replaces this
                 | PIf ProgramTree ProgramTree ProgramTree

data CodeGenTree = CGImmediate String
                 | CGCall String [CodeGenTree]
                 | CGIf CodeGenTree CodeGenTree CodeGenTree
