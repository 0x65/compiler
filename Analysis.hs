module Analysis (
      analyze
) where

import Data.Bits (shift)

import Trees (ProgramTree (..), CodeGenTree (..))

-- later: make some config generator that gens .h and .hs file
fixNumShift = 0x2
fixNumMask  = 0x3
boolFalse   = 0x2F
boolTrue    = 0x6F
nil         = 0x3F

analyze :: ProgramTree -> CodeGenTree
analyze (PInt i)        = CGImmediate $ show (i `shift` fixNumShift)
analyze (PBool b)       = CGImmediate $ show (if (b) then boolTrue else boolFalse)
analyze PNil            = CGImmediate $ show nil
analyze (PCall s es)    = CGCall s (map analyze es)
analyze (PIf c e1 e2)   = CGIf (analyze c) (analyze e1) (analyze e2)
