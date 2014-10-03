import Analysis
import CodeGen
import Parser
import Structures

main :: IO ()
main = do
    let raw = parse "((lambda (x) (add x 1)) 4)"
    generate (analyze raw) "test.c"
