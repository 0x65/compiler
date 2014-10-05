import Analysis
import CodeGen
import Parser
import Structures

compile :: String -> Compilation String
compile input = do
    let program = parse input
    code <- analyze program
    generate code

main :: IO ()
main = do
    let input = "((lambda (x) (add x 1)) 4)"
    let output = runCompiler (compile input)

    writeFile "test.c" output
