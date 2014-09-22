import Analysis
import CodeGen
import Trees

main :: IO ()
main = do
    let a = (PInt 42)
    let b = (PInt 10)
    let c = (PInt 5)
    let d = (PCall "add" [b, c]) -- 15
    let e = (PCall "add" [d, a]) -- 57
    let f = (PIf (PBool False) d e)
    let v1 = analyze f
    let v2 = generate v1
    putStrLn (unlines v2)
