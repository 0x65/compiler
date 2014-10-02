import Analysis
import CodeGen
import Structures

main :: IO ()
main = do
    let a = (PInt 42)
    let b = (PInt 10)
    let c = (PInt 5)

    let f = PLambda ["y"] (PIf (PBool False) (PVar "y") (PVar "x"))
    let g = PLambda ["y"] (PApply (PVar "add") [PVar "x", PVar "y"])
    let e = PLet [("x", c)] (PApply g [PInt 10])

    let v1 = analyze e
    generate v1 "test.c"
