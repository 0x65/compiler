import Analysis
import CodeGen
import Structures

main :: IO ()
main = do
    let a = (PInt 42)
    let b = (PInt 10)
    let c = (PInt 5)
    let e = (PCall "add" [PInt 10, PInt 5])
    let g = (PCall "cons" [e, PBool False])
    let h = (PCall "car" [g])
    let d = (PLet [("first", (PInt 30)), ("second", h)] (PVar "second"))
    let f = (PIf (PBool False) b c)
    let v1 = analyze d
    generate v1 "test.c"
