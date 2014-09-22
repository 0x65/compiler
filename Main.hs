import Analysis
import CodeGen
import Structures

main :: IO ()
main = do
    let a = (PInt 42)
    let b = (PInt 10)
    let c = (PInt 5)
    let d = (PLet [("first", (PInt 30)), ("second", (PBool True))] (PVar "second"))
    let f = (PIf (PBool False) b c)
    let v1 = analyze d
    generate v1 "test.c"
