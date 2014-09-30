import Analysis
import CodeGen
import Structures

main :: IO ()
main = do
    let a = (PInt 42)
    let b = (PInt 10)
    let c = (PInt 5)
    let f = (PIf (PBool False) b c)

    let cc = (PIf (PBool False) c (PVar "x"))
    let aa = (PLambda ["x"] cc)
    let bb = (PApply aa [PVar "first"])

    let d = (PLet [("first", (PInt 30)), ("second", c)] bb)

    let v1 = analyze d
    generate v1 "test.c"
