import Analysis
import CodeGen
import Structures

main :: IO ()
main = do
    let a = (PInt 42)
    let b = (PInt 10)
    let c = (PInt 5)


    let cons (a, b) = PApply (PVar "cons") [a, b]
    let theList = cons (PInt 1, cons(PInt 2, cons(PInt 3, PNil)))
    let addOne = PLambda "addOne" ["x"] 
                    (PApply (PVar "add") [PVar "x", PInt 1])

    let test = (PLet [("first", (PApply (PVar "car") [PVar "xs"]))]
                        (PIf (PApply (PVar "is_nil") [PVar "first"])
                             (PNil)
                             --(PApply (PVar "map") [PVar "f", cons (PNil, PNil)])
                             --(PApply (PVar "f") [PVar "first"])
                             (cons (PApply (PVar "f") [PVar "first"], (PApply (PVar "map") [PApply (PVar "cdr") [PVar "xs"]])))
                             --(PApply (PVar "cdr") [PVar "xs"])
                        )
                    )
    let map = PLambda "map" ["f", "xs"] test

    print $ freeVars test ["xs"]

    let prog = PLet [("map", map)] (PApply (PVar "map") [addOne, theList])
    --let prog = PApply (PLambda "map" ["x"] (PLet [("y", PApply addOne [PVar "x"])] (PVar "y"))) [(PInt 1)]

    let v1 = analyze prog
    generate v1 "test.c"
