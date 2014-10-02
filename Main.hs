import Analysis
import CodeGen
import Structures

main :: IO ()
main = do
    let cons (a, b) = PApply (PVar "cons") [a, b]

    let theList = cons (PInt 1,
                       cons(PInt 2,
                            cons(PInt 3,
                                 PNil)))

    let addOne = PLambda "addOne" ["x"] (PApply (PVar "add") [PVar "x", PInt 1])

    let map = PLambda "map" ["f", "xs"]
                (PIf
                    (PApply (PVar "is_nil") [PVar "xs"])
                    PNil
                    (cons (PApply
                            (PVar "f")
                            [PApply
                                (PVar "car")
                                [PVar "xs"]],
                          (PApply
                            (PVar "map")
                            [PVar "f", PApply
                                        (PVar "cdr")
                                        [PVar "xs"]]))))

    let prog = PLet [("map", map)] (PApply (PVar "map") [addOne, theList])

    generate (analyze prog) "test.c"
