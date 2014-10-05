module Parser (
      parse
) where

import Control.Applicative ((<$>))
import Data.Maybe
import Prelude hiding (readList)
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P (parse)

import Structures

symbol :: Parser Char
symbol = oneOf "_"

readAtom :: Parser RawTree
readAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> symbol <|> digit)
    return $ RAtom (first:rest)

readInt :: Parser RawTree
readInt = (RInt . read) <$> many1 digit

readList :: Parser RawTree
readList = do
    char '('
    x <- sepBy readExpr (skipMany1 space)
    char ')'
    return $ RList x

readExpr :: Parser RawTree
readExpr = readAtom
        <|> readInt
        <|> readList

getName :: RawTree -> String
getName (RAtom s) = s
getName _         = error "Invalid argument name"

transformLet :: [RawTree] -> ProgramTree
transformLet [RList vs, b]  = PLet (map bind vs) (transform b)
    where bind (RList [RAtom s, v]) = (s, transform v)
          bind _                    = error "Improper let binding"
transformLet _              = error "Let must be applied to a set of bindings and a body"

transformIf :: [RawTree] -> ProgramTree
transformIf [c, e1, e2] = PIf (transform c) (transform e1) (transform e2)
transformIf _           = error "If must be applied to 3 arguments"

transformLambda :: [RawTree] -> ProgramTree
transformLambda [RList vs, b]           = PLambda Nothing (map getName vs) (transform b)
transformLambda [RAtom n, RList vs, b]  = PLambda (Just n) (map getName vs) (transform b)
transformLambda _                       = error "Syntax error in defining lambda"

transform :: RawTree -> ProgramTree
transform (RAtom "'()")  = PNil
transform (RAtom "#f")   = PBool False
transform (RAtom "#t")   = PBool True
transform (RAtom s)      = PVar s
transform (RInt i)       = PInt i
transform (RList [])     = PNil
transform (RList (e:es)) = case transform e of
    (PVar "if")     -> transformIf es
    (PVar "let")    -> transformLet es
    (PVar "lambda") -> transformLambda es
    v               -> PApply v (if not (null es) then map transform es else [])

parse :: String -> ProgramTree
parse input = case P.parse readExpr "parser" input of
    Left e  -> error $ "Parser error: " ++ show e
    Right t -> transform t
