module Parser (
      parse
) where

import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P (parse)

import Structures (RawTree(..), ProgramTree(..))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

parseAtom :: Parser RawTree
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> symbol <|> digit)
    return $ RAtom (first:rest)

parseInt :: Parser RawTree
parseInt = (RInt . read) <$> many1 digit

parseList :: Parser RawTree
parseList = do
    char '('
    x <- sepBy parseExpr (skipMany1 space)
    char ')'
    return $ RList x

parseExpr :: Parser RawTree
parseExpr = parseAtom
        <|> parseInt
        <|> parseList

parse :: String -> ProgramTree
parse input = case P.parse parseExpr "parser" input of
    Left e  -> error $ "Parser error: " ++ show e
    Right t -> transform t

transformLet :: [RawTree] -> ProgramTree
transformLet [RList vs, b]  = PLet (map bind vs) (transform b)
    where bind (RList [RAtom s, v]) = (s, transform v)
          bind _                    = error "Improper let binding"
transformLet _              = error "Let must be applied to a set of bindings and a body"

transformIf :: [RawTree] -> ProgramTree
transformIf [c, e1, e2] = PIf (transform c) (transform e1) (transform e2)
transformIf _           = error "If must be applied to 3 arguments"

-- TODO: put in monad
transformLambda :: [RawTree] -> ProgramTree
transformLambda [RList vs, b]           = transformLambda' "FIXME" vs b
transformLambda [RAtom n, RList vs, b]  = transformLambda' n vs b
transformLambda _                       = error "Syntax error in defining lambda"

transformLambda' :: String -> [RawTree] -> RawTree -> ProgramTree
transformLambda' name vars body = PLambda name (map getName vars) (transform body)
    where getName (RAtom s) = s
          getName _         = error "Invalid argument name"

transform :: RawTree -> ProgramTree
transform (RAtom "'()")  = PNil
transform (RAtom "#f")   = PBool False
transform (RAtom "#t")   = PBool True
transform (RAtom s)      = PVar s
transform (RInt i)       = PInt i
transform (RList (e:es)) = case transform e of
    (PVar "if")     -> transformIf es
    (PVar "let")    -> transformLet es
    (PVar "lambda") -> transformLambda es
    v               -> PApply v (if not (null es) then map transform es else [])
transform (RList [])     = PNil

