module SimpleInteractiveInterpreter (
    Interpreter,
    newInterpreter,
    Result,
    input
) where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)

{- * Parsing -}

calcLang = makeTokenParser $ emptyDef {
    identStart = letter,
    identLetter = alphaNum,
    reservedNames = ["fn"],
    reservedOpNames = ["+", "-", "*", "/", "%", "=>"]
}

data Expr = Const Double

pExpr = Const <$> float calcLang

data Interpreter
type Result = Maybe Double

newInterpreter :: Interpreter
newInterpreter = undefined

input :: String -> Interpreter -> Either String (Result, Interpreter)
input str interpreter = case parse pExpr "" str of
    Left err -> Left $ show err
    Right (Const val) -> Right (Just val, interpreter)