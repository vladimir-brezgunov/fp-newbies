module SimpleInteractiveInterpreter (
    Interpreter,
    newInterpreter,
    Result,
    input
) where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr

{- * Parsing -}

calcLang = makeTokenParser $ emptyDef {
    identStart = letter,
    identLetter = alphaNum,
    reservedNames = ["fn"],
    reservedOpNames = ["+", "-", "*", "/", "%", "=>"],
    opLetter = oneOf "*/+-%"
}

data Expr = Const Double | BinaryOp Op Expr Expr

pConst = Const . toDouble <$> naturalOrFloat calcLang where
    toDouble (Left l) = fromIntegral l
    toDouble (Right r) = r

pExpr = buildExpressionParser table pTerm

pTerm = parens calcLang pExpr <|> pConst

{-
table = [
            [ char '*' *> return (BinaryOp Mul) ,
              char '/' *> return (BinaryOp Div) ],
            [ char '+' *> return (BinaryOp Add),
              char '-' *> return (BinaryOp Sub) ]
        ]
-}

operators = [ [('*', Mul), ('/', Div)],
              [('+', Add), ('-', Sub)] ]

table = [ [ Infix (char ch *> whiteSpace calcLang *> return (BinaryOp op)) AssocLeft
          | (ch, op) <- opGroup ]
        | opGroup <- operators ]

eval :: Expr -> Double
eval (Const val) = val
eval (BinaryOp op left right) = opDo (eval left) (eval right) where
    opDo = case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> (/)

data Interpreter
type Result = Maybe Double
data Op = Add | Sub | Mul | Div | Rem

newInterpreter :: Interpreter
newInterpreter = undefined

input :: String -> Interpreter -> Either String (Result, Interpreter)
input str interpreter = case parse (pExpr <* eof) "" str of
    Left err -> Left $ show err
    Right e -> Right (Just $ eval e, interpreter)