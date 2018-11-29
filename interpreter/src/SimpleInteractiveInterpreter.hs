module SimpleInteractiveInterpreter where

import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor (first)
import Text.Parsec hiding (State)
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr
import qualified Data.Map as M
import Data.Maybe

{- * Определения -}
-- $definitions 
-- В маленьком язычке, который должен уметь наш интерпретатор-калькулятор, должны
-- поддерживаться следующие сущности:
--
--   - константы типа 'Double': @2@, @3.141@;
--   - переменные со строковым именем: @x@, @my_variable_42@;
--   - арифметические выражения с операторами и скобками: @2+2@, @foo*(bar-1)@;
--   - объявление функций: @fn inc x => x + 1@; @fn mean x y => (x + y) / 2@.
--
-- На данный момент поддерживаются только выражения с константами. Выражения – это
-- такие куски кода, которые можно вычислить и получить результат. Выражения могут
-- рекурсивно состоять из других подвыражений, и в ФП удобно представлять их
-- алгебраическими типами-суммами.

-- |Алгебраический тип для представления произвольных выражений.
data Statement
    = Fun String Params Expr
    | Line Expr
    deriving Show

type Params = [String]

data Expr
    = Const Double          -- ^Выражение может быть константным значением...
    | BinaryOp Op Expr Expr -- ^...бинарным оператором, применённым к левому и правому подвыражениям.
    | Var String            -- ^...переменной...
    | Assign String Expr    -- ^...либо оператором присваивания.
    deriving (Show, Eq)

-- |Простой тип-перечисление для поддерживаемых бинарных операторов.
data Op = Add | Sub | Mul | Div | Rem
    deriving (Show, Eq)

{- * Парсинг -}
-- $parsing
-- Для разбора выражений, введённых в виде строк, будем использовать широко применяемые
-- в ФП комбинаторные парсеры, которые декларативно описывают структуру разбираемого языка.
-- Возьмём популярную, промышленного качества библиотеку /Parsec/ (что важно, она идёт 
-- в поставке хаскеля на Codewars), которая позволит существенно автоматизировать
-- нашу задачу, а также выдаёт понятные сообщения об ошибках.

-- ** Вспомогательные структуры /Parsec/
-- $aux
-- В /Parsec/ из коробки есть средства для разбора лексем языка, которые берут на себя
-- всю заботу по обработке пробелов, комментариев, разборе числовых литералов и
-- экранированных строк и т. д. Для этого нужно задать описание некоторых характеристик
-- языка в специальной структуре и скормить её функции, которая выдаст пачку полезных
-- парсеров.

-- | Набор парсеров, полученный из описания того, что может встретиться в языке.
calcLang = makeTokenParser $ emptyDef {
    identStart = letter,
    identLetter = alphaNum,
    reservedNames = ["fn"],
    reservedOpNames = ["+", "-", "*", "/", "%", "=>", "="],
    opLetter = oneOf "*/+-%="
}

-- | Список, задающий, какой символ в какой оператор нужно превратить.
operators :: [[(Char, Op)]]
operators = [ [ ('*', Mul), ('/', Div) ],
              [ ('+', Add), ('-', Sub) ] ]

-- | Таблица парсеров операторов с убыванием приоритета, сконвертированная из 'operators'.
table = [ [ Infix parser AssocLeft
          | (ch, op) <- opGroup
          , let parser = char ch *> whiteSpace calcLang *> return (BinaryOp op) ]
        | opGroup <- operators ]

{- Код выше – более короткий способ записать кучу бойлерплейта по определению операторов, вроде:
table = [ [ Infix (char '*' *> return (BinaryOp Mul)) AssocLeft,
            Infix (char '/' *> return (BinaryOp Div)) AssocLeft ],
          [ Infix (char '+' *> return (BinaryOp Add)) AssocLeft,
        <...>
-}

-- ** Парсеры
-- $parsers
-- Теперь можно непосредственно приступить к разбору структур языка. Каждой сущности
-- (будь то константа, переменная, оператор, выражение) соответствует парсер, который
-- может её разобрать, а парсеры для составных сущностей строятся из более простых
-- применением различных функций-комбинаторов.

-- |Парсер числовых констант.
pConst = Const . toDouble <$> naturalOrFloat calcLang where
    toDouble (Left l) = fromIntegral l
    toDouble (Right r) = r

-- |Парсер арифметических выражений, который сгенерирован /Parsec/ из описания операторов.
pExpr = try pAssign <|> buildExpressionParser table pTerm

pAssign = Assign <$> pName <*> (reservedOp calcLang "=" *> pExpr)

-- |Парсер элементов, из которых могут составляться выражения -
-- сейчас это либо константы, либо подвыражения в скобках, либо переменные.
pTerm = parens calcLang pExpr <|> pConst <|> (Var <$> pName)

pName = identifier calcLang

pFun = do
    reserved calcLang "fn"
    name <- pName
    params <- many pName
    reservedOp calcLang "=>"
    expr <- pExpr
    return $ Fun name params expr

pStatement = (Line <$> pExpr <|> pFun) <* eof

{- * Вычисления -}
--
-- $evaluation
-- 
-- Чтобы получить результат, выражение нужно /вычислить/. Делается это рекурсивным обходом
-- значения 'Expr' с превращением в 'Double'-вый результат всего что можно.

eval :: Expr -> StateT Memory (Either String) Double
eval (Const val) = return val
eval (BinaryOp op left right) = do
    leftRes <- eval left
    rightRes <- eval right
    return $ opDo leftRes rightRes
    where
        opDo = case op of
            Add -> (+)
            Sub -> (-)
            Mul -> (*)
            Div -> (/)
eval (Var name) = do
    var <- gets (M.lookup name)
    maybe (throwError $ "Undefined variable: " ++ name) return var
    --case var of
    --    Just val -> return val
    --    Nothing  -> throwError $ "Undefined variable: " ++ name
    --mem <- get
    --return $ fromMaybe (error $ "Undefined variable: " ++ name) $ M.lookup name mem
    --gets (fromMaybe (error $ "Undefined variable: " ++ name) . M.lookup name)
eval (Assign name expr) = do
    res <- eval expr
    modify $ M.insert name res
    return res

{- * Выполнение -}
--
-- $interpretation
--
-- Теперь осталось запустить интерпретатор – скормить ему строку и получить результат.

-- |Результат – это опциональный 'Double'.
type Result = Maybe Double

-- |Тип для интерпретатора, который должен иметь информацию о переменных и функциях.
data Interpreter = Interpreter { 
    getMem :: Memory,
    funcs :: FunDefs
}

-- |Тип для хранения значений переменных.
type Memory = M.Map String Double

type FunDefs = M.Map String (Params, Expr)

-- |Средство создать новый экземпляр интерпретатора, пока тоже заглушка.
newInterpreter :: Interpreter
newInterpreter = Interpreter M.empty M.empty

-- |Итоговая функция, которую по требованию задачи должен экспортировать модуль.
-- Ей можно скормить строку с состоянием интерпретатора и получить результат
-- с новым состоянием (либо ошибку).
input :: String -> Interpreter -> Either String (Result, Interpreter)
input str interpreter = do
    stmt <- parseStatement str
    case stmt of
        Line expr -> do
            (res, mem) <- runStateT (eval expr) $ getMem interpreter
            return (Just res, interpreter {getMem = mem})
        Fun name params expr -> do
            undefined

parseStatement :: String -> Either String Statement
parseStatement = first show . parse pStatement ""