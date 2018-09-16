{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Prelude hiding (Right)

--Нужные монады
import Control.Monad.State
import Control.Monad.Writer

--Мемоизатор функций
import Data.Function.Memoize

--Необходимые структуры данных
import qualified Data.Array as A
import Data.Array ((!))
import Data.List
import qualified Data.Map as M

--Юнит-тесты
import Test.QuickCheck

---- Определения --------------------------------------------------------------

--Клетка матрицы – это пара (строка, столбец)
type Cell = (Int, Int)

--Матрица – это массив, индексированный клетками
type Matrix a = A.Array Cell a

--Задача: найти кратчайший путь (с минимальным весом) из левого верхнего
--в правый нижний угол, идя только вправо или вниз

--Несколько матриц для тестов
test1, test2, test3, test4 :: Matrix Int
test1 = A.listArray ((1, 1), (2, 2)) [ 1, 2
                                     , 3, 4]

test2 = A.listArray ((1, 1), (4, 4)) [  1,  2,  3,  4
                                     ,  5, 10, 30, 50
                                     ,  6, 11, 13,  3
                                     , 10, 12, 15,  2]

test3 = A.listArray ((1, 1), (6, 6)) [  1,  3,  4,  5,  6,  7
                                     ,  2, 10,  6, 50, 10, 20
                                     ,  6, 11,  7,  8, 10, 14
                                     , 10, 12, 15,  9, 10, 11
                                     , 13, 14, 15, 16, 12, 13
                                     , 20, 21, 22, 23, 24, 25]

test4 = A.array ((1,1),(5,6)) [((1,1),-16),((1,2),19),((1,3),1),((1,4),-13),((1,5),23),((1,6),-15),((2,1),22),((2,2),24),((2,3),-7),((2,4),0),((2,5),18),((2,6),0),((3,1),9),((3,2),-18),((3,3),-19),((3,4),4),((3,5),-13),((3,6),-6),((4,1),-8),((4,2),-5),((4,3),-23),((4,4),-13),((4,5),13),((4,6),-25),((5,1),-16),((5,2),-18),((5,3),-12),((5,4),7),((5,5),20),((5,6),-17)]

--Направление шага: вправо или вниз
data Direction = Right | Down
    deriving (Eq, Ord)

instance Show Direction where
    show Right = ">"
    show Down = "v"

--Путь – это последовательность направлений-шагов
type Path = [Direction]

--Сделать шаг в направлении из клетки в следующую
move, moveBack :: Direction -> Cell -> Cell
move Down  (i, j) = (i + 1, j)
move Right (i, j) = (i, j + 1)

moveBack Down  (i, j) = (i - 1, j)
moveBack Right (i, j) = (i, j - 1)

--Самый наивный алгоритм: перебрать все пути и выбрать кратчайший
shortestPathNaive :: (Num a, Ord a)
                  => (Int -> Int -> [Path]) --функция для генерации путей
                  -> (Matrix a -> Path -> [a]) --функция для обхода матрицы по пути
                  -> Matrix a
                  -> Path
shortestPathNaive pathGen traverser mat =
    snd $ minimum $ zip (map (sum . traverser mat) paths) paths where
        (_, (h, w)) = A.bounds mat
        paths = pathGen h w

---- Обход пути ---------------------------------------------------------------

--Простое рекурсивное определение обхода, возвращающее список ячеек
traverseRec :: Matrix a -> Path -> [a]
traverseRec mat = go (1, 1) where
    go _ [] = []
    go pos (dir:dirs) = (mat ! pos) : go (move dir pos) dirs

--То же самое, только со свёрткой вместо явной рекурсии
traverseFold :: Matrix a -> Path -> [a]
traverseFold mat = reverse . fst . foldl step ([], (1, 1)) where
    step (acc, pos) d = (mat ! pos : acc, move d pos)

--А теперь сделаем промежуточное состояние в императивном виде
--при помощи монады State
traverseState :: Matrix a -> Path -> [a]
traverseState mat path = reverse $ fst $ flip execState ([], (1, 1)) $ do
    forM_ path $ \dir -> do
        (acc, pos) <- get
        put (mat ! pos : acc, move dir pos)
        --modify (\(acc, pos) -> (mat ! pos : acc, move dir pos)) --то же самое

--Промежуточное состояние образовано двумя частями:
--полностью изменяемой координатой текущего шага
--и накапливаемым итоговым списком ячеек, для которого лучше подходит монада Writer.
--Поэтому состояние можно разделить на две части при помощи монадного трансформера –
--комбинации из State и Writer.
traverseStateWriter :: Matrix a -> Path -> [a]
traverseStateWriter mat path = execWriter $ flip evalStateT (1, 1) $ do
    forM_ path $ \dir -> do
        pos <- get --получить текущую координату
        tell $ [mat A.! pos] --дописать ячейку в путь
        put $ move dir pos --изменить координату на новую

--Предыдущий вариант был "совсем императивным", а теперь "совсем функциональный"
traverseFunctional :: Matrix a -> Path -> [a]
traverseFunctional mat = tail . map (mat A.!) . scanl (flip move) (1, 1)

---- Перебор путей ------------------------------------------------------------

--Простое рекурсивное определение всех возможных путей
allPathsNaive n m = go (n, m) where
    go (1, 1) = [[]]
    go (0, _) = []
    go (_, 0) = []
    go pos = do
        --Генерация комбинаций при помощи монады списка
        dir <- [Right, Down]
        rest <- go (moveBack dir pos)
        return (dir:rest)
        --То же самое с list comprehension
        --[dir:rest | dir <- [Right, Down], rest <- go (moveBack dir pos)]

--То же самое с неявной мемоизацией промежуточных вычислений,
--что существенно снижает сложность алгоритма
allPathsMemoized n m = memoFix go (n, m) where
    go _ (1, 1) = [[]]
    go _ (0, _) = []
    go _ (_, 0) = []
    go f pos = [dir:rest | dir <- [Right, Down], rest <- f (moveBack dir pos)]
                  --map (Right:) (f (x-1, y)) ++ map (Down:) (f (x, y-1)

---- Поиск пути ---------------------------------------------------------------

--Простое рекурсивное вычисление кратчайшего пути
shortestPathRec :: (Ord a, Num a) => Matrix a -> Path
shortestPathRec mat = reverse $ snd $ shortestAt size where
    (_, size) = A.bounds mat
    --Локальная функция, вычисляющая кратчайший путь до определённой клетки
    shortestAt (1, 1) = (0, [])
    shortestAt c@(i, j) | i == 1    = tryRight
                        | j == 1    = tryDown
                        | otherwise = min tryRight tryDown
        where
            curScore = mat A.! c
            (scoreRight, pathRight) = shortestAt (moveBack Right c)
            (scoreDown,  pathDown)  = shortestAt (moveBack Down c)
            tryRight = (curScore + scoreRight, Right : pathRight)
            tryDown  = (curScore + scoreDown,  Down : pathDown)        

--Оптимизированный вариант с явной мемоизацией промежуточных результатов,
--для чего используется словарик с путями, изменяемый в монаде State
shortestPathState :: (Ord a, Num a) => Matrix a -> Path
shortestPathState mat = reverse $ snd $ (M.! size) $ flip execState M.empty $ do
    modify $ M.insert (1, 1) (0, [])
    forM_ [1 .. h] $ \i -> do
        forM_ [1 .. w] $ \j -> do
            let c = (i, j)
            (prevScore, prevPath) <- gets (M.! c)
            --gets раскрывается в
            --temp <- get
            --let (prevScore, prevPath) = temp M.! c
            let newScore = prevScore + mat ! c
            modify $ M.insertWith min (move Right c) (newScore, Right : prevPath)
            modify $ M.insertWith min (move Down c)  (newScore, Down : prevPath)
    where
        (_, size@(h, w)) = A.bounds mat

---- Тесты --------------------------------------------------------------------
--Немножечко юнит-тестов с применением библиотеки QuickCheck
--Сравнивать будем эффективную реализацию с эталонной наивной

--Генератор случайного размера массива, не слишком большого,
--чтобы наивная реализация считалась приемлемое время
--Для этого нужно завернуть целое в собственный тип, чтобы определить для него
--собственную реализацию случайного генератора
newtype MatrixSize = MatrixSize Int
    deriving Show

instance Arbitrary MatrixSize where
    arbitrary = MatrixSize <$> choose (2, 6) --случайное целое от 2 до 6

prop_allPaths (MatrixSize h) (MatrixSize w) = allPathsNaive w h == allPathsMemoized w h

--Генератор случайных массивов, использует монаду Gen
instance Arbitrary a => Arbitrary (Matrix a) where
    arbitrary = do
        (MatrixSize h) <- arbitrary
        (MatrixSize w) <- arbitrary
        elements <- vectorOf (h * w) arbitrary --Случайно сгенерируем соответствующее число клеток
        return $ A.listArray ((1, 1), (h, w)) elements

shortestPathEtalon = shortestPathNaive allPathsNaive traverseRec

prop_shortestPath :: Matrix Int -> Bool
prop_shortestPath mat = score shortestPathEtalon == score shortestPathState where
    score fun = sum $ traverseRec mat (fun mat)

{-
Запустим и получим заветное:
*Main> quickCheck prop_shortestPathState
+++ OK, passed 100 tests.
-}