{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef

bubbleSort :: forall a . Ord a => [a] -> [a]
bubbleSort list = runST $ do
    let len = length list
    arr <- newListArray (0, len - 1) list :: ST s (STArray s Int a)
    forM_ [len-1, len-2 .. 1] $ \i -> do
        forM_ [0 .. i-1] $ \j -> do
            el1 <- readArray arr j
            el2 <- readArray arr $ j + 1
            when (el1 > el2) $ do
                writeArray arr j el2
                writeArray arr (j + 1) el1
    getElems arr
                
