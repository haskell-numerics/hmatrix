
-- Alternative interface and utilities for creation of real arrays, useful to work in interactive mode.

module Real(
    module Numeric.LinearAlgebra,
    (<>), (*>), (<*), (<\>), (\>),
    vector,
    eye,
    zeros, ones,
    diagl,
    row,
    col,
    (#),(&), (//), blocks,
    rand
) where

import Numeric.LinearAlgebra hiding ((<>), (<|>), (<->), (<\>), (.*), (*/))
import System.Random(randomIO)

infixl 7 <>
-- | Matrix product ('multiply')
(<>) :: Field t => Matrix t -> Matrix t -> Matrix t
(<>) = multiply

infixl 7 *>
-- | matrix x vector
(*>) :: Field t => Matrix t -> Vector t -> Vector t
m *> v = flatten $ m <> (asColumn v)

infixl 7 <*
-- | vector x matrix
(<*) :: Field t => Vector t -> Matrix t -> Vector t
v <* m = flatten $ (asRow v) <> m


-- | Least squares solution of a linear system for several right-hand sides, similar to the \\ operator of Matlab\/Octave. (\<\\\>) = 'linearSolveSVD'.
(<\>) :: (Field a) => Matrix a -> Matrix a -> Matrix a
infixl 7 <\>
(<\>) = linearSolveSVD

-- | Least squares solution of a linear system for a single right-hand side. See '(\<\\\>)'.
(\>) :: (Field a) => Matrix a -> Vector a -> Vector a
infixl 7 \>
m \> v = flatten (m <\> reshape 1 v)

-- | Pseudorandom matrix with uniform elements between 0 and 1.
rand :: Int -- ^ rows
     -> Int -- ^ columns
     -> IO (Matrix Double)
rand r c = do
    seed <- randomIO
    return (reshape c $ randomVector seed Uniform (r*c))

-- | Real identity matrix.
eye :: Int -> Matrix Double
eye = ident

-- | Create a real vector from a list.
vector :: [Double] -> Vector Double
vector = fromList

-- | Create a real diagonal matrix from a list.
diagl :: [Double] -> Matrix Double
diagl = diag . vector

-- | Create a matrix or zeros.
zeros :: Int -- ^ rows
      -> Int -- ^ columns
      -> Matrix Double
zeros r c = reshape c (constant 0 (r*c))

-- | Create a matrix or ones.
ones :: Int -- ^ rows
     -> Int -- ^ columns
     -> Matrix Double
ones r c = reshape c (constant 1 (r*c))

-- | Concatenation of real vectors.
infixl 9 #
(#) :: Vector Double -> Vector Double -> Vector Double
a # b = join [a,b]

-- | Horizontal concatenation of real matrices.
infixl 8 &
(&) :: Matrix Double -> Matrix Double -> Matrix Double
a & b = fromBlocks [[a,b]]

-- | Vertical concatenation of real matrices.
infixl 7 //
(//) :: Matrix Double -> Matrix Double -> Matrix Double
a // b = fromBlocks [[a],[b]]

-- | Real block matrix from a rectangular list of lists.
blocks :: [[Matrix Double]] -> Matrix Double
blocks = fromBlocks

-- | A real matrix with a single row, create from a list of elements.
row :: [Double] -> Matrix Double
row = asRow . vector

-- | A real matrix with a single column, created from a list of elements.
col :: [Double] -> Matrix Double
col = asColumn . vector

