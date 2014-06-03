{-# OPTIONS_HADDOCK hide #-}

module Numeric.Container(
    module Data.Packed,
    constant,
    linspace,
    diag,
    ident,
    ctrans,
    Container(scaleRecip, addConstant,add, sub, mul, divide, equal),
    scalar,
    conj,
    scale,
    arctan2,
    cmap,
    Konst(..),
    Build(..),
    atIndex,
    minIndex, maxIndex, minElement, maxElement,
    sumElements, prodElements,
    step, cond, find, assoc, accum,
    Element(..),
    Product(..), dot, udot,
    optimiseMult,
    mXm, mXv, vXm, (<.>),
    Mul(..),
    LSDiv, (<\>),
    outer, kronecker,
    RandDist(..),
    randomVector, gaussianSample, uniformSample,
    meanCov,
    Convert(..),
    Complexable,
    RealElement,
    RealOf, ComplexOf, SingleOf, DoubleOf, IndexOf,
    module Data.Complex,
    dispf, disps, dispcf, vecdisp, latexFormat, format,
    loadMatrix, saveMatrix, readMatrix
) where


import Data.Packed.Numeric
import Data.Packed
import Data.Packed.Internal(constantD)
import Data.Complex

constant :: Element a => a -> Int -> Vector a
constant = constantD

