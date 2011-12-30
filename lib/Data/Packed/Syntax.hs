{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-name-shadowing #-}

module Data.Packed.Syntax(vec, mat) where

import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote as TH
import Language.Haskell.TH.Syntax as TH

import Language.Haskell.Exts as HSE
import qualified Language.Haskell.Meta.Syntax.Translate as MT

import Data.Packed.Vector
import Data.Packed.Matrix
import Data.Packed.ST
import Data.Packed.Internal(at', atM')
import Data.Packed.Development(MatrixOrder(..))

import Control.Applicative

-- | Quasiquoter for vectors. For example, use as an expression:
--
-- > buildVec x = [vec| x, sin x |]
--
-- or use as a pattern:
--
-- > swap [vec| x, y |] = [vec| y, x |]
-- 
-- The following language pragma is needed to use this syntax:
--
-- > {-# LANGUAGE QuasiQuotes, ViewPatterns #-}
vec :: QuasiQuoter
vec = qq vecExp vecPat

-- | Quasiquoter for matrices. For example, use as an expression:
--
-- > buildMat x y = [mat| x,     y;
-- >                      x + y, sin y |]      
--
-- or use as a pattern:
--
-- > adjugateMat2 [mat| a, b; c, d |] = [mat| d, -b; -c, a |]
--
-- If row sizes don't match, this will be caught at compile time. 
-- The following language pragma is needed to use this syntax:
--
-- > {-# LANGUAGE QuasiQuotes, ViewPatterns #-}
mat :: QuasiQuoter
mat = qq matExp matPat

qq exp pat = QuasiQuoter exp pat (const $ fail "Type quasiquotes not supported") (const $ fail "Declaration quasiquotes not supported")

wrap s = "[" ++ s ++ "]"


-- TODO: remove the intermediate lists in the following

-- approach to parsing vectors: surround with [] brackets and parse as a list

vecExp :: String -> Q TH.Exp
vecExp s = case parseExp (wrap s) of
  ParseOk (List es) -> buildVectorST (map MT.toExp es)
  ParseOk _ -> fail "unexpected parse"
  ParseFailed _loc msg -> fail msg

buildVectorST es = 
  [| runSTVector (do
                     v <- newUndefinedVector $( lift (length es) )
                     $( let buildWrites _i [] = [| return () |]
                            buildWrites i (exp:exps) = [| unsafeWriteVector v i $(return exp) >> $(buildWrites (i+1) exps) |]
                        in buildWrites 0 es)
                     return v) |]

buildToList n = [| \vec -> if dim vec /= n then Nothing 
                           else Just 
                                $(let buildList i | i == n    = [| [] |]
                                                  | otherwise = [| at' vec i : $(buildList (i+1)) |]
                                  in buildList 0) |]
  
vecPat :: String -> Q TH.Pat
vecPat s = case parsePat (wrap s) of
  ParseOk l@(PList ps) -> viewP (buildToList (length ps)) (conP 'Just [return $ MT.toPat l])
  ParseOk _ -> fail "unexpected parse"
  ParseFailed _loc msg -> fail msg


-- approach to parsing matrices: surround with [] brackets, and repeatedly parse. Will get a parse error with message semiParseError when we encounter an "unexpected" semicolon: we break at this point, and continue parsing
semiParseError = "Parse error: ;"

-- | find the location in the given string, returning everything strictly before; and everything strictly after
-- the character *at* the location is dropped
splitAtLoc :: SrcLoc -> String -> (String, String)
splitAtLoc loc s = case splitAt (srcLine loc - 1) (lines s) of
  (linesBefore, line:linesAfter) -> case splitAt (srcColumn loc - 1) line of
    (lineStart, _:lineEnd) -> (concat linesBefore ++ lineStart, lineEnd ++ concat linesAfter)

breakOnSemis :: (String -> ParseResult res) -> String -> ParseResult [res]
breakOnSemis parse s = case parse wrapped_s of
  ParseOk r -> ParseOk [r]
  ParseFailed loc msg | msg == semiParseError -> 
    case splitAtLoc loc wrapped_s of
      ('[': h, init -> t) -> (:) <$> parse (wrap h) <*> breakOnSemis parse t
                      | otherwise -> ParseFailed loc msg
 where wrapped_s = wrap s

unList (List l) = l
matExp s = case breakOnSemis parseExp s of
  ParseOk rows@(r:_) -> let rowLen = length (unList r)
                        in
                         if all (\r' -> length (unList r') == length (unList r)) rows 
                         then buildMatST (map (map MT.toExp . unList) rows)
                         else fail "Not all rows have the same length"
  ParseFailed _loc msg -> fail msg

buildMatST :: [[TH.Exp]] -> Q TH.Exp
buildMatST es =
  let r = length es
      c = length (head es)
  in
  [| runSTMatrix 
       (do
          m <- newUndefinedMatrix RowMajor r c
          $( let writes = [ [| unsafeWriteMatrix m ir ic $(return $ es !! ir !! ic) |] | ir <- [0..r-1], ic <- [0..c-1] ]
             in foldr (\h t -> [| $h >> $t |]) [| return () |] writes)
          return m
       ) |]

unPList (PList l) = l

matPat s = case breakOnSemis parsePat s of
  ParseOk rows@(r:_) -> let rowLen = length (unPList r)
                            colLen = length rows
                        in
                         if all (\r' -> length (unPList r') == length (unPList r)) rows 
                         then viewP (buildToLists colLen rowLen) (conP 'Just [return $ ListP (map MT.toPat rows)])
                         else fail "Not all rows have the same length"
  ParseFailed _loc msg -> fail msg

buildToLists r c =
  [| \m -> if (rows m, cols m) /= (r, c) then Nothing
           else Just 
                $( TH.listE [ TH.listE [ [| atM' m ir ic |] | ic <- [0..c-1] ] | ir <- [0..r-1] ] )
   |]
