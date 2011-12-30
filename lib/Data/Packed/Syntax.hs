{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-name-shadowing #-}

module Data.Packed.Syntax(vec, mat) where

import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote as TH

import Language.Haskell.Exts as HSE
import qualified Language.Haskell.Meta.Syntax.Translate as MT

import Data.Packed.Vector
import Data.Packed.Matrix

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
  ParseOk l@List{} -> [| fromList $( return $ MT.toExp l ) |]
  ParseOk _ -> fail "unexpected parse"
  ParseFailed _loc msg -> fail msg

  
vecPat :: String -> Q TH.Pat
vecPat s = case parsePat (wrap s) of
  ParseOk l@PList{} -> viewP [| toList |] (return $ MT.toPat l)
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
                         then [| fromLists $( return $ ListE (map MT.toExp rows) ) |]
                         else fail "Not all rows have the same length"
  ParseFailed _loc msg -> fail msg

unPList (PList l) = l

matPat s = case breakOnSemis parsePat s of
  ParseOk rows@(r:_) -> let rowLen = length (unPList r)
                        in
                         if all (\r' -> length (unPList r') == length (unPList r)) rows 
                         then viewP [| toLists |] (return $ ListP (map MT.toPat rows))
                         else fail "Not all rows have the same length"
  ParseFailed _loc msg -> fail msg

