-- automatic generation of wrappers for simple GSL special functions

import Text.ParserCombinators.Parsec
import System
import Data.List(intersperse, isPrefixOf)
import Data.Char(toUpper,isUpper,toLower)

data Type = Normal Ident | Pointer Ident deriving (Eq, Show)

type Ident = String

data Header = Header Type Ident [(Type,Ident)] deriving Show

headers f = case parse parseHeaders "" f of
    Right l -> l
    Left s -> error (show s)


rep (c,r) [] = []
rep (c,r) f@(x:xs) 
  | c `isPrefixOf` f = r ++ rep (c,r) (drop (length c) f)
  | otherwise        = x:(rep (c,r) xs)


fixlong [] = []
fixlong "\\" = []
fixlong ('\\':'\n':xs) = xs
fixlong (x:xs) = x : fixlong xs


safe (Header _ _ args) =  all ok args 
                         || all ok (init args) && kn (last args)
    where ok ((Normal s),_) | s `elem` ["double","float","int","gsl_mode_t"] = True
          ok _ = False
          kn ((Pointer "gsl_sf_result"),_) = True
          kn ((Pointer "gsl_sf_result_e10"),_) = True
          kn _ = False



fixC s = rep ("gsl_mode_t","int") $ rep ("gsl_sf_result","double") $ rep ("gsl_sf_result_e10","double") $ s

main = do
    args <- getArgs
    file <- readFile (args!!1)
    let name = args!!0
    putStrLn (args!!1)
    --mapM_ print (headers $ fixlong file)
    let parsed = (headers $ fixlong file)
    writeFile (name ++".h") (fixC $ unlines $ map showC parsed) 

    --putStrLn ""
    --mapM (\(Header _ n _) -> putStrLn (drop 7 n ++",")) parsed
    --putStrLn ""
    --mapM_ (putStrLn.showFull (name ++".h")) parsed
    let exports = rep (")",") where") $ rep ("(\n","(\n  ") $ rep (",\n",", ") $ unlines $ ["("]++intersperse "," (map (\(Header _ n _) -> hName n) (filter safe parsed))++[")"]
    let defs = unlines $ map (showFull (name ++".h")) parsed
    let imports = "\nimport Foreign(Ptr)\nimport GSL.Special.Internal\n"
    let mod = modhead name ++ "module GSL.Special."++ upperFirst name++exports++imports++defs
    writeFile (upperFirst name ++ ".hs") mod


modhead name = replicate 60 '-' ++ "\n"
             ++"{- |\n"
             ++"Module      :  GSL.Special."++upperFirst name++"\n"
             ++"Copyright   :  (c) Alberto Ruiz 2006\n"
             ++"License     :  GPL-style\n"
             ++"Maintainer  :  Alberto Ruiz (aruiz at um dot es)\n"
             ++"Stability   :  provisional\n"
             ++"Portability :  uses ffi\n"
             ++"\n\n\n-}\n"
             ++ replicate 60 '-' ++ "\n\n"

upperFirst (x:xs) = toUpper x : xs

comment = do
    string "/*"
    closecomment
    spaces
    return "comment"

closecomment = try (string "*/")
               <|> (do anyChar
                       closecomment)

ident = do 
    spaces
    id <- many1 (noneOf "()[]* \n\t,;")
    spaces
    return id

comment' = between (char '(') (char ')') (many $ noneOf ")") 


define = do
    string "#"
    closedefine
    spaces
    return "define"

closedefine = try (string "\n")
              <|> (do anyChar
                      closedefine)

marks = do
    try (string "__BEGIN_DECLS" >> spaces >> return "begin")
    <|>
    try (string "__END_DECLS" >> spaces >> return "end")



irrelevant =
    try comment
    <|>
    try define
    <|>
    marks


parseHeaders = many parseHeader

parseHeader = do
    spaces
    many irrelevant
    spaces
    (res,name)  <- typ
    spaces
    args <- between (char '(') (char ')') (sepBy typ (char ','))
    spaces
    char ';'
    spaces
    many irrelevant
    return $ Header res name args

typ = try t1 <|> t2

symbol s = spaces >> string s >> spaces

t1 = do
    t <- try (symbol "const" >> symbol "unsigned" >> ident) -- aaagh 
         <|>
         try (symbol "const" >> ident)
         <|>
         try (symbol "unsigned" >> ident)
         <|> ident
    n <- ident
    return (Normal t,n)

t2 = do
    t <- ident
    spaces
    char '*'
    spaces
    n <- ident
    return (Pointer t,n)

pure (Header _ _ args) | fst (last args) == Pointer "gsl_sf_result" = False
                       | fst (last args) == Pointer "gsl_sf_result_e10" = False
                       | otherwise = True

showC (Header t n args) = showCt t ++ " " ++ n ++ "("  ++ (concat $ intersperse "," $ map showCa args) ++ ");"

showCt (Normal s) = s
showCt (Pointer s) = s ++ "*"

showCa (t, a) = showCt t ++" "++ a

showH hc h@(Header t n args) = "foreign import ccall \""++hc++" "++n++"\" "++n++" :: "++ (concat$intersperse" -> "$map showHa args) ++" -> " ++ t'
    where t' | pure h = showHt t
             | otherwise = "IO("++showHt t++")"

showHt (Normal (s:ss)) = toUpper s : ss
showHt (Pointer "gsl_sf_result") = "Ptr Double"
showHt (Pointer "gsl_sf_result_e10") = "Ptr ()"
showHt (Pointer (s:ss)) = "Ptr "++toUpper s : ss

showHa (t,a) = showHt t

showFull hc h@(Header t n args) = "\n-- | wrapper for "++showC h++"\n"++ boiler h ++"\n" ++showH hc h 

fixmd1 = rep ("Gsl_mode_t","Precision")
fixmd2 = rep ("mode"," (precCode mode)")

boiler h@(Header t n args) | fst (last args) == Pointer "gsl_sf_result" = boilerResult h
                           | fst (last args) == Pointer "gsl_sf_result_e10" = boilerResultE10 h
                           | any isMode args = boilerMode h
                           | otherwise = boilerBasic h

isMode (Normal "gsl_mode_t",_) = True
isMode _ = False

hName n = f $ drop 7 n
    where f (s:ss) = toLower s : ss


boilerResult h@(Header t n args) =
    hName n++" :: "++ (fixmd1 $ concat $ intersperse" -> "$ map showHa (init args)) ++" -> " ++ "(Double,Double)\n" ++
     hName n ++ " "++ initArgs args ++
       " = createSFR \""++ hName n ++"\" $ " ++ n ++ " "++ (fixmd2 $ initArgs args)

boilerResultE10 h@(Header t n args) =
    hName n++" :: "++ (fixmd1 $ concat $ intersperse" -> "$ map showHa (init args)) ++" -> " ++ "(Double,Int,Double)\n" ++
     hName n ++ " "++ initArgs args ++
       " = createSFR_E10 \""++ hName n ++"\" $ " ++ n ++ " "++ (fixmd2 $ initArgs args)

boilerBasic h@(Header t n args) = 
    hName n++" :: "++ (fixmd1 $ concat $ intersperse" -> "$map showHa args) ++" -> " ++showHt t ++ "\n" ++
      hName n ++ " = " ++fixmd2 n

boilerMode h@(Header t n args) =
    hName n++" :: "++ (fixmd1 $ concat $ intersperse" -> "$ map showHa args) ++" -> " ++ showHt t++"\n" ++
     hName n ++ " "++ allArgs args ++
       " = " ++ n ++ " "++ (fixmd2 $ allArgs args)

cVar (v:vs) | isUpper v = toLower v : v : vs
            | otherwise = v:vs

allArgs args =  unwords (map (cVar.snd) args)
initArgs args = unwords (map (cVar.snd) (init args))