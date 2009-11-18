#! /usr/bin/env runhaskell
{- configure.hs for hmatrix
   ------------------------

    GSL and LAPACK may require auxiliary libraries which depend on OS,
    distribution, and implementation. This script tries to to find out
    the correct link command for your system.
    Suggestions and contributions are welcome.

    By default we try to link -lgsl -llapack. This works in ubuntu/debian,
    both with and without ATLAS.
    If this fails we try different sets of additional libraries which are
    known to work in some systems.

    The desired libraries can also be explicitly given by the user using cabal
    flags (e.g., -fmkl, -faccelerate) or --configure-option=link:lib1,lib2,lib3,...

-}

import System
import Data.List(isPrefixOf, intercalate)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Configure
import Distribution.PackageDescription

-- possible additional dependencies for the desired libs (by default gsl lapack)

opts = [ ""                              -- Ubuntu/Debian
       , "blas"
       , "blas cblas"
       , "cblas"
       , "gslcblas"
       , "blas gslcblas"
       , "f77blas"
       , "f77blas cblas atlas gcc_s"     -- Arch Linux (older version of atlas-lapack)
       , "blas gslcblas gfortran"        -- Arch Linux with normal blas and lapack
       ]

-- compile a simple program with symbols from GSL and LAPACK with the given libs
testprog buildInfo libs fmks =
    "echo \"#include <gsl/gsl_sf_gamma.h>\nint main(){zgesvd_(); gsl_sf_gamma(5);}\""
                     ++" > /tmp/dummy.c; gcc "
                     ++ (join $ ccOptions buildInfo) ++ " "
                     ++ (join $ cppOptions buildInfo) ++ " "
                     ++ (join $ map ("-I"++) $ includeDirs buildInfo)
                     ++" /tmp/dummy.c -o /tmp/dummy "
                     ++ (join $ map ("-L"++) $ extraLibDirs buildInfo) ++ " "
                     ++ (prepend "-l" $ libs) ++ " "
                     ++ (prepend "-framework " fmks) ++ " > /dev/null 2> /dev/null"

join = intercalate " "
prepend x = unwords . map (x++) . words

check buildInfo libs fmks = (ExitSuccess ==) `fmap` system (testprog buildInfo libs fmks)

-- simple test for GSL
gsl buildInfo = "echo \"#include <gsl/gsl_sf_gamma.h>\nint main(){gsl_sf_gamma(5);}\""
           ++" > /tmp/dummy.c; gcc "
           ++ (join $ ccOptions buildInfo) ++ " "
           ++ (join $ cppOptions buildInfo) ++ " "
           ++ (join $ map ("-I"++) $ includeDirs buildInfo)
           ++ " /tmp/dummy.c -o /tmp/dummy "
           ++ (join $ map ("-L"++) $ extraLibDirs buildInfo) ++ " -lgsl -lgslcblas"
           ++ " > /dev/null 2> /dev/null"

-- test for gsl >= 1.12
gsl112 buildInfo =
    "echo \"#include <gsl/gsl_sf_exp.h>\nint main(){gsl_sf_exprel_n_CF_e(1,1,0);}\""
           ++" > /tmp/dummy.c; gcc /tmp/dummy.c "
           ++ (join $ ccOptions buildInfo) ++ " "
           ++ (join $ cppOptions buildInfo) ++ " "
           ++ (join $ map ("-I"++) $ includeDirs buildInfo)
           ++" -o /tmp/dummy "
           ++ (join $ map ("-L"++) $ extraLibDirs buildInfo) ++ " -lgsl -lgslcblas"
           ++ " > /dev/null 2> /dev/null"


checkCommand c = (ExitSuccess ==) `fmap` system c

-- test different configurations until the first one works
try _ _ _ [] = return Nothing
try i b f (opt:rest) = do
    ok <- check i (b ++ " " ++ opt) f
    if ok then return (Just opt)
          else try i b f rest

-- read --configure-option=link:lib1,lib2,lib3,etc
linkop = "link:"
getUserLink = concatMap (g . drop (length linkop)) . filter (isPrefixOf linkop)
    where g = map cs
          cs ',' = ' '
          cs x   = x

main = do
    putStr "Checking foreign libraries..."

    args <- getArgs
    Just bInfo <- maybeGetPersistBuildConfig "dist"

    let Just lib = library . localPkgDescr $ bInfo
        buildInfo = libBuildInfo lib
        base = unwords . extraLibs $ buildInfo
        fwks = unwords . frameworks $ buildInfo
        auxpref = getUserLink args

    -- We extract the desired libs from hmatrix.cabal (using a cabal flags)
    -- and from a posible --configure-option=link:lib1,lib2,lib3
    -- by default the desired libs are gsl lapack.

    let pref = if null (words (base ++ " " ++ auxpref)) then "gsl lapack" else auxpref
        fullOpts = map ((pref++" ")++) opts

    r <- try buildInfo base fwks fullOpts
    case r of
        Nothing -> do
            putStrLn " FAIL"
            g  <- checkCommand $ gsl buildInfo
            if g
                then putStrLn " *** Sorry, I can't link LAPACK."
                else putStrLn " *** Sorry, I can't link GSL."
            putStrLn " *** Please make sure that the appropriate -dev packages are installed."
            putStrLn " *** You can also specify the required libraries using"
            putStrLn " *** cabal install hmatrix --configure-option=link:lib1,lib2,lib3,etc."
            writeFile "hmatrix.buildinfo" ("buildable: False\n")
        Just ops -> do
            putStrLn " OK"
            g <- checkCommand $ gsl112 buildInfo
            writeFile "hmatrix.buildinfo" $ "extra-libraries: " ++
                ops ++ "\n" ++
                if g
                    then ""
                    else "cc-options: -DGSL110\n"
