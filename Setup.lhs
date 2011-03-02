#! /usr/bin/env runhaskell

> import Distribution.Simple
> import System.Process(system)
>
> import Config(config)
>
> import Distribution.Simple.Setup
> import Distribution.PackageDescription
> import Distribution.Simple.LocalBuildInfo
> import Distribution.Simple.Command
> import Distribution.PackageDescription.Parse
> import Distribution.Simple.Utils(info)
> import Distribution.Verbosity

> main = do
>    defaultMainWithHooks autoconfUserHooks {
>        runTests = t, 
>        postConf = modifiedPostConf }
>   where modifiedPostConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
>         modifiedPostConf args flags pkg_descr lbi
>              = do let verbosity = fromFlag (configVerbosity flags)
>                   noExtraFlags args
>
>                   config
>
>                   pbi <- getHookedBuildInfo verbosity
>                   let pkg_descr' = updatePackageDescription pbi pkg_descr
>                   postConf simpleUserHooks args flags pkg_descr' lbi


> getHookedBuildInfo :: Verbosity -> IO HookedBuildInfo
> getHookedBuildInfo verbosity = do
>   maybe_infoFile <- defaultHookedPackageDesc
>   case maybe_infoFile of
>     Nothing       -> return emptyHookedBuildInfo
>     Just infoFile -> do
>       info verbosity $ "Reading parameters from " ++ infoFile
>       readHookedBuildInfo verbosity infoFile


> t _ _ _ _ = system ( "runhaskell examples/tests.hs") >> return()

