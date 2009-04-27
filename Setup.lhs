#! /usr/bin/env runhaskell

> import Distribution.Simple
> import System(system)

> main = defaultMainWithHooks autoconfUserHooks {runTests = t}

> t _ _ _ _ = system ( "runhaskell examples/tests.hs") >> return()
