#!/bin/bash

#Airy.hs
#  include Precision (..) in the export list
#Exp.hs
#  remove extern inline definition, qualify name
#Coupling
#  remove deprecated INCORRECT
#Trig.hs
#  qualify names
#Legendre.hs
#  remove extern inline
#Log.hs
#  remove extern inline, qualify name

#runhaskell auto airy
runhaskell auto bessel
runhaskell auto clausen
runhaskell auto coulomb
#runhaskell auto coupling
runhaskell auto dawson
runhaskell auto debye
runhaskell auto dilog
runhaskell auto elementary
runhaskell auto ellint
runhaskell auto erf
#runhaskell auto exp
runhaskell auto expint
runhaskell auto fermi_dirac
runhaskell auto gamma
runhaskell auto gegenbauer
runhaskell auto hyperg
runhaskell auto laguerre
runhaskell auto lambert
#runhaskell auto legendre legendre.h
#runhaskell auto log
runhaskell auto pow_int
runhaskell auto psi
runhaskell auto synchrotron
#runhaskell auto trig
runhaskell auto zeta