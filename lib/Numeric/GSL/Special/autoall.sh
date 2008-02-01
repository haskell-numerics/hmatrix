#!/bin/bash

function rep {
    ./replace.hs "$1" "$2" < $3 > /tmp/tmp-rep
    cp /tmp/tmp-rep $3
}

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

./auto.hs airy
rep ') where' ', Precision(..)\n) where' Airy.hs
./auto.hs bessel
./auto.hs clausen
./auto.hs coulomb
runhaskell auto coupling
rep ', coupling_6j_INCORRECT_e\n, coupling_6j_INCORRECT\n' '' Coupling.hs
./auto.hs dawson
./auto.hs debye
./auto.hs dilog
./auto.hs elementary
./auto.hs ellint
./auto.hs erf
#runhaskell auto exp
./auto.hs expint
./auto.hs fermi_dirac
./auto.hs gamma
./auto.hs gegenbauer
./auto.hs hyperg
./auto.hs laguerre
./auto.hs lambert
#runhaskell auto legendre legendre.h
#runhaskell auto log
./auto.hs pow_int
./auto.hs psi
./auto.hs synchrotron
#runhaskell auto trig
./auto.hs zeta