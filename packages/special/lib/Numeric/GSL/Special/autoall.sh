#!/bin/bash

function rep {
    ./replace.hs "$1" "$2" < $3 > /tmp/tmp-rep
    cp /tmp/tmp-rep $3
}

rm -f funs.txt

./auto.hs airy
rep ') where' ', Precision(..)\n) where' Airy.hs
./auto.hs bessel
./auto.hs clausen
./auto.hs coulomb
./auto.hs coupling
rep ', coupling_6j_INCORRECT_e\n, coupling_6j_INCORRECT\n' '' Coupling.hs
./auto.hs dawson
./auto.hs debye
./auto.hs dilog
./auto.hs elementary
./auto.hs ellint
#./auto.hs elljac
./auto.hs erf
./auto.hs exp
rep ', exp\n' ', Numeric.GSL.Special.Exp.exp\n' Exp.hs
rep ', exprel_n_CF_e' '-- , exprel_n_CF_e' Exp.hs
./auto.hs expint
./auto.hs fermi_dirac
./auto.hs gamma
./auto.hs gegenbauer
./auto.hs hyperg
./auto.hs laguerre
./auto.hs lambert
./auto.hs legendre
./auto.hs log
rep ', log\n' ', Numeric.GSL.Special.Log.log\n' Log.hs
#./auto.hs mathieu
./auto.hs pow_int
./auto.hs psi
./auto.hs synchrotron
./auto.hs transport
./auto.hs trig
rep ', sin\n' ', Numeric.GSL.Special.Trig.sin\n' Trig.hs
rep ', cos\n' ', Numeric.GSL.Special.Trig.cos\n' Trig.hs
./auto.hs zeta
