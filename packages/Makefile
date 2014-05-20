pkgs=base hmatrix special glpk tests ../../hTensor

all:
	for p in $(pkgs); do \
	  if [ -e $$p ]; then \
	    cd $$p; cabal install --force-reinstall --enable-documentation ; cd -; \
	  fi; \
	done

