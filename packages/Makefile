pkgs=base gsl special glpk tests ../../hTensor ../../easyVision/packages/base

all:
	for p in $(pkgs); do \
	  if [ -e $$p ]; then \
	    cd $$p; cabal install --force-reinstall --enable-documentation ; cd -; \
	  fi; \
	done
	cd sparse; \
	cabal install --extra-include-dirs=$(MKL) --extra-lib-dirs=$(MKL) \
                  --force-reinstall --enable-documentation ; cd -;

fast:
	for p in $(pkgs); do \
	  if [ -e $$p ]; then \
	    cd $$p; cabal install --force-reinstall ; cd -; \
	  fi; \
	done
	cd sparse; \
	cabal install --extra-include-dirs=$(MKL) --extra-lib-dirs=$(MKL) \
                  --force-reinstall; cd -;

