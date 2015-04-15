pkgs=base gsl special glpk tests ../../hTensor ../../easyVision/packages/tools ../../easyVision/packages/base

mkl=--extra-include-dirs=$(MKL) --extra-lib-dirs=$(MKL)

cabalcmd =                         \
    for p in $(1); do              \
	  if [ -e $$p ]; then          \
	    cd $$p; cabal $(2) ; cd -; \
	  fi;                          \
	done;                          \
	cd sparse;                     \
	cabal $(3) $(2); cd -;


all:
	$(call cabalcmd, $(pkgs), install --force-reinstall --enable-documentation, $(mkl))

fast:
	$(call cabalcmd, $(pkgs), install --force-reinstall, $(mkl))

clean:
	$(call cabalcmd, $(pkgs), clean)

prof:
	$(call cabalcmd, $(pkgs), install --force-reinstall --enable-library-profiling, $(mkl))

