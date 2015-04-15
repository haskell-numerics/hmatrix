pkgs=base gsl special glpk tests ../../hTensor ../../easyVision/packages/tools ../../easyVision/packages/base

cabalcmd =                         \
      for p in $(1); do            \
	  if [ -e $$p ]; then          \
	    cd $$p; cabal $(2) ; cd -; \
	  fi;                          \
	done                           \
	cd sparse;                     \
	cabal install --extra-include-dirs=$(MKL) --extra-lib-dirs=$(MKL) $(2); cd -;

all:
	$(call cabalcmd, $(pkgs), install --force-reinstall --enable-documentation)

fast:
	$(call cabalcmd, $(pkgs), install --force-reinstall)

