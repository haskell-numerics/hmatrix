# Sundials installation

You can download the Sundials source files [here](https://computation.llnl.gov/projects/sundials/sundials-software). Note: `hmatrix-sundials` doesn't work with versions superior to 3.2.1 (4.0.0 and above).

Assuming we choose the 3.2.1 version:

    $ wget https://computation.llnl.gov/projects/sundials/download/sundials-3.2.1.tar.gz
    $ tar -xzf sundials-3.2.1.tar.gz
    $ cd sundials-3.2.1
    $ mkdir instdir
    $ mkdir builddir
    $ cd builddir
    $ cmake -DCMAKE_INSTALL_PREFIX=/absolute/path/to/sundials-3.2.1/instdir -DEXAMPLES_INSTALL_PATH=/absolute/path/to/sundials-3.2.1/instdir/examples ../../sundials-3.2.1
    $ make
    $ make install


Then for an installation in a project, one should add to `stack.yaml`:
```
extra-include-dirs:
- /absolute/path/to/instdir/sundials-3.2.1/include
extra-lib-dirs:
- /absolute/path/to/instdir/sundials-3.2.1/builddir/src/cvode
- /absolute/path/to/instdir/sundials-3.2.1/builddir/src/arkode
```

Or in the global scope: `stack install hmatrix-sundials --extra-lib-dirs=/absolute/path/to/instdir/sundials-3.2.1/builddir/src/arkode --extra-lib-dirs=//absolute/path/to/instdir/sundials-3.2.1/builddir/src/cvode --extra-include-dirs=/absolute/path/to/instdir/sundials-3.2.1/include`

