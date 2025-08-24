# Set up development environment

Drop into a development environment using `guix shell`. This shell includes additional packages required for development, and not simply the dependencies required to build ravanan.
```
guix shell -L .guix -Df manifest.scm
```

# Run end-to-end tests

ravanan comes with a suite of end-to-end tests under `e2e-tests`. End-to-end tests require a running Guix daemon. To run them, create and change into a new empty directory.
```
mkdir rundir
cd rundir
```
Then, build and run the tests.
```
$(guix build -L ../.guix -e '(@ (cwl-conformance) e2e-tests)')
```
