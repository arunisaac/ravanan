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
$(guix build -L ../.guix -f ../.guix/e2e-tests.scm)
```

## Run specific end-to-end test

When hacking on ravanan, you may be trying to get a specific test to pass, and may want to repeatedly run that specific test alone. You can do this by passing additional cwltest arguments. For example, to only run the `hello-world` test:
```
$(guix build -L ../.guix -f ../.guix/e2e-tests.scm) -s hello-world
```

# Run the CWL v1.2 conformance test suite

The CWL v1.2 conformance test suite is run similar to the end-to-end tests. Create and change into a new empty directory.
```
mkdir rundir
cd rundir
```
Then, build and run the tests.
```
$(guix build -L ../.guix -f ../.guix/cwl-conformance.scm)
```
To run a specific test alone (say, the `wf_simple` test):
```
$(guix build -L ../.guix -f ../.guix/cwl-conformance.scm) -s wf_simple
```
