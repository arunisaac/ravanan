# Set up development environment

Drop into a development environment using `guix shell`. This shell includes additional packages required for development, and not simply the dependencies required to build ravanan.
```
guix shell -L .guix -Df manifest.scm
```
