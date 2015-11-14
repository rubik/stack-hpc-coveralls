<h1 align="center">
    <a href="https://github.com/rubik/stack-hpc-coverage">
        Stack HPC Coveralls (SHC)
    </a>
</h1>

<p align="center">
    <a href="https://travis-ci.org/rubik/stack-hpc-coveralls">
        <img alt="Tests"
             src="https://img.shields.io/travis/rubik/stack-hpc-coveralls.svg?style=flat-square">
    </a>
    <a href="https://coveralls.io/github/rubik/stack-hpc-coveralls">
        <img alt="Code coverage"
             src="https://img.shields.io/coveralls/rubik/stack-hpc-coveralls.svg?style=flat-square">
    </a>
    <a href="https://github.com/rubik/stack-hpc-coveralls/blob/master/LICENSE">
        <img alt="License"
             src="https://img.shields.io/badge/license-ISC-blue.svg?style=flat-square">
    </a>
    <a href="https://hackage.haskell.org/package/stack-hpc-coveralls">
        <img alt="Version"
             src="https://img.shields.io/hackage/v/stack-hpc-coveralls.svg?label=version&amp;style=flat-square">
    </a>
</p>

<p align="center">
    Coveralls.io integration for Stack-based Haskell projects.
</p>

<hr>

### Installing

You can install it with ``stack install stack-hpc-coveralls``, but normally you
would use SHC on a continuous integration system such as Travis. In that case,
it's best to just download the right precompiled binary in the [Releases
section](https://github.com/rubik/stack-hpc-coveralls/releases) on Github, in
order to avoid all the compilation hassle.

**IMPORTANT NOTICE**: SHC needs at least Stack 0.1.7, which at the time of
writing is still unreleased and in development.

### Running

SHC should work out of the box in any Stack-based project. It may work in other
scenarios if you supply the right options, but that's not guaranteed, nor
supported. For plain Cabal projects, there is already
[hpc-coveralls](https://github.com/guillaume-nargeot/hpc-coveralls), of which
SHC is a spinoff.

Normally, for Stack projects, SHC just needs two things:

  - the name of your package
  - the name of the test suite that generated the coverage data

Just run the program from the root of you Stack project:

    $ shc packagename package-tests

Here is, for example, the Travis configuration for the
[Argon](https://github.com/rubik/argon) project:

```yaml
env:
  - GHCVER=7.8.4 STACK_YAML=stack-7.8.yaml
  - GHCVER=7.10.2 STACK_YAML=stack.yaml

# [...]

script:
  - stack test :argon-test --no-terminal --coverage

after_script:
  - travis_retry curl -L https://github.com/rubik/stack-hpc-coveralls/releases/download/v0.0.0.4/shc-linux-x64-$GHCVER.tar.bz2 | tar -xj
  - ./shc argon argon-test
```
