<h1 align="center">
    <a href="https://github.com/rubik/argon">
        Stack HPC Coveralls (SHC)
    </a>
</h1>

<p align="center">
    <!--<a href="https://travis-ci.org/rubik/argon">-->
        <!--<img alt="Tests"-->
             <!--src="https://img.shields.io/travis/rubik/argon.svg?style=flat-square">-->
    <!--</a>-->
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

<p align="center">
    <img alt="Argon screenshot"
         src="https://cloud.githubusercontent.com/assets/238549/10644166/5a0f5efc-7827-11e5-9b29-6e7bcccb2345.png">
</p>

<hr>

### Installing

You can install it with ``stack install stack-hpc-coveralls``, but normally you
would use SHC on a continuous integration system such as Travis. In that case,
it's best to just download the right precompiled binary in the [Releases
section](https://github.com/rubik/stack-hpc-coveralls/releases) on Github, in
order to avoid all the compilation hassle.

### Running

SHC should work out of the box in any Stack-based project. It may work in other
scenarios if you supply the right options, but that's not guaranteed. With
plain Cabal, there is already
[hpc-coveralls](https://github.com/guillaume-nargeot/hpc-coveralls), of which
SHC is a spinoff.

Normally, for Stack projects, SHC just needs two things:

  - the name of your package
  - the name of the test suite that generated the coverage data

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
