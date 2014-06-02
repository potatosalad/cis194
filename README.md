# cis194

Homework solutions, tests, and notes from the University of Pennsylvania's [CIS 194](http://www.seas.upenn.edu/~cis194/lectures.html) class.

Heavily influenced by [coopernurse/cis194](https://github.com/coopernurse/cis194) (tests and original project structure in particular).

Course information originally found thank to [bitemyapp/learnhaskell](https://github.com/bitemyapp/learnhaskell).

## Setup

 * Install GHC according to [learnhaskell: Getting Started](https://github.com/bitemyapp/learnhaskell#getting-started).
 * Install [cabal-install](http://www.haskell.org/cabal/download.html)

```bash
cabal install hspec
cabal configure --enable-test
cabal build
cabal test
```
