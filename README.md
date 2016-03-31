# How to start Riskell #

1) Get Stack running on your pc

2) Get the dependencies (do it in two ways - just to be sure)
> stack build
> stack exec --no-ghc-package-path -- cabal update
> stack exec --no-ghc-package-path -- cabal install

2) Run Riskell with cabal
> stack exec --no-ghc-package-path -- cabal run (might work on second try)

3) Connect to localhost:3000

4) Enjoy the game...
