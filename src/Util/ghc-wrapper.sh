stack ghc --       \
    -threaded      \
    -dynamic       \
    -fforce-recomp \
    -fllvm         \
    -pgmP=clang    \
    -pgmc=clang    \
    -pgma=clang    \
    -pgml=clang++  \
    -pgml=clang++  \
    $*
