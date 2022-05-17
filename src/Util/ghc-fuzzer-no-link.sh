sh ghc-wrapper.sh                                                            \
    -optc="-g" -optl="-g"                                                    \
    -optc="-fsanitize=address"                                               \
    -optc="-fsanitize-coverage=edge,indirect-calls," \
    -optc="-fsanitize=fuzzer-no-link"                                        \
    -optl="-fsanitize=address"                                               \
    -optl="-fsanitize-coverage=edge,indirect-calls," \
    -optl="-fsanitize=fuzzer-no-link"                                        \
    $*
