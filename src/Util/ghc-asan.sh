sh ghc-wrapper.sh                                                            \
    -optc="-g" -optl="-g"                                                    \
    -optc="-fsanitize=address"                                               \
    -optc="-fsanitize-coverage=edge,indirect-calls," \
    -optc="-fsanitize=fuzzer"                                        \
    -optl="-fsanitize=address"                                               \
    -optl="-fsanitize-coverage=edge,indirect-calls," \
    -optl="-fsanitize=fuzzer"                                        \
    $*
