import Test.DocTest

main = doctest
    [ "-isrc"
    , "src/Data/ConnectionString.hs"
    ]
