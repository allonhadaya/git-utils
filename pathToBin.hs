import System.Directory

main = do
    p <- canonicalizePath "bin"
    putStr p
