import Control.Monad
import System.Directory
import System.IO
import System.FilePath

scripts :: [(FilePath, String, String)]
scripts = [
          ("pullall", "Pulling", "git fetch && git pull origin $(git rev-parse --symbolic-full-name --abbrev-ref HEAD)"),
          ("statall", "Status", "git status" ++ bashPause),
          ("branchall", "Branches", "git branch" ++ bashPause),
          ("pruneall", "Pruning", "git fetch && git branch --merged | grep -v '\\*' | xargs -n 1 git branch -d")
          ]

bashPause :: String
bashPause = " && read -p '...'; echo ''"

main = do
  createDirectoryIfMissing True "bin"
  mapM_ writeScript scripts

writeScript :: (FilePath, String, String) -> IO ()
writeScript (name, message, command) = do
  let path = combine "bin" name
  writeFile path (script message command)
  p <- getPermissions path
  setPermissions path (p {executable = True})

script :: String -> String -> String
script message command =  "#!/bin/bash \n\n\
\cd ${1-'.'} \n\n\
\for file in * \n\
\do \n\
\  if [ -d $file ] \n\
\  then \n\
\    cd $file \n\
\    if [ -d '.git' ] \n\
\    then \n\
\      M='" ++ message ++ "' \n\
\      echo ${M-''} `pwd` `git branch 2>/dev/null | grep -e '^*' | sed -E 's/^\\* (.+)$/(\\1) /'` \n\
\      " ++ command ++ " \n\
\    fi \n\
\    cd ../ \n\
\  fi \n\
\done"
