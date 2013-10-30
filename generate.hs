import Control.Monad
import System.Directory
import System.IO
import System.FilePath

scripts :: [(FilePath, String, String)]
scripts = [
          ("pull", "Pulling", "git fetch && git pull origin $(git rev-parse --symbolic-full-name --abbrev-ref HEAD)"),
          ("stat", "Status", "git status" ++ bashPause),
          ("branch", "Branches", "git branch" ++ bashPause),
          ("prune", "Pruning", "git fetch && git branch --merged | grep -v '\\*' | xargs -n 1 git branch -d")
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
\run_command() {\n\
\  if [ -d $1 ]\n\
\  then\n\
\    cd $1\n\
\    if [ -d '.git' ]\n\
\    then\n\
\      M='" ++ message ++ "' \n\
\      echo ${M-''} `pwd` `git branch 2>/dev/null | grep -e '^*' | sed -E 's/^\\* (.+)$/(\\1) /'` \n\
\      " ++ command ++ " \n\
\    fi\n\
\    cd - 1>/dev/null\n\
\  fi\n\
\}\n\n\
\while getopts ':a' opt; do\n\
\  case $opt in\n\
\    a)\n\
\      shift\n\
\      cd ${1-'.'}\n\
\      for entry in *; do \n\
\        run_command $PWD'/'$entry\n\
\      done\n\
\      exit 0\n\
\      ;;\n\
\  esac\n\
\done\n\n\
\run_command ${1-'.'}"
