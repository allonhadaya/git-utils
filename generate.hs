import Control.Monad
import System.Directory
import System.IO
import System.FilePath
import Unlinesv

scripts :: [(FilePath, String, String, Bool)]
scripts = [
          ("gpull", "Pulling", "git fetch && git pull origin $(git rev-parse --symbolic-full-name --abbrev-ref HEAD)", False),
          ("gstat", "Status", "git status", True),
          ("gbranch", "Branches", "git branch", True),
          ("gprune", "Pruning", "git fetch && git branch --merged | grep -v '\\*' | xargs -n 1 git branch -d", False)
          ]

main = do
  createDirectoryIfMissing True "bin"
  mapM_ writeScript scripts

writeScript :: (FilePath, String, String, Bool) -> IO ()
writeScript (name, message, command, pause) = do
  let path = combine "bin" name
  writeFile path (script message command pause)
  p <- getPermissions path
  setPermissions path (p {executable = True})

script :: String -> String -> Bool -> String
script message command pause = unlinesv
  "#!/bin/bash"
  ""
  "run_command() {"
  "  if [ -d $1 ]"
  "  then"
  "    cd $1"
  "    if [ -d '.git' ]"
  "    then"
 ("      M='" ++ message ++ "'")
  "      echo ${M-''} `pwd` `git branch 2>/dev/null | grep -e '^*' | sed -E 's/^\\* (.+)$/(\\1) /'`"
 ("      " ++ command)
  "    fi"
  "    cd - 1>/dev/null"
  "  fi"
  "}"
  ""
  "while getopts ':a' opt; do"
  "  case $opt in"
  "    a)"
  "      shift"
  "      cd ${1-'.'}"
  "      for entry in *; do"
  "        run_command $PWD'/'$entry"
 ("        " ++ if pause then "read-p '...'; echo ''" else "# no pause")
  "      done"
  "      exit 0"
  "      ;;"
  "  esac"
  "done"
  ""
  "run_command ${1-'.'}" 
