import Control.Monad
import System.Directory
import System.IO
import System.FilePath
import Unlinesv

data Script = Script { help :: String
                     , name :: String
                     , label :: String
                     , body :: String
                     , pause :: Bool
                     }
bin :: FilePath
bin = "bin"

scripts = [
          Script {
            help  = "Fetches and then pulls HEAD from origin.",
            name  = "pu",
            label = "Pulling",
            body  = "git fetch && git pull origin $(git rev-parse --symbolic-full-name --abbrev-ref HEAD)",
            pause = False
          },
          Script {
            help  = "Prints repository status.",
            name  = "st",
            label = "Status",
            body  = "git status",
            pause = True
          },
          Script {
            help  = "Prints local branches.",
            name  = "br",
            label = "Branches",
            body  = "git branch",
            pause = True
          },
          Script {
            help  = "Deletes the local branches that have been merged.",
            name  = "pr",
            label = "Pruning",
            body  = "git fetch && git branch --merged | grep -v '\\*' | xargs -n 1 git branch -d",
            pause = False
          }
          ]

main = do
  createDirectoryIfMissing True bin
  mapM_ write scripts

write s = do
  let p = path s
  writeFile p $ render s
  permissions <- getPermissions p
  setPermissions p $ permissions {executable = True}

path :: Script -> FilePath
path s = combine bin $ name s

render :: Script -> String
render s = unlinesv
  "#!/bin/bash"
  ""
  "run_command() {"
  "  if test -d $1 && cd $1"
  "  then"
  "    if [ -d '.git' ]"
  "    then"
 ("      M='" ++ label s ++ "'")
  "      echo ${M-''} `pwd` `git branch 2>/dev/null | grep -e '^*' | sed -E 's/^\\* (.+)$/(\\1) /'`"
 ("      " ++ body s)
  "      cd - 1>/dev/null"
  "    else"
  "      cd - 1>/dev/null"
  "      false"
  "    fi"
  "  else"
  "    false"
  "  fi"
  "}"
  ""
  "ALL=false"
  "DIR='.'"
  ""
  "for i in \"$@\""
  "do"
  "case $i in"
  "  -a|--all)"
  "    ALL=true"
  "    ;;"
  "  -h|--help)"
 ("    echo '" ++ help s ++ "'")
  "    exit 0"
  "    ;;"
  "  *)"
  "    if [ -d $i ]"
  "    then"
  "      DIR=$i"
  "    else"
  "      echo \"unknown directory: $i\" 1>&2"
  "      exit 1"
  "    fi"
  "    ;;"
  "esac"
  "done"
  ""
  "if $ALL"
  "then"
  "  cd $DIR"
  "  for ENTRY in *; do"
 ("    run_command $PWD'/'$ENTRY " ++ if pause s then "&& read -p '...' && echo ''" else "# no pause")
  "  done"
  "else"
  "  run_command $DIR"
  "fi"
