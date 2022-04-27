grep -e "import.*[\'\"];\?$" src -r | (grep --line-buffered -v "js") | cat
