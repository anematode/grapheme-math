grep -e "import.*[\'\"];\?$" src -r | (grep --line-buffered -v "js") | cat

sed -i "" '/js/! s/import\(.*\)['\''"]\(.*\)['\''"]/import\1"\2\.js"/g' -r
