grep -e "import.*[\'\"];\?$" src -r | (grep --line-buffered -v "js") | cat

find src -type f -name '*.ts' -exec sed -i "" -E '/js/! s/import(.*)['\''"](.*)['\''"];?$/import\1"\2\.js"/g' {} +
