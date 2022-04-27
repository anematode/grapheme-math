wat2wasm test.wat

cp _wasm_instantiate.ts wasm_instantiate.ts

b64encode=$(base64 test.wasm)
b64encode=$(echo ${b64encode//\//\\/})
replace=$(echo "s/\$WASM_CONTENTS/$b64encode/g")

echo $replace
sed -i '' -e "$replace" wasm_instantiate.ts
