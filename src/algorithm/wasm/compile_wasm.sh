wasm_file=${1:-impl.wat}

wat2wasm "$wasm_file" -o impl.wasm

b64encode=$(base64 impl.wasm)
b64encode=$(echo ${b64encode//\//\\/})
replace=$(echo "s/\/\* \$WASM_CONTENTS \*\/.*$""/\/\* \$WASM_CONTENTS \*\/\"$b64encode\"/g")

sed -i '' -e "$replace" wasm_wrapper.ts
echo "Compiled $wasm_file"
