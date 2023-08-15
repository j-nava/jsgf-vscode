PROJECT=jsgf-vscode

all: build

build: 
	idris2 --codegen node --build jsgf-vscode.ipkg
	cat ./src/_exports.js >> ./build/exec/lib.js
	cp ./build/exec/lib.js ./src/
	npm run compile

clean:
	idris2 --clean jsgf-vscode.ipkg
	rm -rf ./build/

run: build
	node ./build/exec/extension.js

bundle: clean build
	npm run bundle
	npx vsce package --out ./build/

.PHONY: all build clean run bundle
