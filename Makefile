PROJECT=jsgf-vscode

all: build

build: 
	idris2 --codegen node --build jsgf-vscode.ipkg
	npm run compile
	cat ./src/_exports.js >> ./build/exec/extension.js

clean:
	idris2 --clean jsgf-vscode.ipkg
	rm -rf ./build/

run: build
	node ./build/exec/extension.js

bundle: clean build
	npm run bundle
	npx vsce package --out ./build/

.PHONY: all build clean run bundle
