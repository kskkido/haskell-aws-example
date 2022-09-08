.PHONY: build
build:
	yarn
	yarn build

.PHONY: build-scripts
build-scripts:
	rm -rf ./output
	rm -rf ./public/scripts
	spago bundle-app --main Pages.Home.Main --to ./public/scripts/pages/home/index.js
	spago bundle-app --main Pages.Photographs.Main --to ./public/scripts/pages/photographs/index.js
	spago bundle-app --main Pages.Drawings.Main --to ./public/scripts/pages/drawings/index.js

.PHONY: build-styles
build-styles:
	yarn postcss ./src/styles/index.css -d ./public/styles

.PHONY: watch
watch:
	find ./src -type f -name '*.purs' -o -name "*.css" | entr -r $(MAKE) build

