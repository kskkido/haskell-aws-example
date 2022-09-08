.PHONY: build
build:
	$(MAKE) build-static || exit
	$(MAKE) build-app || exit

.PHONY: build-app
build-app:
	stack build

.PHONY: build-static
build-static:
	yarn --cwd ./static build

.PHONY: generate
build-static-site:
	$(MAKE) build
	stack exec eriko-ikeda-portfolio-fp-cli-exe static-site-generator

.PHONY: lambda
build-lambda:
	DOCKER_BUILDKIT=1 docker build --file Lambda.Dockerfile -t generate-static-site-lambda . --build-arg EXECUTABLE=eriko-ikeda-portfolio-fp-lambda-exe
	id=$$(docker create generate-static-site-lambda); docker cp $$id:/root/output .; docker rm -v $$id
	cd output; cp -r ../resources .; cp -r ../static/public .; zip -r function.zip *

.PHONY: watch
watch:
	find ./src ./static/src -type f -name '*.hs' -o -name "*.purs" -o -name "*.css" | entr -r $(MAKE) start

