
# BUILD_FLAGS=--pedantic --library-profiling --executable-profiling
BUILD_FLAGS=--pedantic

RUN=stack exec -- dialogue

run: build
	$(RUN) journal --help

init: build
	$(RUN) init

journal: build
	$(RUN) journal --help

docs:
	stack haddock
	open `stack path --local-doc-root`/index.html

# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# deploy:
# prep and push

configure:
	cabal configure \
		--package-db=clear \
		--package-db=global \
		--package-db=`stack path --snapshot-pkg-db` \
		--package-db=`stack path --local-pkg-db`

install:
	stack install

tags: $(SRC)
	codex update

hlint:
	hlint *.hs src specs

clean:
	stack clean
	codex cache clean

distclean: clean
	rm stack.yaml

build:
	stack build $(BUILD_FLAGS)

test:
	stack test $(BUILD_FLAGS)

bench:
	stack bench $(BUILD_FLAGS)

watch:
	stack build --file-watch --pedantic --fast --exec 'make run'

watch-test:
	stack test --file-watch --pedantic # --test-arguments "-m TODO"

restart: distclean build

rebuild: clean build

.PHONY: run docs configure install hlint clean distclean build test
	bench watch watch-test restart rebuild
	init journal
