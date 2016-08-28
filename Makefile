
# BUILD_FLAGS=--pedantic --library-profiling --executable-profiling
BUILD_FLAGS=--pedantic

CONFIG=.creds.json
RUN=stack exec -- dime

USER=

run: build
	$(RUN) --help

init: stack.yaml

stack.yaml:
	stack init --prefer-nightly

docs:
	stack haddock
	open `stack path --local-doc-root`/index.html

login: build login-twitter login-gmail

login-twitter:
	$(RUN) twitter-login --config=$(CONFIG)

login-gmail:
	$(RUN) gmail-login --config=$(CONFIG)

dms.json: build
	$(RUN) dms --config=$(CONFIG) $(USER) --output=$@

merged.json:
	$(RUN) merge --dir archive/ --output $@

gmail:
	$(RUN) gmail --config=$(CONFIG) --user-index user-index.json --input `make last-archive` &> gmail-3.out

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
	stack build $(BUILD_FLAGS) --file-watch --fast

watch-test:
	stack test --file-watch --pedantic # --test-arguments "-m TODO"

count-dms:
	cat `make last-archive` | aeson-pretty | grep '"text"' | wc -l

last-archive:
	@ls -1 archive/*.json | tail -1

restart: distclean init build

rebuild: clean build

.PHONY: init run docs configure install hlint clean distclean build test
.PHONY: bench watch watch-test restart rebuild login count-dms gmail
