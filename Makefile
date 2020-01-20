site:
		build-site --color

clean:
		rm -rf docs
		rm -rf .shake
		git checkout docs/CNAME

refresh:
		$(MAKE) clean
		$(MAKE) site

# Targets for the underlying generation code
lib:
		cabal install

test:
		cabal test

# Utils
serve:
		serve docs

# Official builds
full: lib clean
		$(MAKE) site

publish: full
		git add docs
		git commit -m "Publishing posts"
		git push

.PHONY: site clean refresh lib test serve full publish
