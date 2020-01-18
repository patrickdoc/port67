site:
		build-site

slick:
		cabal install

serve:
		serve docs

clean:
		rm -rf docs
		rm -rf .shake

full: slick clean | site
		git checkout docs/CNAME

publish: full
		git add docs
		git commit -m "Publishing posts"
		git push

.PHONY: slick site serve publish
