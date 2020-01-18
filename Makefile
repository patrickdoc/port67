site:
		build-site

slick:
		cabal install

deploy:
		serve docs

clean:
		rm -rf docs

publish: slick clean | site
		git checkout docs/CNAME
		git add docs
		git commit -m "Publishing"

.PHONY: slick site deploy publish
