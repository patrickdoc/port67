site:
		build-site

slick:
		cabal install

deploy:
		serve docs

clean:
		rm -rf docs
		rm -rf .shake

publish: slick clean | site
		git checkout docs/CNAME
		git add docs
		git commit -m "Publishing"
		git push

.PHONY: slick site deploy publish
