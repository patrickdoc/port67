# Use Pandoc to build the markdown into a website
#
# Author: Patrick Dougherty
# Date: July 19, 2017
#
# Use "make" or "make html" to build the website
#
# Use "clean" to remove all html

# Pandoc
PANDOC=/usr/bin/pandoc
PANDOC_OPTIONS=--smart
PANDOC_HTML_OPTIONS=--to html5 --toc --template=template.html

# Files
SECTIONS = Section1 Section2
SRC := $(foreach sec, $(SECTIONS), $(wildcard $(sec)/*.md) $(wildcard $(sec)/Fun/*.md)) $(wildcard *.md)
DST = $(SRC:%.md=docs/%.html)


.PHONY: html clean show index readme support

html: $(DST) index readme support
	cp -r css docs/

index: Top/index.md template.html
	@mkdir -p docs
	$(PANDOC) $(PANDOC_OPTIONS) $(PANDOC_HTML_OPTIONS) -o docs/index.html $<

readme: Top/README.md template.html
	@mkdir -p docs
	$(PANDOC) $(PANDOC_OPTIONS) $(PANDOC_HTML_OPTIONS) -o docs/README.html $<

support: Top/support.md template.html
	@mkdir -p docs
	$(PANDOC) $(PANDOC_OPTIONS) $(PANDOC_HTML_OPTIONS) -o docs/support.html $<

# Pattern matching
docs/%.html: %.md template.html
	@mkdir -p $(@D)
	$(PANDOC) $(PANDOC_OPTIONS) $(PANDOC_HTML_OPTIONS) -o $@ $<

clean:
	rm -r docs

# Debugging
show:
	@echo '$(VALUE)="$($(VALUE))"'
