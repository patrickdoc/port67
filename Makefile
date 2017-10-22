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


.PHONY: html clean show

html: $(DST)
	cp -r css docs/

# Pattern matching
docs/%.html: %.md template.html
	@mkdir -p $(@D)
	$(PANDOC) $(PANDOC_OPTIONS) $(PANDOC_HTML_OPTIONS) -o $@ $<

clean:
	rm -rf docs

# Debugging
show:
	@echo '$(VALUE)="$($(VALUE))"'
