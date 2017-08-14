# Use Pandoc to build the markdown into a website
#
# Author: Patrick Dougherty
# Date: July 19, 2017
#
# Use "make" or "make html" to build the website
#
# Use "clean" to remove all html

# Pandoc
PANDOC=/usr/local/bin/pandoc
PANDOC_OPTIONS=--smart
PANDOC_HTML_OPTIONS=--to html5 --toc --template=template.html

# Files
SRC := $(wildcard */*.md)
DST = $(SRC:%.md=docs/html/%.html)


.PHONY: html clean show

html: $(DST)

# Pattern matching
docs/html/%.html: %.md template.html
	@mkdir -p $(@D)
	$(PANDOC) $(PANDOC_OPTIONS) $(PANDOC_HTML_OPTIONS) -o $@ $<

clean:
	rm -rf docs/html

# Debugging
show:
	@echo '$(VALUE)="$($(VALUE))"'
